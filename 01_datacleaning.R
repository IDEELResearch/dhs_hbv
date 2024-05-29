# 01_datacleaning.R

# renv setup--------
#install.packages("renv")
renv::init()

# Load packages---------
library(tidyverse)
library(foreign)
library(here)
library(srvyr)
library(survey)

#Codebooks info------
# The files used in this analyses were CDKR61FL.DO (kids recode file) and CDPR61FL.DO (household member recode file), as well as hepatitis B surface antigen results (which have been provided to the DHS Program)
# After making an account and submitting a request for data to the DHS Program, download the CDKR61FL and CDPR61FL files.
# The Stata DO files were downloaded to use as codebooks to obtain the variable name/response options. These can be opened in a text editor (DO files are executable Stata code files) to view the variable labels and values.

#Children data----------
# CDOB61FL <- read.dta(here("Data", "CDOB61FL", "POLIO RESULTS CDC 12MAY2018.DTA"), convert.factors = FALSE) # other data not used in this analysis
cdpr61dt <- read.dta(here("Data","CDPR61DT", "CDPR61FL.DTA"), convert.factors = FALSE)
dhsmeta <- cdpr61dt # R data name used below

# In addition to the hepB outcome variable, Plasmodium falciparum PCR results (variable pfldh_kids) have been previously provided to the DHS Program.

#kids recode file with additional questions for a subset of children
d_k_or <- read.dta(here("Data","CDKR61DT","CDKR61FL.DTA"), convert.factors = FALSE)

# create unique ID for households (need to combine cluster and hh number)
dhsmeta$cluster_hh <- paste(dhsmeta$hv001, dhsmeta$hv002,sep = "_")
dhsmeta$hv105 <- as.numeric(dhsmeta$hv105)

#ADD other dataframes used below---------------

#Eligibility - fig 1----------
# important variables for kids' sampling: hv027 (selected for male interview: 0=no, 1=yes); sh310a (consent for vaccine biospecimen read: 1=granted,2=refused)
# number of distinct household selected and not selected for male interview (these are also the households selected for the kids biospecimen collection)
dhsmeta %>% group_by(hv027) %>% summarize(count_distinct = n_distinct(cluster_hh))
# or can use cdpr61dt

# number of children 6-59 months in selected households
dhsmeta %>% filter(hv027==1 & (hc1 >= 6 & hc1<=59)) %>% count() 
# number of children 6-71 months (≤5 years) --ACTUAL subset; hc1 incomplete (filled for some not all - using complete age in years variable)
dhsmeta %>% filter(hv027==1 & hv105 < 6 & (hc1>=6 | is.nan(hc1))) %>% count() # if leave hc1>6 it also drops those with missing ages in months (but have valid years)

# gave consent for DBS collection (separate from consent for additional testing)
dhsmeta %>% filter(hv027==1 & (hc1 >= 6 & hc1<=59)) %>% group_by(sh310a) %>% count() %>% print(n=Inf)
dhsmeta %>% filter(hv027==1 & hv105 < 6 & (hc1>=6 | is.nan(hc1))) %>% group_by(sh310a) %>% count() %>% print(n=Inf)

# slept in hh last night (de facto resident)
dhsmeta %>% filter(hv027==1 &  (hc1 >= 6 & hc1<=59) & sh310a == 1) %>% group_by(hv103) %>% count() %>% print(n=Inf)
dhsmeta %>% filter(hv027==1 &  hv105 < 6 & (hc1>=6 | is.nan(hc1)) & sh310a == 1) %>% group_by(hv103) %>% count() %>% print(n=Inf)

# consent for additional testing - can't find variable specific to children

# subset data frame 
dhsmeta <- dhsmeta %>% mutate_at(c('hv027', 'hc1', 'sh310a', 'hv103'), as.numeric) 
# elig_kids....and subsequent datasets includes eligibility on consent and de factor (subpart on flowchart) whereas
# elig_kids_2 and subsequent datasets applies propensity score to top level of all children selected (representative of all children in households selected for survey)
elig_kids <- dhsmeta %>% filter(hv027==1 & (hc1 >= 6 & hc1<=59) & sh310a == 1 & hv103==1) # %>% select(cluster_hh, hv001, hv002, hvidx, kids_barcode, hv105, hv104, hc1, hv270, pfldh_kids,
                                                                                          #           hv005, hv006, hv024, shnprovin, hv025, hv040, hv201, hv106, hv246, hml1, hml10, hml20, hv027, hv028)
#weight up to full n=8547 with propensity score
elig_kids_2 <- dhsmeta %>% filter(hv027==1 & (hc1 >= 6 & hc1<=59)) #%>% select(cluster_hh, hv001, hv002, hvidx, kids_barcode, hv105, hv104, hc1, hv270, pfldh_kids,sh310a,hv103,
                                                                   #                                      hv005, hv006, hv024, shnprovin, hv025, hv040, hv201, hv106, hv246, hml1, hml10, hml20, hv027, hv028)

# if subsetting kids from cdpr61dt: elig_kids$kids_barcode <- tolower(elig_kids$sh312)

# add hbv results 
elig_kids_2 <- elig_kids_2 %>% mutate(validbarcode = case_when(
  grepl("9999", elig_kids_2$kids_barcode) ~ 0, ## 99993  Sample damaged/insufficient/not found in lab # 99994  Not present # 99995  Refused # 99996  Other
  grepl("\\?", elig_kids_2$kids_barcode) ~ 0,
  TRUE ~ 1
))
table(barcode = elig_kids_2$validbarcode, consent =elig_kids_2$sh310a, useNA = "always")

# propensity score code to address missingness-----------

# indicator variable for barcode analyzed or not, has pf result, pf+/pf-/no pf result - children 6-59mo
elig_kids_2 <- elig_kids_2 %>% mutate(bc_select = case_when( # exchange between elig_kids_2 (n=8547) and elig_kids (n=8182)
  kids_barcode %in% kid_hbv_kr_dis$kids_barcode ~ 1,
  TRUE ~ 0),
  haspfres = case_when(
    !is.na(pfldh_kids) ~ 1,
    is.na(pfldh_kids) ~ 0),
  pf_3way = case_when(
    haspfres==1 & pfldh_kids==1 ~ 2, # sample has a pf result and it's positive
    haspfres==1 & pfldh_kids==0 ~ 1, # sample has a pf result and it's negative
    haspfres==0  ~ 0, # sample does not have a pf result
  ) %>%  as.factor(),
  pf_posvsoth = case_when(
    haspfres==1 & pfldh_kids==1 ~ 1, # sample has a pf result and it's positive
    haspfres==1 & pfldh_kids==0 ~ 0, # sample has a pf result and it's negative or no pf result
    haspfres==0  ~ 0, # sample does not have a pf result
  ) %>%  as.factor(), 
  # new age in years based on months variable
  hv105_fromhc1 = case_when(
    hc1 < 12 ~ 0, # kids below 6mo = 0 years
    hc1 >= 12 & hc1 <24 ~ 1, #  kids 12-<24mo = 1 years
    hc1 >= 24 & hc1 <36 ~ 2, # kids 24=<36 = 2 years
    hc1 >= 36 & hc1 <48 ~ 3, # kids 36-<48 = 3 years
    hc1 >= 48 & hc1 <60 ~ 4, # kids 48-<60 = 4 years
    hc1 >= 60 ~ 5) %>% as.factor(), # kids >=60mo = 5years
  discr = case_when( # when months/years ages discrepant
    hv105_fromhc1 == hv105 ~ 1,
    hv105_fromhc1 != hv105 ~ 0),
  sh310a_nano = ifelse(is.nan(sh310a), 2,sh310a),
  reltoheadhh=factor(hv101, 
                     levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,98),
                     labels = c("Head", "Spouse","Son/daughter","Son/daughter-in-law","Grandchild","Parent","In-laws","Brother/sister","Co-spouse","Other","Adopted/in custody","Not related","Nephew/niece","Nephew/niece by marriage","Don't know")),
  reltoheadhh_simp = case_when(
    hv101 == "3" ~ "Child", # son/daughter
    hv101 == "5" ~ "Grandchild", # grandchild
    TRUE ~ "Other") # all others since so few counts in each
  )
# data checks
table(elig_kids_2$pf_posvsoth, useNA = "always")
table(elig_kids_2$hc1, useNA = "always")
table(elig_kids_2$hv105, useNA = "always")
table(elig_kids_2$sh310a, useNA = "always")
table(elig_kids_2$sh310a_nano, useNA = "always")
table(elig_kids_2$hv026, elig_kids_2$hv025,useNA = "always")
table(elig_kids_2$hv101, useNA = "always")

#propensity score with full barcodes/dataset
## vars of prop score: hv006 - month of interview; shnprovin - province (try hv024, the pre-2015 provinces, if rank deficient warning affects);
## hv026 - capital/city/town/countryside, more specific than hv025 - urban/rural; hv040 - cluster altitude in meters; hv104 - sex; hv201 - source of drinking water; hv106 - education (delete for kids?);
## hv246 - owns livestock; hml1 - mosquito nets; hml10 - insecticide treated net; hml20 - slept under net; hv270 - hh wealth; hv105 - age
## sh310a_nano consent for participation (nan as refused); + sh310a_nano
selectioninstudy <- glm(bc_select ~ shnprovin + hv026 + hv104 + hv270 + hv105_fromhc1 + pf_posvsoth +  hv103 + reltoheadhh_simp, #check removing: +hml10+hml20; removed: hv106 (education), hml1 (has net), hv201 (water)
                        data=elig_kids_2,
                        family=binomial("logit"))
summary(selectioninstudy$coefficients) #view

# denominator of weights
elig_kids_2$d <- predict(selectioninstudy, elig_kids_2, type = "response")
summary(elig_kids_2$d)
# numerator of weights
num_mod <- glm(bc_select ~ 1, 
               data = elig_kids_2, 
               family = "binomial"(link="logit"))

elig_kids_2$n <- predict(num_mod, data = elig_kids_2, type = "response")

#unstandardized and standardized

### for standardized 
p_exposure <- sum(elig_kids_2$bc_select) / nrow(elig_kids_2)
summary(elig_kids_2$n)

elig_kids_2 <- elig_kids_2 %>% 
  mutate(sw = ifelse(bc_select == 1, n/d, (1-n)/(1-d)), # from 722 code
         iptw_u = ifelse(bc_select==1, 1/d, 1/(1-d)),
         iptw_s = ifelse(bc_select==1, p_exposure/d, (1-p_exposure)/(1-d))
  )
summary(elig_kids_2$sw) # 722 standardized weights
summary(elig_kids_2$iptw_u) # unstandardized weights from Hillary
summary(elig_kids_2$iptw_s) # standardized weights from Hillary

#look at distribution of weights, by whether sample was tested for hbv
elig_kids_2 %>% 
  ggplot() + geom_histogram(aes(x=iptw_s)) + facet_wrap(~bc_select, nrow = 2)

# add hbv results onto this eligible kid df------
elig_kids_2_wres <- left_join(elig_kids_2, kid_hbv_kr_dis[,c("kids_barcode", grep("hbv", names(kid_hbv_kr_dis), value=T))], by = "kids_barcode")

elig_kids_2_wres %>% filter(validbarcode==1) %>%  group_by(hbvresult5) %>% count()

# of these, final df with hbv results (subset to those with results)
elig_kids_2_whbvres <- elig_kids_2_wres %>% filter(validbarcode==1 & !is.na(hbvresult5)) # make sure hbvresult5 has all results

# of these, final df without hbv results
nores2 <- elig_kids_2_wres %>% filter(is.na(hbvresult5) & validbarcode==1)

# hbv results that aren't in final df (df per jan2024 committee discussion about starting with SURVEYED not SAMPLES)
hbvres_notonfinaldf <- kid_hbv_kr_dis %>% filter(!(kids_barcode %in% elig_kids_whbvres$kids_barcode))
# check age
table(hbvres_notonfinaldf$hv105, useNA = "always") # MOST are 5yos (>59months) for the first new flow chart
table(hbvres_notonfinaldf$hv105_fromhc1, useNA = "always")

# look at the 2903 with exhausted sample
table(nores2$pfldh_kids, useNA = "always")
table(nores$pfldh_kids, useNA = "always")

# distinct households
elig_kids_2_whbvres %>% summarize(count_distinct = n_distinct(cluster_hh))

# elig_kids_whbvres_wt used below; since merge above no longer needed, change name here
elig_kids_whbvres_wt <- elig_kids_2_whbvres
nrow(elig_kids_whbvres_wt)
summary(elig_kids_whbvres_wt$iptw_s)
mean(elig_kids_whbvres_wt$iptw_s)

# add and check unstabilized weights 
test2 <- left_join(elig_kids_whbvres, elig_kids[,c("kids_barcode","iptw_s","iptw_u")], by ="kids_barcode")
summary(test2$iptw_u)
mean(test2$iptw_u)

test2$comb <- test2$iptw_u*((test2$hv028)/1000000)
test2$comb_s <- test2$iptw_s*((test2$hv028)/1000000)
mean(test2$comb)

designf <-svydesign(ids=test2$hv001, strata=test2$hv022 , weights=test2$comb_s,  data=test2)
designf_dhs2 <-as_survey_design(designf)
as.data.frame(survtable("pfldh_kids"))
# proportions are similar - proceed with stabilized

# combine household weights (for kid samples) and propensity score weights accounting for missingness
# where hh weight came from: kid_dhs_int$hh_weight <- as.numeric(kid_dhs_int$hv005)/1000000
elig_kids_whbvres_wt$hv028 <- as.numeric(elig_kids_whbvres_wt$hv028)
elig_kids_whbvres_wt$hv028_div <- elig_kids_whbvres_wt$hv028/1000000
elig_kids_whbvres_wt$hh_weight <- as.numeric(elig_kids_whbvres_wt$hv005)/1000000

summary(elig_kids_whbvres_wt$hv028_div)
summary(elig_kids_whbvres_wt$hh_weight)

elig_kids_whbvres_wt$both_wt_old <- (elig_kids_whbvres_wt$iptw_s)*(elig_kids_whbvres_wt$hh_weight)
elig_kids_whbvres_wt$both_wt_new <- (elig_kids_whbvres_wt$iptw_s)*(elig_kids_whbvres_wt$hv028_div)
summary(elig_kids_whbvres_wt$both_wt_old)
summary(elig_kids_whbvres_wt$both_wt_new)
#whether hv028 or hv005 is the household weight used, the summary of the weights is very similar, so results won't differ much
#hv028 is the weight for households selected for the male subsample (hv027), which are the households in which children were selected for DBS testing

#New variables---------------
##Province collapsed------
##Tetanus Ab indic---------
##Age variable - using age in months, create new var for age in years (given discrepancy with hv105)--------
##Stunting,weight,wasting-------

#get variables in correct class
elig_kids_whbvres_wt <- elig_kids_whbvres_wt %>% mutate_at(c('hv024', 'hc70', 'hc71', 'hc72', 'hc57', 'hv009'), as.numeric)

# leaving new variables as numeric and before analysis can convert to factor
elig_kids_whbvres_wt <- elig_kids_whbvres_wt %>% mutate(
# group provinces - kinshasa combined with kongo central/bandundu
  provgrp = case_when(
    hv024 == "1" | hv024 == "2" | hv024 == "3" ~ 1,  #kinshasa, kongo central, bandundu (driving distance)
    hv024 == "4" ~ 2,  # Equateur
    hv024 == "5" | hv024 == "6" ~ 3,  # kasais
    hv024 == "7" ~ 4,   # Katanga
    hv024 == "8" ~ 5,# orientale
    hv024 == "9" |  hv024 == "11"  ~ 6,# Kivus
    hv024 == "10" ~ 7),# maniema
# group provinces - kinshasa own group
  provgrp_kin = case_when(
    hv024 == "1"  ~ 1,  #kinshasa,  (capital)
    hv024 == "2" | hv024 == "3" ~ 2,  # kongo central, bandundu (driving distance)
    hv024 == "4" ~ 3,  # Equateur
    hv024 == "5" | hv024 == "6" ~ 4,  # kasais
    hv024 == "7" ~ 5,   # Katanga
    hv024 == "8" ~ 6,# orientale
    hv024 == "9" |  hv024 == "11"  ~ 7,# Kivus
    hv024 == "10" ~ 8), # maniema
provgrp_kin_l = factor(provgrp_kin,
    levels = c(1,2,3,4,5,6,7,8),
    labels = c("Kinshasa","Kongo central/Bandundu", "Equateur", "Kasai", "Katanga", "Orientale", "Kivu", "Maniema")),
# tetanus serology indicators
  shtetaindasno = case_when(
    shteta=="0" ~ 0,
    shteta=="1" ~ 1,
    shteta=="3" ~ 0) %>% as.factor(),
  shtetaindasyes = case_when(
    shteta=="0" ~ 0,
    shteta=="1" ~ 1,
    shteta=="3" ~ 1) %>% as.factor(),
# new age in years based on months variable
hv105_fromhc1 = case_when(
  hc1 < 12 ~ 0, # kids below 6mo = 0 years
  hc1 >= 12 & hc1 <24 ~ 1, #  kids 12-<24mo = 1 years
  hc1 >= 24 & hc1 <36 ~ 2, # kids 24=<36 = 2 years
  hc1 >= 36 & hc1 <48 ~ 3, # kids 36-<48 = 3 years
  hc1 >= 48 & hc1 <60 ~ 4, # kids 48-<60 = 4 years
  hc1 >= 60 ~ 5), # kids >=60mo = 5years
hv105_fromhc1_f = as.factor(hv105_fromhc1),
discr = case_when( # when months/years ages discrepant
  hv105_fromhc1 == hv105 ~ 1,
  hv105_fromhc1 != hv105 ~ 0),
# nutritional status from https://dhsprogram.com/data/Guide-to-DHS-Statistics/Nutritional_Status.htm
# stunting: height-for-age
  sevstunt = case_when(
    hc70 < -300 ~ 1, # severe stunting defined as ht/age <3 SDs; hc70 has been multipled by 100
    hc70 >= -300 & hc70 < 2000 ~ 0, # not severe; excludes the 9997 and up that are out of range
    hc70 >=2000 ~ 9, # set to 9 to include as separate category if desired
    is.nan(hc70) ~ NA_real_, # set NaNs to missing
    TRUE ~ NA_real_),
  modstunt = case_when(
    hc70 < -200 ~ 1, # mod-to-severe stunting defined as ht/age  <2 SDs; hc70 has been multipled by 100
    hc70 >= -200 & hc70 < 2000 ~ 0, # not moderate; excludes the 9997 and up that are out of range
    hc70 >=2000 ~ 9, # set to 9 to include as separate category if desired
    is.nan(hc70) ~ NA_real_, # set NaNs to missing
    TRUE ~ NA_real_),
  stunt = case_when(
    hc70 < -300 ~ 2, # severe stunting defined as ht/age  <3 SDs; hc70 has been multipled by 100
    hc70 >= -300 & hc70 < -200 ~ 1, # mod: between -3 and -2 SDs
    hc70 >= -200 & hc70 < 2000 ~ 0, # not moderate; excludes the 9997 and up that are out of range
    hc70 >=2000 ~ 9, # set to 9 to include as separate category if desired
    is.nan(hc70) ~ NA_real_, # set NaNs to missing
    TRUE ~ NA_real_),
# wasting: weight-for-height  
  sevwasting = case_when(
    hc72 < -300 ~ 1, # severe wasting defined as wt-for-ht <3 SDs; hc72 has been multipled by 100
    hc72 >= -300 & hc72 < 2000 ~ 0, # not severe; excludes the 9997 and up that are out of range
    hc72 >=2000 ~ 9, # set to 9 to include as separate category if desired
    is.nan(hc72) ~ NA_real_, # set NaNs to missing
    TRUE ~ NA_real_),
  modwasting = case_when(
    hc72 < -200 ~ 1, # mod wasting defined as wt-for-ht <2 SDs; hc72 has been multipled by 100
    hc72 >= -200 & hc72 < 2000 ~ 0, # not severe; excludes the 9997 and up that are out of range
    hc72 >=2000 ~ 9, # set to 9 to include as separate category if desired
    is.nan(hc72) ~ NA_real_, # set NaNs to missing
    TRUE ~ NA_real_),  
  wasting = case_when(
    hc72 < -300 ~ 2, # severe wasting defined as wt-for-ht <3 SDs; hc72 has been multipled by 100
    hc72 >= -300 & hc72 < -200 ~ 1, # mod wasting; 
    hc72 >= -200 & hc72 < 2000 ~ 0, # not severe; excludes the 9997 and up that are out of range
    hc72 >=2000 ~ 9, # set to 9 to include as separate category if desired
    is.nan(hc72) ~ NA_real_, # set NaNs to missing
    TRUE ~ NA_real_),  
# under and over weight: weight-for-age
  weightforage = case_when(
    hc71 < -300 ~ 2, # severely underweight
    hc71 >= -300 & hc71 < -200 ~ 1, # moderately underweight
    hc71 >= -200 & hc71 < 200 ~ 0, # not over/underweight
    hc71 >= 200 & hc71 < 2000 ~ 3, # overweight
    hc71 >=2000 ~ 9, # set to 9 to include as separate category if desired
    is.nan(hc71) ~ NA_real_, # set NaNs to missing
    TRUE ~ NA_real_),  
# anemia - reverse coding
  anemia = ifelse(!is.nan(hc57), 5 - hc57, NA_real_) # reverse 1-4 order so higher is more anemic
  )
# check new variables provgrp
elig_kids_whbvres_wt %>% group_by(shnprovin, hv024,provgrp, provgrp_kin) %>% count()
elig_kids_whbvres_wt %>% group_by(shteta, shtetaindasno,shtetaindasyes) %>% count()
elig_kids_whbvres_wt %>% group_by(hv105, hv105_fromhc1) %>% count()
elig_kids_whbvres_wt %>% group_by(sevstunt, modstunt, stunt) %>% count()
elig_kids_whbvres_wt %>% group_by(sevwasting, modwasting, wasting) %>% count()
elig_kids_whbvres_wt %>% group_by(hc71, weightforage) %>% count()
elig_kids_whbvres_wt %>% group_by(hc57, anemia) %>% count()

# convert certain variables to factors for analysis
elig_kids_whbvres_wt <- elig_kids_whbvres_wt %>% mutate_at(c('sevstunt', 'modstunt', 'stunt', 'sevwasting', 'modwasting', 'wasting', 'weightforage', 'anemia'), as.factor)

#Merge KR vars on------
# see 05_famtreesdhs.R for merge of biospecimen results, PR, and KR (asked to caretakers about children, able to merge ~75%)
# d_k_or imported from CDKR61FL.DTA at beginning

# make unique identifier using the line number of the person
d_k_or$clus_hh_ind <- paste(d_k_or$v001, d_k_or$v002, d_k_or$b16, sep = "_")
elig_kids_whbvres_wt$clus_hh_ind <-  paste(elig_kids_whbvres_wt$hv001, elig_kids_whbvres_wt$hv002, elig_kids_whbvres_wt$hvidx, sep = "_")

# per https://userforum.dhsprogram.com/index.php?t=msg&th=11867&goto=24903&S=Google, also need to account for b16 since neither PR nor KR is a subset of the other
# instructional video https://www.youtube.com/watch?v=SJkJmtgaqBc
addmargins(table(d_k_or$b16, useNA = "always"))
# b16 label variable b16      "Child's line number in household"
# from https://userforum.dhsprogram.com/index.php?t=msg&th=8499&start=0&S=Google
# instructions to use b16 not v003 as child line number in KR https://userforum.dhsprogram.com/index.php?t=msg&goto=14951&S=Google

# If you try to merge the KR file with the PR file, there are four reasons why you may appear to lose cases:
#  (1) Children in the KR file will not be in the PR file if they have died (b16=.)
# (2) Children in the KR file will not be in the PR file if they are not living with the mother (b16=0)
# (3) Children in the PR file will not be in the KR file if their mother has died (hv112=.)
# (4) Children in the PR file will not be in the KR file if they are not living with their mother (hv112=0)
# might be restricted to hv103 de facto cases (kid slept in hh last night)

# additional info: https://dhsprogram.com/data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm

##Subset--------
# variables of interest from KR (1000 vars so should reduce)
d_k_or_red <- d_k_or %>% select(c(clus_hh_ind,v001,v002,b16,midx,v006,v007,v008,v011,v012,v044, v136, v137, v138, v150, m15, m17, m18,
                                  bord, b0, b1, b2, b3, b8, b11, b12, b15, b16, seligdv, s1323, s1324, h3, h5, h7, h10, starts_with("h15"),
                                  v477, v478, v480, v501, v502, v503, v504, v505, v506, v507, v508, v525, v527, v528, v529, v530, v531, v532,
                                  v743f, starts_with("v744"),snprovin, v003, hw51, s1202, s1208, v034)) # s1323, s1324 not available for kids with dbs
# indicator for IDs that also are on KR dataframe
d_k_or_red$fromKR <- 1

##Merge step-----
elig_kids_whbvres_wt_kr <- left_join(elig_kids_whbvres_wt, d_k_or_red, by = "clus_hh_ind" )
##check conditions of those without KR data----------
elig_kids_whbvres_wt_kr %>% group_by(fromKR) %>% count() #659 no KR data
# mother living? hv111==1
elig_kids_whbvres_wt_kr %>% group_by(fromKR, hv111) %>% count()
# living with mother?  (hv112==0) - only those living with mother were surveyed for KR
elig_kids_whbvres_wt_kr %>% group_by(fromKR, hv112) %>% count() %>% print(n=Inf)
elig_kids_whbvres_wt_kr %>% filter(is.na(fromKR)) %>% group_by(hv111,hv112) %>% count()
elig_kids_whbvres_wt_kr %>% filter(is.na(fromKR) & hv111 == 1 & hv112 != 0 ) %>%  count()
# of 659 kids without KR data, 80 mother was deceased or na, 419 not living with mother, 160 unknown why?

##New vars------
###DPT vaccination: dpt1, dpt2, dpt3, dpt_count, dpt_doses---------
###injections--------
###beating of wife------

elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate(
#DPT vaccination (diptheria/pertussis/tetanus, which has been replaced with pentavalent that includes HBV)
  dpt1 = case_when(
    h3 == 0 ~ 0, # did not receive DPT1 (reported as no and not on vac card)
    h3 == 8 ~ 0, # assign don't know as didn't receive
    h3 > 0 & h3 < 8 ~ 1, #h3==1 is vacc date on card, 2 is reported by mother, 3 is vacc on card
    is.na(h3) ~ NA_real_), # these are the values to impute - need help on this
  dpt2 = case_when(
    h5 == 0 ~ 0, # did not receive DPT1 (reported as no and not on vac card)
    h5 == 8 ~ 0, # assign don't know as didn't receive
    h5 > 0 & h5 < 8 ~ 1, #h3==1 is vacc date on card, 2 is reported by mother, 3 is vacc on card
    is.na(h5) ~ NA_real_), # these are the values to impute - need help on this
  dpt3 = case_when(
    h7 == 0 ~ 0, # did not receive DPT1 (reported as no and not on vac card)
    h7 == 8 ~ 0, # assign don't know as didn't receive
    h7 > 0 & h7 < 8 ~ 1, #h3==1 is vacc date on card, 2 is reported by mother, 3 is vacc on card
    is.na(h7) ~ NA_real_), # these are the values to impute - need help on this
  dpt_count = dpt1 + dpt2 + dpt3,
  dpt_doses = case_when(
    dpt_count==3 ~ 2, # reported or noted as received for all
    dpt_count==1 | dpt_count==2  ~ 1, # received 1 or 2 doses
    dpt_count==0 ~ 0), # received none
  dpt_any = case_when(
    dpt_count == 0 ~ 0, # no doses
    dpt_count > 0 ~ 1), # any doses
#2.  injections: h15[x] vars - very few responses so skipping; using v477 (count of injections received)
  injec = case_when(
    v477==0 ~ 0, # no injections in last 12 mo
    v477 > 0 & v477 <13 ~ 1, # 1-12 injections 
    v477 >= 13  & v477 <25 ~ 2, # 13-24 injections 
    v477 >= 25 ~ 3), # >=25 injections
# 3. beating justified - any of the five questions answered yes/don't know (v744a=wife goes out without telling husband; v744b=wife neglects children; v744c=wife argues with husband; v744d=wife refuses sex with husband; v744e=wife burns food)
beat = case_when(
  v744a > 0 | v744b > 0 | v744c > 0 | v744d > 0 | v744e > 0 ~ 1, 
  v744a == 0 | v744b ==  0 | v744c == 0 | v744d == 0 | v744e == 0 ~ 0
))
elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate_at(c('v480', 'injec','beat'), as.factor)
# check new variables
elig_kids_whbvres_wt_kr %>% group_by(dpt1, dpt2, dpt3, dpt_count, dpt_doses) %>% count()
elig_kids_whbvres_wt_kr %>% group_by(v477, injec) %>% count()
elig_kids_whbvres_wt_kr %>% group_by(v744a, v744b, v744c, v744d, v744e, beat) %>% count() %>% print(n=Inf)
elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate(syringe = case_when(is.na(v480) ~ 0, !is.na(v480) ~ 1))

# labels : relationship to head of hh
elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% dplyr::mutate(
    reltoheadhh=factor(hv101, 
      levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,98),
      labels = c("Head", "Spouse","Son/daughter","Son/daughter-in-law","Grandchild","Parent","In-laws","Brother/sister","Co-spouse","Other","Adopted/in custody","Not related","Nephew/niece","Nephew/niece by marriage","Don't know")),
    reltoheadhh_simp = case_when(
      hv101 == "3" ~ "Child", # son/daughter
      hv101 == "5" ~ "Grandchild", # grandchild
      TRUE ~ "Other"), # all others since so few counts in each
    sex=factor(hv104, 
        levels = c(1, 2),
        labels = c("Male", "Female")),    
    prov2015=factor(shnprovin, 
        levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
        labels = c("Kinshasa", "Kwango","Kwilu","Mai-Ndombe","Kongo Central","Equateur","Mongala","Nord-Ubangi","Sud-Ubangi","Tshuapa","Kasai","Kasai-Central","Kasai-Oriental","Lomami","Sankuru","Haut-Katanga","Haut-Lomami","Lualaba","Tanganyka","Maniema","Nord-Kivu","Bas-Uele","Haut-Uele","Ituri","Tshopo","Sud-Kivu")
        ),
    location = factor(hv026,
        levels = c(0, 1, 2, 3),
        labels = c("Capital", "Small city", "Town", "Countryside")),
    urbanrural = factor(hv025,
        levels = c(1,2),
        labels = c("Urban", "Rural")),
    wealth = factor(hv270,
        levels = c(1,2,3,4,5),
        labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    pfmalaria = factor(pfldh_kids,
        levels = c(0,1),
        labels = c("Pf-negative", "Pf-positive")),
    tetab = factor(shteta,
        levels = c(0,1,3),
        labels = c("Nonreactive", "Reactive", "Indeterminate")),
    anemia_f = case_when(
      anemia == "1" ~ "Mild-to-none", 
      anemia == "2" ~ "Mild-to-none", 
      anemia == "3" ~ "Moderate-to-severe", 
      anemia ==  "4" ~ "Moderate-to-severe", 
        is.na(anemia) ~ "Not available"),
    modstunt_f = case_when(
        modstunt == 0 ~ "No stunting",
        modstunt == 1 ~ "Moderate-to-severe",
        modstunt == 9 ~ "Not available",
        is.na(modstunt) ~ "Not available"),
    dpt_doses_f = case_when(
      dpt_doses == 0 ~ "No doses received",
      dpt_doses == 1 ~ "Series incomplete",
      dpt_doses == 2 ~ "Series completed",
      is.na(dpt_doses) ~ "Not available"),
    dpt_any_f = factor(dpt_any,
                       levels = c(0,1),
                       labels = c("No doses", "Series initiated")),
    injec_f = case_when(
      injec == 0 ~ "None",
      injec == 1 ~ "1-12",
      injec == 2 ~ "13-24",
      injec == 3 ~ "25+",
      is.na(injec) ~ "Not available"),
    beat_f = case_when(
      beat == 0 ~ "Never justified",
      beat == 1 ~ "Justified",
      is.na(beat) ~ "Not available"))

# positives by household
poskidsdf <- elig_kids_whbvres_wt_kr %>% group_by(cluster_hh) %>% 
  summarize(poskids = sum(hbvresult5), 
            nkids = n(), 
            percpos = round(100*(poskids/nkids),2))
view(poskidsdf)
poskidsdf %>% filter(poskids >1) %>% reframe()
elig_kids_whbvres_wt_kr <- left_join(elig_kids_whbvres_wt_kr, poskidsdf[,c("cluster_hh","poskids", "nkids","percpos")], by ="cluster_hh")
elig_kids_whbvres_wt_kr %>% group_by(percpos, poskids) %>% count()

elig_kids_whbvres_wt_kr$hv014 <- as.numeric(elig_kids_whbvres_wt_kr$hv014)
elig_kids_whbvres_wt_kr$poskids <- as.factor(elig_kids_whbvres_wt_kr$poskids)

# export hbv results--------
kidshbv <- elig_kids_whbvres_wt_kr %>% select(c(hv001, hv002, kids_barcode, hbvresult5, hbvresult1, hbvresult2, hbvresult100))
write.csv(kidshbv, file = here("Data", "kidshbv.csv"))

