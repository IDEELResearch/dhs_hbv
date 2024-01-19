# 01_datacleaning_rev.R

# Load packages---------
library(tidyverse)
library(foreign)
#Codebooks info------
# from DHS website, download CDKR61FL.DO and CDPR61FL.DO (or the DO files for other datasets of interest). Open these in a text editor (DO files are executable Stata code files), and control-F search for the variable labels and values

#Children data----------
CDOB61FL <- read.dta("/Users/camillem/Documents/GitHub/dhs_hbv/Data/CDOB61FL/POLIO RESULTS CDC 12MAY2018.DTA", convert.factors = FALSE)
cdpr61dt <- read.dta("/Users/camillem/Documents/GitHub/dhs_hbv/Data/CDPR61DT/CDPR61FL.DTA", convert.factors = FALSE)

#kids recode file with additional questions for a subset of children
d_k_or <- read.dta("/Users/camillem/Documents/GitHub/dhs_hbv/Data/CDKR61DT/CDKR61FL.DTA", convert.factors = FALSE)

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
elig_kids <- dhsmeta %>% filter(hv027==1 & (hc1 >= 6 & hc1<=59) & sh310a == 1 & hv103==1) # %>% select(cluster_hh, hv001, hv002, hvidx, kids_barcode, hv105, hv104, hc1, hv270, pfldh_kids,
                                                                                          #           hv005, hv006, hv024, shnprovin, hv025, hv040, hv201, hv106, hv246, hml1, hml10, hml20, hv027, hv028)
elig_kids_2 <- dhsmeta %>% filter(hv027==1 & hv105 < 6 & (hc1>=6 | is.nan(hc1)) & sh310a == 1) %>% select(cluster_hh, hv001, hv002, hvidx, kids_barcode, hv105, hv104, hc1, hv270, pfldh_kids,
                                                                                                          hv005, hv006, hv024, shnprovin, hv025, hv040, hv201, hv106, hv246, hml1, hml10, hml20, hv027, hv028)

# if subsetting kids from cdpr61dt: elig_kids$kids_barcode <- tolower(elig_kids$sh312)

# add hbv results 
elig_kids <- elig_kids %>% mutate(validbarcode = case_when(
  grepl("9999", elig_kids$kids_barcode) ~ 0, ## 99993  Sample damaged/insufficient/not found in lab # 99994  Not present # 99995  Refused # 99996  Other
  grepl("\\?", elig_kids$kids_barcode) ~ 0,
  TRUE ~ 1
))
elig_kids_2 <- elig_kids_2 %>% mutate(validbarcode = case_when(
  grepl("9999", elig_kids_2$kids_barcode) ~ 0, ## 99993  Sample damaged/insufficient/not found in lab # 99994  Not present # 99995  Refused # 99996  Other
  grepl("\\?", elig_kids_2$kids_barcode) ~ 0,
  TRUE ~ 1
))
table(elig_kids_2$validbarcode, useNA = "always")

# add hbv results onto this eligible kid df
elig_kids_wres <- left_join(elig_kids, kid_hbv_kr_dis[,c("kids_barcode", grep("hbv", names(kid_hbv_kr_dis), value=T))], by = "kids_barcode")
elig_kids_2_wres <- left_join(elig_kids_2, kid_hbv_kr_dis[,c("kids_barcode", grep("hbv", names(kid_hbv_kr_dis), value=T))], by = "kids_barcode")

elig_kids_wres %>% filter(validbarcode==1) %>%  group_by(hbvresult5) %>% count()
elig_kids_2_wres %>% filter(validbarcode==1) %>%  group_by(hbvresult5) %>% count()

# of these, final df with hbv results
elig_kids_whbvres <- elig_kids_wres %>% filter(validbarcode==1 & !is.na(hbvresult5)) # make sure hbvresult5 has all results
elig_kids_2_whbvres <- elig_kids_2_wres %>% filter(validbarcode==1 & !is.na(hbvresult5)) # make sure hbvresult5 has all results

# of these, final df without hbv results
nores <- elig_kids_wres %>% filter(is.na(hbvresult5) & validbarcode==1)
nores2 <- elig_kids_2_wres %>% filter(is.na(hbvresult5) & validbarcode==1)

# hbv results that aren't in final df (df per jan2024 committee discussion about starting with SURVEYED not SAMPLES)
hbvres_notonfinaldf <- kid_hbv_kr_dis %>% filter(!(kids_barcode %in% elig_kids_whbvres$kids_barcode))
# check age
table(hbvres_notonfinaldf$hv105, useNA = "always") # MOST are 5yos (>59months) for the first new flow chart
table(hbvres_notonfinaldf$hv105_fromhc1, useNA = "always")

# look at the 2903 with exhausted sample
table(nores2$pfldh_kids, useNA = "always")
table(nores$pfldh_kids, useNA = "always")

# connect hh members
elig_kids_whbvres %>% summarize(count_distinct = n_distinct(cluster_hh))

# propensity score code to address missingness-----------
## see spring 2022 EPID 722 exercise 4 (starting at q6)

# indicator variable for barcode analyzed or not, has pf result, pf+/pf-/no pf result - children 6-59mo
elig_kids <- elig_kids %>% mutate(bc_select = case_when(
  kids_barcode %in% kid_hbv_kr_dis$kids_barcode ~ 1,
  TRUE ~ 0),
  haspfres = case_when(
    !is.na(pfldh_kids) ~ 1,
    is.na(pfldh_kids) ~ 0),
  pf_3way = case_when(
    haspfres==1 & pfldh_kids==1 ~ 2, # sample has a pf result and it's positive
    haspfres==1 & pfldh_kids==0 ~ 1, # sample has a pf result and it's negative
    haspfres==0  ~ 0, # sample does not have a pf result
  ))

# indicator variable for barcode analyzed or not, has pf result, pf+/pf-/no pf result - children ≤5 (6-71mo)
elig_kids_2 <- elig_kids_2 %>% mutate(bc_select = case_when(
  kids_barcode %in% elig_kids_2_whbvres$kids_barcode ~ 1,
  TRUE ~ 0),
  haspfres = case_when(
    !is.na(pfldh_kids) ~ 1,
    is.na(pfldh_kids) ~ 0),
  pf_3way = case_when(
    haspfres==1 & pfldh_kids==1 ~ 2, # sample has a pf result and it's positive
    haspfres==1 & pfldh_kids==0 ~ 1, # sample has a pf result and it's negative
    haspfres==0  ~ 0, # sample does not have a pf result
  ))
table(elig_kids_2$bc_select)

table(elig_kids$pf_3way, useNA = "always")
table(elig_kids$hv024, useNA = "always")

#ps with full barcodes/dataset
## vars of prop score: hv006 - month of interview; shnprovin - province (try hv024, the pre-2015 provinces, if rank deficient warning affects);
## hv025 - urban/rural; hv040 - cluster altitude in meters; hv104 - sex; hv201 - source of drinking water; hv106 - education (delete for kids?);
## hv246 - owns livestock; hml1 - mosquito nets; hml10 - insecticide treated net; hml20 - slept under net; hv270 - hh wealth; hv105 - age
selectioninstudy <- glm(bc_select ~ hv006+shnprovin+hv025+hv104+hv246+hml10+hml20+hv270+hc1+pf_3way, #check removing: ; removed: hv106 (education), hml1 (has net), hv201 (water)
                        data=elig_kids,
                        family=binomial("logit"))
summary(selectioninstudy$coefficients) # vary significantly - more work into what should be included
# getting warning about rank deficiency

# denom of weights
elig_kids$d <- predict(selectioninstudy, elig_kids, type = "response")

# numerator of weights
num_mod <- glm(bc_select ~ 1, 
               data = elig_kids, 
               family = "binomial"(link="logit"))

elig_kids$n <- predict(num_mod, data = elig_kids, type = "response")

#unstandardized and standardized

### for standardized 
p_exposure <- sum(elig_kids$bc_select) / nrow(elig_kids)
summary(elig_kids$n)

elig_kids <- elig_kids %>% 
  mutate(sw = ifelse(bc_select == 1, n/d, (1-n)/(1-d)), # from 722 code
         iptw_u = ifelse(bc_select==1, 1/d, 1/(1-d)),
         iptw_s = ifelse(bc_select==1, p_exposure/d, (1-p_exposure)/(1-d))
  )
summary(elig_kids$sw) # 722 standardized weights
summary(elig_kids$iptw_u) # unstandardized weights from Hillary
summary(elig_kids$iptw_s) # standardized weights from Hillary

#look at distribution of weights, by whether sample was tested for hbv
elig_kids %>% 
  ggplot() + geom_histogram(aes(x=sw)) + facet_wrap(~bc_select, nrow = 2)

# Q to confirm - once subsetted into participants who are included, the weights will just not sum to 1, right? or does standardization take care of that
test <- elig_kids %>% filter(kids_barcode %in% elig_kids_whbvres$kids_barcode) %>% select(c(kids_barcode, sw, iptw_s, bc_select))
summary(test$sw)
summary(test$iptw_s)
mean(test$sw)

elig_kids_whbvres <- left_join(elig_kids_whbvres, elig_kids[,c("kids_barcode","iptw_s")], by ="kids_barcode")
summary(elig_kids_whbvres$iptw_s)
mean(elig_kids_whbvres$iptw_s)

# combine household weights (for kid samples) and propensity score weights accounting for missingness
# where hh weight came from: kid_dhs_int$hh_weight <- as.numeric(kid_dhs_int$hv005)/1000000
elig_kids_whbvres$hv028 <- as.numeric(elig_kids_whbvres$hv028)
elig_kids_whbvres$hv028_div <- elig_kids_whbvres$hv028/1000000
elig_kids_whbvres$hh_weight <- as.numeric(elig_kids_whbvres$hv005)/1000000

summary(elig_kids_whbvres$hv028_div)
summary(elig_kids_whbvres$hh_weight)

elig_kids_whbvres$both_wt_old <- (elig_kids_whbvres$iptw_s)*(elig_kids_whbvres$hh_weight)
elig_kids_whbvres$both_wt_new <- (elig_kids_whbvres$iptw_s)*(elig_kids_whbvres$hv028_div)
summary(elig_kids_whbvres$both_wt_old)
summary(elig_kids_whbvres$both_wt_new)

#New variables---------------
##Province collapsed------
##Tetanus Ab indic---------
##Age variable - using age in months, create new var for age in years (given discrepancy with hv105)--------
##Stunting,weight,wasting-------

#get variables in correct class
elig_kids_whbvres <- elig_kids_whbvres %>% mutate_at(c('hv024', 'hc70', 'hc71', 'hc72', 'hc57', 'hv009'), as.numeric)

# leaving new variables as numeric and before analysis can convert to factor

elig_kids_whbvres <- elig_kids_whbvres %>% mutate(
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
# tetanus serology indicators
  shtetaindasno = case_when(
    shteta=="0" ~ 0,
    shteta=="1" ~ 1,
    shteta=="3" ~ 0),
  shtetaindasyes = case_when(
    shteta=="0" ~ 0,
    shteta=="1" ~ 1,
    shteta=="3" ~ 1),
# new age in years based on months variable
  hv105_fromhc1 = case_when(
    hc1 < 12 ~ 0, # kids below 6mo = 0 years
    hc1 >= 12 & hc1 <24 ~ 1, #  kids 12-<24mo = 1 years
    hc1 >= 24 & hc1 <36 ~ 2, # kids 24=<36 = 2 years
    hc1 >= 36 & hc1 <48 ~ 3, # kids 36-<48 = 3 years
    hc1 >= 48 & hc1 <60 ~ 4, # kids 48-<60 = 4 years
    hc1 >= 60 ~ 5), # kids >=60mo = 5years
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
elig_kids_whbvres %>% group_by(shnprovin, hv024,provgrp, provgrp_kin) %>% count()
elig_kids_whbvres %>% group_by(shteta, shtetaindasno,shtetaindasyes) %>% count()
elig_kids_whbvres %>% group_by(hv105, hv105_fromhc1) %>% count()
elig_kids_whbvres %>% group_by(sevstunt, modstunt, stunt) %>% count()
elig_kids_whbvres %>% group_by(sevwasting, modwasting, wasting) %>% count()
elig_kids_whbvres %>% group_by(hc71, weightforage) %>% count()
elig_kids_whbvres %>% group_by(hc57, anemia) %>% count()

# convert certain variables to factors for analysis
elig_kids_whbvres <- elig_kids_whbvres %>% mutate_at(c('sevstunt', 'modstunt', 'stunt', 'sevwasting', 'modwasting', 'wasting', 'weightforage'), as.factor)


#Merge KR vars on------
# see 05_famtreesdhs.R for merge of biospecimen results, PR, and KR (asked to caretakers about children, able to merge ~75%)
# d_k_or imported from CDKR61FL.DTA at beginning

# make unique identifier using the line number of the person
d_k_or$clus_hh_ind <- paste(d_k_or$v001, d_k_or$v002, d_k_or$b16, sep = "_")
elig_kids_whbvres$clus_hh_ind <-  paste(elig_kids_whbvres$hv001, elig_kids_whbvres$hv002, elig_kids_whbvres$hvidx, sep = "_")

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
elig_kids_whbvres_kr <- left_join(elig_kids_whbvres, d_k_or_red, by = "clus_hh_ind" )
##check conditions of those without KR data----------
elig_kids_whbvres_kr %>% group_by(fromKR) %>% count() #595 no KR data
# mother living? hv111==1
elig_kids_whbvres_kr %>% group_by(fromKR, hv111) %>% count()
# living with mother?  (hv112==0) - only those living with mother were surveyed for KR
elig_kids_whbvres_kr %>% group_by(fromKR, hv112) %>% count() %>% print(n=Inf)
elig_kids_whbvres_kr %>% group_by(fromKR, hv102) %>% count()

##New vars------
###DPT vaccination: dpt1, dpt2, dpt3, dpt_count, dpt_doses---------
###injections--------
###beating of wife------

elig_kids_whbvres_kr <- elig_kids_whbvres_kr %>% mutate(
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
# check new variables
elig_kids_whbvres_kr %>% group_by(dpt1, dpt2, dpt3, dpt_count, dpt_doses) %>% count()
elig_kids_whbvres_kr %>% group_by(v477, injec) %>% count()
elig_kids_whbvres_kr %>% group_by(v744a, v744b, v744c, v744d, v744e, beat) %>% count() %>% print(n=Inf)









