# Missing data
library(tidyverse)

#Kids with not outcome result-------------
kidmiss <- dhsmeta %>% filter(kids == "1" & !(kids_barcode %in% kid_hbv_kr_dis$kids_barcode)) %>% 
  select(hv005, cluster_hh, latnum, longnum, kids_barcode, shnprovin, hv105, hv104, hv025, hv026, hv270, pfldh_kids)

kidmiss <- kidmiss %>% 
  dplyr::mutate(prov2015=factor(
    kidmiss$shnprovin, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
    labels = c("Kinshasa", "Kwango","Kwilu","Mai-Ndombe","Kongo Central","Equateur","Mongala","Nord-Ubangi","Sud-Ubangi","Tshuapa","Kasai","Kasai-Central","Kasai-Oriental","Lomami","Sankuru","Haut-Katanga","Haut-Lomami","Lualaba","Tanganyka","Maniema","Nord-Kivu","Bas-Uele","Haut-Uele","Ituri","Tshopo","Sud-Kivu")))

# count missing by household
miskid_sumclusthh <- kidmiss %>% group_by(cluster_hh) %>% count()

#are missing from unique households or from same households?
clusthhnonmiss <- kid_hbv_kr_dis %>% group_by(cluster_hh) %>% count()

overlap <- miskid_sumclusthh %>% filter(cluster_hh %in% clusthhnonmiss$cluster_hh)
nrow(overlap) / nrow(miskid_sumclusthh) # about half overlap

# map missing
kidmiss %>% filter(latnum=="0") %>% count() # 156 /1853 with missing GPS
view(kidmiss)
library(sf)
mapmiss = st_as_sf(kidmiss[!is.na(kidmiss$latnum) &!is.na(kidmiss$longnum),], coords = c("longnum", "latnum"), crs = 4326) 

ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=mapmiss, alpha=0.8) + 
  #labs(color='HBV prevalence \nin children < 5') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  #ggtitle("Children ≤ 5")+ # for adding with adult figure
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

# 
kid_hbv_kr_dis %>% filter(is.nan(hc1)) %>% count(hv105)
table(kid_hbv_kr_dis$hc1, useNA = "always")

# check missingness of kids without sample result----------
# Demographics of kids with no hbv result
nrow(kidmiss) # 1853
kidmiss %>% group_by(prov2015) %>% count() %>% print(n=Inf)
dhsmeta %>% filter(kids=="1" & (hv105=="0" | hv105=="1" | hv105=="2" | hv105=="3" | hv105=="4" | hv105=="5")) %>% group_by(shnprovin) %>% count() %>% print(n=Inf)
class(dhsmeta$hv105)

kidbarcodes <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/Under5_Inventory_Apr2017.xlsx", 
                          sheet = "ALL 0-5 SAMPLES copy")
kidbarcodes$kids_barcode <- tolower(kidbarcodes$`CORRESPONDING DHS BAR CODE`)
nrow(kidbarcodes)
table(kidbarcodes$GROUP)

barcodenodata <- kidbarcodes %>% filter(!(kids_barcode %in% kid_hbv_kr_dis$kids_barcode))
table(barcodenodata$GROUP)

nonzerogroup <- kidbarcodes %>% filter(GROUP!= "0" & !(kids_barcode %in% barcodenodata$kids_barcode)) 
checkstat <- kid_hbv_kr_dis %>% filter((kids_barcode %in% nonzerogroup$kids_barcode)) %>% select(kids_barcode,hv005, cluster_hh,hbvresult )
table(checkstat$hbvresult)

# focus on eval of missingness on missing Group 0s, n=1251

group0miss <- kidbarcodes %>% filter(GROUP== "0" & !(kids_barcode %in% kid_hbv_kr_dis$kids_barcode)) 
# get metadata on these
group0miss_dem <- dhsmeta %>% filter(kids_barcode %in% group0miss$kids_barcode) %>% 
  select(hv005, cluster_hh, latnum, longnum, kids_barcode, shnprovin, hv105, hv104, hv025, hv026, hv270, pfldh_kids)
# Demogs of missingness
## Age
group0miss_dem %>% group_by(hv105) %>% count()
dhsmeta %>% filter(kids_barcode !="" & (hv105=="0" | hv105=="1" | hv105=="2" | hv105=="3" | hv105=="4" | hv105=="5")) %>%  group_by(hv105) %>% count()
dhsmeta %>% filter(kids == "1" & (hv105=="0" | hv105=="1" | hv105=="2" | hv105=="3" | hv105=="4" | hv105=="5")) %>%  group_by(hv105) %>% count()
# sex
group0miss_dem %>% group_by(hv104) %>% count() #1=male, 2=female
dhsmeta %>% filter(kids == "1" & (hv105=="0" | hv105=="1" | hv105=="2" | hv105=="3" | hv105=="4" | hv105=="5")) %>%  group_by(hv104) %>% count()
# wealth
group0miss_dem %>% group_by(hv270) %>% count()
dhsmeta %>% filter(kids == "1" & (hv105=="0" | hv105=="1" | hv105=="2" | hv105=="3" | hv105=="4" | hv105=="5")) %>%  group_by(hv270) %>% count()
# province
group0miss_dem %>% group_by(shnprovin) %>% count() %>% print(n=Inf)
dhsmeta %>% filter(kids == "1" & (hv105=="0" | hv105=="1" | hv105=="2" | hv105=="3" | hv105=="4" | hv105=="5")) %>%  group_by(shnprovin) %>% count()%>% print(n=Inf)

# all kids----------
CDOB61FL <- read.dta("/Users/camillem/Documents/GitHub/dhs_hbv/Data/CDOB61FL/POLIO RESULTS CDC 12MAY2018.DTA", convert.factors = FALSE)
cdpr61dt <- read.dta("/Users/camillem/Documents/GitHub/dhs_hbv/Data/CDPR61DT/CDPR61FL.DTA", convert.factors = FALSE)
# create unique ID for households (need to combine cluster and hh number)
dhsmeta$cluster_hh <- paste(dhsmeta$hv001, dhsmeta$hv002,sep = "_")
dhsmeta$hv105 <- as.numeric(dhsmeta$hv105)

# important variables for kids' sampling: hv027 (selected for male interview: 0=no, 1=yes); sh310a (consent for vaccine biospecimen read: 1=granted,2=refused)
# number of distinct household selected and not selected for male interview (these are also the households selected for the kids biospecimen collection)
dhsmeta %>% group_by(hv027) %>% summarize(count_distinct = n_distinct(cluster_hh))
# or can use cdpr61dt

# number of children 6-59 months in selected households
dhsmeta %>% filter(hv027==1 & (hc1 >= 6 & hc1<=59)) %>% count() 
# number of children 6-71 months (≤5 years)
###**side bar to assess the children between 5 and 6########
k_5yo <- dhsmeta %>% filter(hv027==1 & as.numeric(hv105)==5) %>% select(sh310a, hv005, cluster_hh, latnum, longnum, kids_barcode, shnprovin,shteta,  hv103, hv027, hv028, hv105, hc1, hv104, hv025, hv026, hv270, pfldh_kids)
k_5yo <- k_5yo %>% mutate(hasbarcode = case_when(
  kids_barcode!="" ~ 1,
  kids_barcode=="" ~ 0,
))

addmargins(table(barcode = k_5yo$hasbarcode, consent = k_5yo$sh310a, useNA = "always"))
#kids age 0 - find months age
k_0yo <- dhsmeta %>% filter(hv027==1 & as.numeric(hv105)==0 & hc1<6) %>% select(hc18, hc19, hc30, hc31, hc32, sh310a, hv005, cluster_hh, latnum, longnum, kids_barcode, shnprovin,shteta,  hv103, hv027, hv028, hv105, hc1, hv104, hv025, hv026, hv270, pfldh_kids)
table(k_0yo$hc1, useNA = "always") # none missing - can use to remove those <6mo
table(k_0yo$sh310a, useNA = "always")
#########
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
elig_kids <- dhsmeta %>% filter(hv027==1 & (hc1 >= 6 & hc1<=59) & sh310a == 1 & hv103==1) %>% select(cluster_hh, hv001, hv002, hvidx, kids_barcode, hv105, hv104, hc1, hv270, pfldh_kids,
                                                                                                     hv005, hv006, hv024, shnprovin, hv025, hv040, hv201, hv106, hv246, hml1, hml10, hml20, hv027, hv028)
elig_kids_2 <- dhsmeta %>% filter(hv027==1 & hv105 < 6 & (hc1>=6 | is.nan(hc1)) & sh310a == 1) %>% select(cluster_hh, hv001, hv002, hvidx, kids_barcode, hv105, hv104, hc1, hv270, pfldh_kids,
                                                                                                     hv005 hv006, hv024, shnprovin, hv025, hv040, hv201, hv106, hv246, hml1, hml10, hml20, hv027, hv028)

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
test <- elig_kids %>% filter(kids_barcode %in% elig_kids_whbvres$kids_barcode)
summary(test$sw)
summary(test$iptw_s)
mean(test$sw)

test <- left_join(elig_kids_whbvres, elig_kids[,c("kids_barcode","iptw_s")], by ="kids_barcode")
summary(test$iptw_s)
mean(test$iptw_s)

# combine household weights (for kid samples) and propensity score weights accounting for missingness
# where hh weight came from: kid_dhs_int$hh_weight <- as.numeric(kid_dhs_int$hv005)/1000000
test$hv028 <- as.numeric(test$hv028)
test$hv028_div <- test$hv028/1000000
test$hh_weight <- as.numeric(test$hv005)/1000000

summary(test$hv028_div)
summary(test$hh_weight)

test$both_wt_old <- (test$iptw_s)*(test$hh_weight)
test$both_wt_new <- (test$iptw_s)*(test$hv028_div)
summary(test$both_wt_old)
summary(test$both_wt_new)

# check effect of propens weights
# make survey design object
# use hv028 (weight for households selected for male interview)
library(survey)
library(srvyr)
designf <-svydesign(ids=test$hv001, strata=test$hv022, weights=test$hv028_div,  data=test)
options(survey.lonely.psu="adjust")
designf_dhs2 <-as_survey_design(designf)

prop.table(svytable(~hbvresult5, designf_dhs2))
svyciprop(~hbvresult5, designf_dhs2, method="lo")


