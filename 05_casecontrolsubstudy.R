# Revised version of 05_famtreesdhs.R

library(tidyverse)
library(writexl)
library(readxl)
library(survey)
library(srvyr)

# new dataset for kids created in 99_missing.R, after starting with kids in selected households (for male interview, these are the hhs with kids with biospecimen), kids eligible (age, consent, de facto resident), kids w sampled, kids with hbv result on sample
# this data is titled elig_kids_whbvres, has 5608 observations, OR elig_kids_2_whbvres which has 6851 observations
# here we need to connect the household members. and create multiple versions for the sensitivity analyses

# indicator variables for case hh status at s/co 5, 1, 2, and 100
# count of positive kids at each s/co cutoff

# START JAN 19 USING elig_kids_2_whbvres
table(elig_kids_whbvres_wt_kr$hbvresult5, useNA = "always")

kids_hh <- 
  elig_kids_whbvres_wt_kr %>% group_by(cluster_hh) %>% 
  summarise(n_poskids5 = sum(hbvresult5), 
            n_poskids1 = sum(hbvresult1), 
            n_poskids2 = sum(hbvresult2), 
            n_poskids100 = sum(hbvresult100), 
            n_kids = n()) %>% 
  mutate(casehh5 = case_when(
    n_poskids5 == 1 ~ 1, # case household if one pos kid (remove the ~7 hh with two pos kids)
    n_poskids5 == 0 ~ 0, # control household if zero positive children
    TRUE ~ NA_real_),
    casehh1 = case_when(
      n_poskids1 == 1 ~ 1, # case household if one pos kid (remove the ~7 hh with two pos kids)
      n_poskids1 == 0 ~ 0, # control household if zero positive children
      TRUE ~ NA_real_),
    casehh2 = case_when(
      n_poskids2 == 1 ~ 1, # case household if one pos kid (remove the ~7 hh with two pos kids)
      n_poskids2 == 0 ~ 0, # control household if zero positive children
      TRUE ~ NA_real_),
    casehh100 = case_when(
      n_poskids100 == 1 ~ 1, # case household if one pos kid (remove the ~7 hh with two pos kids)
      n_poskids100 == 0 ~ 0, # control household if zero positive children
      TRUE ~ NA_real_),
  )
# original list of case/control households based on s/co of 1 and included kids≥5
allhhmemb_ccstat %>% group_by(cc_staneana) %>% count()
# merge original list (to get controls)
kids_hh <- left_join(kids_hh, allhhmemb_ccstat, by = "cluster_hh")

# combine case status using correct sco cutoffs with control list at time of sampling for final list of case/control hhs
kids_hh <- kids_hh %>% mutate(
  ccstat5fin = case_when(
    casehh5==1 ~ 1,
    cc_staneana==0 ~ 0,
    TRUE ~ NA_real_),
  ccstat1fin = case_when(
    casehh1==1 ~ 1,
    cc_staneana==0 ~ 0,
    TRUE ~ NA_real_),
  ccstat2fin = case_when(
    casehh2==1 ~ 1,
    cc_staneana==0 ~ 0,
    TRUE ~ NA_real_),
  ccstat100fin = case_when(
    casehh100==1 ~ 1,
    cc_staneana==0 ~ 0,
    TRUE ~ NA_real_)
  )
kids_hh %>% group_by(casehh5, ccstat5fin) %>% count()
view(kids_hh)
# merge counts back onto df with all kids (likely a way to do this in a single step, but this works for now)
elig_kids_whbvres_wt_kr <- left_join(elig_kids_whbvres_wt_kr, kids_hh[, c("cluster_hh","ccstat5fin", "ccstat1fin","ccstat2fin","ccstat100fin")], by = "cluster_hh")
nrow(elig_kids_whbvres_wt_kr)
elig_kids_whbvres_wt_kr %>% group_by(ccstat1fin) %>% count()
elig_kids_whbvres_wt_kr %>% group_by(ccstat5fin) %>% count()

# case-control substudy investigating hbsag prev among household members-----------
# subset kids df 
cc_kids5 <- elig_kids_whbvres_wt_kr %>% filter(!is.na(ccstat5fin))
cc_kids1 <- elig_kids_whbvres_wt_kr %>% filter(!is.na(ccstat1fin))
cc_kids2 <- elig_kids_whbvres_wt_kr %>% filter(!is.na(ccstat2fin))
cc_kids100 <- elig_kids_whbvres_wt_kr %>% filter(!is.na(ccstat100fin))

# find correct dataset of adults
view(hhmemb5) # `both` contains kids and adult results
hhmemb5 <- dhsmeta %>% filter(cluster_hh %in% cc_kids5$cluster_hh) %>%  
  select(hv001, hv002, hvidx, hv101, hv005, hv006, hv009, hv010, hv011, hv012, hv013, hv014, cluster_hh, latnum, longnum, dbsbarcode, shnprovin, hv105, hv104, hv025, hv026, hv027, hv028, hv040,
         hv201, hv270, hv246, hml1, hml10, hml20, ha64, hb64,
         pfldh_kids, pfldh_adult, pv18s_adult, pv18s, po18s)
table(hhmemb5$hb64, useNA = "always")
table(hhmemb5$ha64, useNA = "always")
class(hhmemb5$ha64)
class(hhmemb5$hb64)

hhmemb5$adline <- paste(hhmemb5$hv001, hhmemb5$hv002, hhmemb5$hvidx, sep = "_")

hhmemb5 <- hhmemb5 %>% mutate(eligtesting = case_when(
    hv105 >= 15 & hv105 < 50 & hv104=="2" ~ 1,
    hv105 >= 15 & hv105 < 60 & hv104=="1" ~ 1,
    TRUE ~ 0),
    consent_add = case_when(
    ha64 == "1" ~ 1,
    ha64 == "2" ~ 3,
    ha64 == "3" ~ 3,
    hb64 == "1" ~ 1,
    hb64 == "2" ~ 3,
    hb64 == "3" ~ 3,
    TRUE ~ NA_real_),
    bc_coll = case_when(
      dbsbarcode == "99991" ~ 0, # sample not present
      dbsbarcode == "99994" ~ 0, # sample not present
      dbsbarcode == "99995" ~ 0, # refused
      dbsbarcode == "99996" ~ 0, # other
      dbsbarcode == "?" ~ 0, # sample not present
      TRUE ~ 1 # samples with valid 5 digit barcode
    ))
table(hhmemb5$eligtesting, hhmemb5$bc_coll, useNA = "always")   
hhmemb5 %>% filter(eligtesting==1 & is.na(consent_add)) %>% reframe(dbsbarcode, ha64, hb64)
hhmemb5 %>% filter(eligtesting==1) %>% group_by(ccstat5fin,bc_coll ) %>% count()

# hbv on 1/2 instead of 0/1
adults4mrg$hbv_adcorr <- adults4mrg$hbv - 1
adults4mrg %>% group_by(hbv) %>% count()
adults4mrg %>% group_by(hbv_adcorr) %>% count()

hhmemb5 %>% filter(eligtesting==1 & consent_add==1) %>% group_by()
hhmemb5 <- left_join(hhmemb5,adults4mrg[,c("dbsbarcode", "hbv_adcorr", "clus_hh_ind")], by = "dbsbarcode")

#sum pos counts at hh level
ad_sum_clusthh <- 
  adults4mrg %>% group_by(cluster_hh) %>% 
  summarise(n_posad5 = sum((hbv_adcorr)),
            n_ad_tested = n(),
            prev5_adtested = n_posad5/n_ad_tested)
table(poscount = ad_sum_clusthh$prev5_adtested, useNA = "always")

# add pos counts and total tested from hh onto list of household members
hhmemb5 <- left_join(hhmemb5, ad_sum_clusthh, by = "cluster_hh")
# counts by case/control
hhmemb5 <- left_join(hhmemb5, kids_hh[, c("cluster_hh", "n_poskids5","ccstat5fin" )], by = "cluster_hh")
addmargins(table(hhmemb5$ccstat5fin, useNA = "always"))

table(ccstat = hhmemb5$ccstat5fin, hhmemb5$hbv_adcorr, useNA = "always" )
# still seeing 8% prevalence among control households

# missingness more among control hh?
hhmemb5 %>% filter(eligtesting==1 & consent_add==1) %>% group_by(ccstat5fin, hbv_adcorr) %>%  count()
table(hhmemb5$n_posad5, hhmemb5$ccstat5fin)

# Adult member flow chart------
# in selected case and control households, how many adult members with DBS were there
hhmemb5 %>% group_by(ccstat5fin, eligtesting) %>% count()
# within adults in DBS range, how many had barcodes collected
hhmemb5 %>% filter(eligtesting == 1) %>% group_by(ccstat5fin,bc_coll ) %>% count()
hhmemb5 %>% filter(eligtesting == 1 &bc_coll==0 &ccstat5fin==1) %>% reframe(ccstat5fin,dbsbarcode,ha64, hb64 )
hhmemb5 %>% filter(eligtesting == 1 &bc_coll==0 &ccstat5fin==0) %>% reframe(ccstat5fin,dbsbarcode,ha64, hb64 )
# within adults in DBS range, how many consented for additional testing on their DBS
hhmemb5 %>% filter(eligtesting == 1 &ccstat5fin==0) %>% group_by(bc_coll, consent_add,hbv_adcorr ) %>% count()
hhmemb5 %>% filter(eligtesting == 1 &ccstat5fin==0 &bc_coll==1 &consent_add==1) %>% group_by(hbv_adcorr ) %>% count()
hhmemb5 %>% filter(eligtesting == 1) %>% group_by(ccstat5fin, bc_coll, consent_add,hbv_adcorr ) %>% count()
hhmemb5 %>% filter(eligtesting == 1 & bc_coll==1) %>% group_by(ccstat5fin ) %>% count()
hhmemb5 %>% filter(eligtesting == 1 & bc_coll==1) %>% group_by(ccstat5fin,consent_add,hbv_adcorr ) %>% count()
### who had missing consent - are these adults without DBS?
hhmemb5 %>% filter(eligtesting == 1 & is.na(consent_add)) %>% group_by(ccstat5fin) %>%  reframe(dbsbarcode, ha64, hb64, pfldh_adult) %>% print(n=Inf)
# adults with sample with consent
hhmemb5 %>% filter(eligtesting == 1 & bc_coll==1 & consent_add==1) %>% group_by(ccstat5fin,hbv_adcorr ) %>% count()
hhmemb5 %>% filter(eligtesting == 1 & bc_coll==1 & consent_add==1 & !is.na(hbv_adcorr)) %>% group_by(ccstat5fin,hbv_adcorr ) %>% count()

### evaluate hh memb with valid barcode, consent for add. testing, but no hbv result - confirm sample was exhausted, did they have pf testing
hhmemb5 %>% filter(eligtesting == 1 & bc_coll==1 & consent_add==1 & is.na(hbv_adcorr)) %>% reframe(dbsbarcode, pfldh_adult)
hhmemb5 %>% filter(eligtesting == 1 & bc_coll==1 & consent_add==1 & is.na(hbv_adcorr)) %>% group_by(pfldh_adult) %>% count()
adnohbv <- hhmemb5 %>% filter(eligtesting == 1 & bc_coll==1 & consent_add==1 & is.na(hbv_adcorr)) %>% select(c(dbsbarcode, clus_hh_ind, cluster_hh, pfldh_adult, ccstat5fin))
nrow(adnohbv)

# import results from hbsag testing with notes about samples
hhresults <- read_excel(here("Data", "allhhmemtotest_res.xlsx"), sheet = "Sheet1")
hhresults$dbsbarcode <- tolower(hhresults$barcodes)
hhresults <- hhresults %>% relocate(dbsbarcode)

hhresults_sag <- read_excel(here("Data", "allhhmemtotest_res.xlsx"), sheet = "Results")
hhresults_sag$dbsbarcode <- tolower(hhresults_sag$Barcodes)
hhresults_sag <- hhresults_sag %>% relocate(dbsbarcode)
 # select hh member results that are in the df of hh member without hbv results - check if sample missing, plate not found, etc.
t <- hhresults %>% filter(dbsbarcode %in% adnohbv$dbsbarcode)

hhresultsall <- left_join(hhresults, hhresults_sag, by = "dbsbarcode")

missingad <- hhresultsall %>% filter(dbsbarcode %in% adnohbv$dbsbarcode)
missingad <- missingad %>% mutate(whymiss = case_when(
  RESULT == "low vol" ~ "low volume",
  RESULT == "Low vol" ~ "low volume",
  is.na(RESULT) ~ "sample not available"))
missingad %>% group_by(whymiss) %>% count()

# adults without result not in inventory
t_i <- adnohbv %>% filter(!(dbsbarcode %in% t$dbsbarcode))
t_i2 <- left_join(t_i, kids_hh, by = "cluster_hh")
view(t_i2)
untested <- elig_kids_whbvres_wt_kr %>% filter(cluster_hh %in% t_i2$cluster_hh) %>% select(c(kids_barcode, cluster_hh, hv104, hv105_fromhc1_f, prov2015,hbvresult5, hbvresult_abb))
untested %>% group_by(cluster_hh, hv105_fromhc1_f, hbvresult5, prov2015, kids_barcode, hbvresult_abb) %>% count()

elig_kids_whbvres_wt_kr %>% filter(!is.na(hbvresult_abb) & (hbvresult_abb != ccstat5fin)) %>% reframe(kids_barcode, hbvresult_abb, ccstat5fin, cluster_hh)
elig_kids_whbvres_wt_kr %>% filter(hbvresult_abb==1 & is.na(ccstat5fin)) %>% reframe(kids_barcode, hbvresult_abb, ccstat5fin, cluster_hh)

elig_kids_whbvres_wt_kr %>% group_by(hbvresult_abb, hbvresult5) %>% count()

check <- kid_hbv_kr_dis %>% filter(kids_barcode == "i7h2y")
check %>% reframe(kids_barcode, cluster_hh, catresult, k08orabb, hbvresult_abb, hbvresult5) # this was the 1/46 low volume redistributed as positive
# the 12 adults without result from the 4 households should be classified as "no result"

adnohbv <- left_join(adnohbv,missingad, by = "dbsbarcode" )

adnohbv <- adnohbv %>% mutate(whymiss2 = case_when(
  dbsbarcode %in% t_i2$dbsbarcode ~ "not tested",
  TRUE ~ whymiss
))
adnohbv %>% group_by(whymiss2, whymiss, ccstat5fin) %>% count()

# hh member status onto kids df------
elig_kids_whbvres_wt_kr <- left_join(elig_kids_whbvres_wt_kr, ad_sum_clusthh[, c("cluster_hh", "n_posad5","n_ad_tested" )], by = "cluster_hh")
elig_kids_whbvres_wt_kr %>% group_by(ccstat5fin, n_posad5) %>% count()
elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate(exposed = case_when(n_posad5 >0 ~ 1,
                                            n_posad5==0 ~ 0))
nrow(elig_kids_whbvres_wt_kr)
elig_kids_whbvres_wt_kr %>% group_by(hbvresult5, ccstat5fin, exposed) %>% count()

# exclude those that were cases at each sco level
elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate(
  ccstat5fin2 = case_when(
    hbvresult5 != ccstat5fin ~ NA_real_,
    TRUE ~ ccstat5fin),
  ccstat1fin2 = case_when(
    hbvresult1 != ccstat1fin ~ NA_real_,
    TRUE ~ ccstat1fin),
  ccstat2fin2 = case_when(
    hbvresult2 != ccstat2fin ~ NA_real_,
    TRUE ~ ccstat1fin),
  ccstat100fin2 = case_when(
    hbvresult100 != ccstat100fin ~ NA_real_,
    TRUE ~ ccstat100fin)
)
elig_kids_whbvres_wt_kr %>% group_by(hbvresult5, ccstat5fin, ccstat5fin2, exposed) %>% count()
elig_kids_whbvres_wt_kr %>% group_by(hbvresult1, ccstat1fin, ccstat1fin2, exposed) %>% count()
elig_kids_whbvres_wt_kr %>% group_by(hbvresult2, ccstat2fin, ccstat2fin2, exposed) %>% count()
elig_kids_whbvres_wt_kr %>% group_by(hbvresult100, ccstat100fin, ccstat100fin2, exposed) %>% count()

#Line num - ad pos Evaluate if mother or father is HBV+ ----------
# label variable hv111    "Mother alive"
#label variable hv112    "Mother's line number"
#label variable hv113    "Father alive"
#label variable hv114    "Father's line number"
#label define HV111   0 "No" 1 "Yes" 8 "Don't know"
#label define HV112   0 "Mother not in household"
#label define HV113   0 "No"1 "Yes" 8 "Don't know"
#label define HV114   0 "Father not in household"

elig_kids_whbvres_wt_kr$motherline <- paste(elig_kids_whbvres_wt_kr$hv001, elig_kids_whbvres_wt_kr$hv002, elig_kids_whbvres_wt_kr$hv112, sep = "_")
elig_kids_whbvres_wt_kr$fatherline <- paste(elig_kids_whbvres_wt_kr$hv001, elig_kids_whbvres_wt_kr$hv002, elig_kids_whbvres_wt_kr$hv114, sep = "_")

#  count of kids with living mother - unweighted and weighted
elig_kids_whbvres_wt_kr %>% group_by(hv111) %>% count()
# svytotal(~as.factor(hv111), designf_dhs2, na.rm=T, survey.lonely.psu="adjust")  # need to run survey design first
# count of kids with mother in study
elig_kids_whbvres_wt_kr %>% group_by(hv112) %>% count()
elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate(mothenr = case_when(hv112=="0" ~ 0, TRUE ~ 1), fathenr = case_when(hv114=="0" ~ 0, TRUE ~ 1))
elig_kids_whbvres_wt_kr %>% group_by(mothenr, hv112) %>% count()
elig_kids_whbvres_wt_kr %>% group_by(fathenr, hv114) %>% count()
#svytotal(~as.factor(mothenr)+hv111, designf_dhs2, na.rm=T, survey.lonely.psu="adjust") # 5089/(5089+494) = 91% of children have mothers enrolled in

# need to draw in respondent line number when getting hbv results above 
hhmemb5_slim2 <- hhmemb5 %>% select(c(adline, hbv_adcorr, dbsbarcode, hvidx, hv101))
nrow(hhmemb5_slim) # from older hhmemb5; slim2 from hhmemb5 run today from dhsmeta above
hhmemb5_asmom <- hhmemb5_slim %>% dplyr::rename(motherline = adline)
hhmemb5_asfa <- hhmemb5_slim %>% dplyr::rename(fatherline = adline)
hhmemb5_asmom2 <- hhmemb5_slim2 %>% dplyr::rename(motherline = adline, adrelhh = hv101)
hhmemb5_asfa2 <- hhmemb5_slim2 %>% dplyr::rename(fatherline = adline, adrelhh = hv101)

momstest <- merge(elig_kids_whbvres_wt_kr[, c("cluster_hh", "clus_hh_ind", "kids_barcode", "hv101","hv105_fromhc1_f","hbvresult5", "hbvresult1", "hbvresult2", "hbvresult100", "motherline","fatherline","mothenr")],
                  hhmemb5_asmom2, by = "motherline")
fathertest <- merge(elig_kids_whbvres_wt_kr[, c("cluster_hh", "clus_hh_ind", "kids_barcode","hv101","hv105_fromhc1_f", "hbvresult5", "hbvresult1", "hbvresult2", "hbvresult100", "motherline","fatherline", "mothenr")],
                    hhmemb5_asfa2, by = "fatherline")
nrow(momstest)
nrow(fathertest)
# n=642 or n=633 that merge with mother
momstest %>% group_by(mothenr,hbvresult5, hbv_adcorr) %>% count()
fathertest %>% group_by(hbvresult5, hbv_adcorr) %>% count()
nrow(fathertest) # n = 467 that match to fathers

others <- hhmemb5_slim2 %>% filter(!(adline %in% momstest$motherline) & !(adline %in% fathertest$fatherline))
nrow(others) + nrow(momstest) + nrow(fathertest)
nrow(hhmemb5_slim2)
## focus on mothers------
momstest %>% group_by(adrelhh, hv101) %>% count() # most moms are spouse to head of hh and child is their child
momstest %>% group_by(hbvresult5, hbv_adcorr) %>% count()
fathertest %>% group_by(hbvresult5, hbv_adcorr) %>% count()

# in moms, there were 42 kids who were pos with neg moms and in fathers, there were 34 kids pos but dads neg
## 1) within the 34, are those with pos moms?
## 2) of the 42 pos kids, are other adults pos in hh?
# hh with a pos kid but mom is neg
kmn <- momstest %>% filter(hbvresult5==1 & hbv_adcorr==0)
kmn %>% n_distinct(kmn$cluster_hh)
kmn2 <- hhmemb5 %>% filter(cluster_hh %in% kmn$cluster_hh)
kmn2 %>% group_by(hbv_adcorr) %>% count()
kmn2 %>% group_by(hbv_adcorr, hv101) %>% count()
kmn2 %>% group_by(hbv_adcorr, hv105) %>% count() %>% print(n=Inf)
kmn2 %>% filter(hv105 < 15 | (hv105 >49 &hv104==2) | (hv105 > 59 & hv104==1)) %>% count()
# of these 42 kids in 42 distinct households, there were 254 adult household members, 161 outside age group tested
kmn2 %>% filter(!(hv105 < 15 | (hv105 >49 &hv104==2) | (hv105 > 59 & hv104==1))) %>% group_by(hbv_adcorr, hv101) %>% count()
# of the 93 within age group tested, 11 pos, 74 neg, and 8 not tested)
# the 11 pos household members were: 5 head of household, 5 siblings of head, and 1 parent
# the 11 pos household members were: 9 male, 2 female; median age 30
kmn2 %>% filter(!(hv105 < 15 | (hv105 >49 &hv104==2) | (hv105 > 59 & hv104==1)) & hbv_adcorr==1) %>% group_by(hv104, hv105) %>% count()
kmn2 %>% filter(!(hv105 < 15 | (hv105 >49 &hv104==2) | (hv105 > 59 & hv104==1)) & hbv_adcorr==1) %>% summarise(median(hv105), mean(hv105))

# who are the household members positive?
adpos <- hhmemb5 %>% filter(hbv_adcorr==1)
adpos %>% group_by(n_poskids5) %>% count() # 1/3 live in households with a pos child
adpos %>% group_by(hv104) %>% count() # 50-50 male/female (makes sense with mothers)
adpos %>% summarise(median(hv105)) # median age 28 years
adpos %>% group_by(hv101) %>% count() # most are head of hh (38) or spouse of head (27). 22 are siblings of head of hh
adpos %>% group_by(n_posad5) %>% count() # in 3 hh, 3 adults are pos; in 24, 2 are pos; in 73, one adult is pos
adpos %>% group_by(shnprovin) %>% count() %>% print(n=Inf) # most are head of hh (38) or spouse of head (27). 22 are siblings of head of hh
adpos %>% filter(n_posad5 > 1) %>% group_by(n_posad5, shnprovin) %>% count() # most are head of hh (38) or spouse of head (27). 22 are siblings of head of hh

# Table 3 counts
# subset kids again
cc_kids5_fin <- elig_kids_whbvres_wt_kr %>% filter(!is.na(ccstat5fin2))
cc_kids1_fin <- elig_kids_whbvres_wt_kr %>% filter(!is.na(ccstat1fin2))
cc_kids2_fin <- elig_kids_whbvres_wt_kr %>% filter(!is.na(ccstat2fin2))
cc_kids100_fin <- elig_kids_whbvres_wt_kr %>% filter(!is.na(ccstat100fin2))

#cc_kids5_fin <- left_join(cc_kids5_fin, ad_sum_clusthh[, c("cluster_hh", "n_posad5","n_ad_tested" )], by = "cluster_hh")
cc_kids5_fin %>% group_by(ccstat5fin2, n_posad5, exposed) %>% count()
#cc_kids5 <- cc_kids5 %>% mutate(exposed = case_when(n_posad5 >0 ~ 1,n_posad5==0 ~ 0))
cc_kids5_fin %>% group_by(exposed, hbvresult5) %>% count()

# get exposure is pos mother
cc_kids5m <- merge(cc_kids5_fin[, c("cluster_hh", "clus_hh_ind", "kids_barcode", "hv101","hv105_fromhc1_f","hbvresult5", "hbvresult1", "hbvresult2", "hbvresult100", "motherline","fatherline","mothenr")], hhmemb5_asmom2, by = "motherline")
cc_kids5m %>% group_by(mothenr,hbvresult5, hbv_adcorr) %>% count()
nrow(cc_kids5m)
cc_kids5m$hasmom <- 1
cc_kids5m <- cc_kids5m %>% mutate(exp_moth = case_when(
  hbv_adcorr == 1 ~ 1,
  hbv_adcorr == 0 ~ 0,
  is.na(hbv_adcorr) ~ 9 # missing result
))
table(cc_kids5m$exp_moth)  

cc_kids5_fin2 <- left_join(cc_kids5_fin, cc_kids5m[,c("clus_hh_ind", "hasmom", "exp_moth")], by = "clus_hh_ind")
nrow(cc_kids5_fin2)                  
cc_kids5_fin2 %>% group_by(hasmom) %>% count()
cc_kids5_fin2 %>% group_by(exp_moth, hbvresult5) %>% count()

# Final case-control dataset--------------
cc_kids5_fin2 %>% group_by(hbvresult5, ccstat5fin, ccstat5fin2, exposed ) %>% count()
# households with hh result
cc_kids5_fin2 %>% group_by(ccstat5fin2, exposed) %>% summarise(n_distinct(cluster_hh))
cc_kids5_fin2 %>% group_by(ccstat5fin2, exposed) %>% count()
cc_kids5_fin2 %>% filter(!is.na(exposed))  %>% group_by(ccstat5fin) %>% count()
cc_kids5_fin2 %>% filter(!is.na(exposed))  %>% group_by(ccstat5fin) %>% summarise(n_distinct(cluster_hh))

##Case-control Tab1------------
### look at distribution of weights
summary(cc_kids5_fin2$both_wt_new)
summary(cc_kids5_fin2$both_wt_old)
summary(cc_kids5_fin2$iptw_s)
summary(cc_kids5_fin2$hv028_div)
summary(elig_kids_whbvres_wt_kr$iptw_s)
summary(elig_kids_whbvres_wt_kr$both_wt_new)
summary(elig_kids_whbvres_wt_kr$hv028_div)
elig_kids_2 %>% summarise(mean(as.numeric(hv028)/1000000), median(as.numeric(hv028)/1000000), mean(as.numeric(hv005)/1000000))

table(elig_kids_2$shteta, useNA = "always")
elig_kids_2 %>% group_by(hc1) %>% count(shteta)

cc_kids5_fin2$exposed <- as.factor(cc_kids5_fin2$exposed)
cc_kids5_fin2$exp_moth <- as.factor(cc_kids5_fin2$exp_moth)

# make case control pop a survey object
designf_cc <-svydesign(ids=cc_kids5_fin2$hv001, strata=cc_kids5_fin2$hv022 , weights=cc_kids5_fin2$both_wt_new,  data=cc_kids5_fin2) # which weight: hv028_div, both_wt_new
options(survey.lonely.psu="adjust")
designf_dhs2cc <-as_survey_design(designf_cc)

# table of study pop characteristics
# counts for n in dataset, stratified by HBV Y or N
survtable <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresult5, designf_dhs2cc, svytotal, na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()
}
# mean for continuos vars in dataset, stratified by HBV Y or N
survmean <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresult5, designf_dhs2cc, svymean, na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()
}
# numeric vars
age_numcc <- as.data.frame(survmean("hc1")) %>% rownames_to_column(var = "cov")  %>%  
  mutate(levels = "Age (months), mean (SD)") %>% rename(mean = hc1)
hhmem_cc <- as.data.frame(survmean("hv009")) %>% rownames_to_column(var = "cov")  %>%  
  mutate(levels = "Household members, mean (SD)") %>% rename(mean = hv009)
kids_cc <- as.data.frame(survmean("hv014")) %>% rownames_to_column(var = "cov")  %>%  
  mutate(levels = "Children in household, mean (SD)") %>% rename(mean = hv014)
avgs_totcc <- dplyr::bind_rows(list(age_numcc, hhmem_cc, kids_cc), .id = 'source') %>% mutate(Tab1tot = paste(round(mean,1),' (',round(se,2),')', sep = ""))
Tab1_num_cc <- avgs_totcc %>% mutate(hbvresult5_lab = case_when(hbvresult5 == 0 ~ "Tab1_ctrl",
                                                                 hbvresult5 == 1 ~ "Tab1_cc")) %>% 
  select(-c(source, cov, mean, se, hbvresult5)) %>% pivot_wider(names_from = c(hbvresult5_lab), values_from =  Tab1tot)
view(Tab1_num_cc)

agecc <- as.data.frame(t(as.data.frame(survtable("hv105_fromhc1_f")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "hv105_fromhc1_f", 2)[,2], cov = "hv105_fromhc1_f") 
# Sex 0=female, 1=male
sexcc <- as.data.frame(t(as.data.frame(survtable("sex")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "sex", 2)[,2], cov = "sex") 
# relationship to head of household
relhhcc <- as.data.frame(t(as.data.frame(survtable("reltoheadhh_simp")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "reltoheadhh_simp", 2)[,2], cov = "reltoheadhh_simp") 
# urban rural:  hv025=urban(1)/rural(2)
urbrurcc <- as.data.frame(t(as.data.frame(survtable("urbanrural")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "urbanrural", 2)[,2], cov = "urbanrural") 
# type of location hv026: 0=capital (provincial); 1=small city; 2=town; 3=countryside
locationcc <- as.data.frame(t(as.data.frame(survtable("location")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "location", 2)[,2], cov = "location")  
# province
provcc <- as.data.frame(t(as.data.frame(survtable("prov2015")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "prov2015", 2)[,2], cov = "prov2015")  
# household wealth
wealthcc <- as.data.frame(t(as.data.frame(survtable("wealth")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "wealth", 2)[,2], cov = "wealth") 
#pf malaria
pfmalcc <- as.data.frame(t(as.data.frame(survtable("pfmalaria")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "pfmalaria", 2)[,2], cov = "pfmalaria") 
# tetanus - variable from dataset
tetorigcc <- as.data.frame(t(as.data.frame(survtable("tetab")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "tetab", 2)[,2], cov = "tetab") 
anemcc <- as.data.frame(t(as.data.frame(survtable("anemia_f")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "anemia_f", 2)[,2], cov = "anemia_f") 
modstucc <- as.data.frame(t(as.data.frame(survtable("modstunt_f")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "modstunt_f", 2)[,2], cov = "modstunt_f") 
dpt_doscc <- as.data.frame(t(as.data.frame(survtable("dpt_doses_f")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "dpt_doses_f", 2)[,2], cov = "dpt_doses_f") 
injecc <- as.data.frame(t(as.data.frame(survtable("injec_f")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "injec_f", 2)[,2], cov = "injec_f") 
beatcc <- as.data.frame(t(as.data.frame(survtable("beat_f")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "beat_f", 2)[,2], cov = "beat_f") 
reusedcc <- as.data.frame(t(as.data.frame(survtable("v480")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "v480", 2)[,2], cov = "v480") 

exposure <- as.data.frame(t(as.data.frame(survtable("exposed")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "exposed", 2)[,2], cov = "exposed") 
exp_mom <- as.data.frame(t(as.data.frame(survtable("exp_moth")))) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "exp_moth", 2)[,2], cov = "exp_moth") 
#add counts for number of positive adults? npos_ad

all_cc <- dplyr::bind_rows(list(agecc, sexcc, relhhcc, urbrurcc, locationcc, wealthcc, pfmalcc, tetorigcc, anemcc, modstucc, dpt_doscc, injecc, beatcc, reusedcc, provcc, exposure, exp_mom)) %>% 
  filter(!grepl("se\\.", covname) & !grepl("hbvresult5", covname))
view(all_cc)
colnames(all_cc)[2] <- "controls"
colnames(all_cc)[3] <- "cases"
all_cc <- all_cc %>% group_by(cov) %>% mutate(total_case = sum(cases), total_control = sum(controls), totperc_case = 100*(cases / total_case), totperc_ctrl = 100*(controls / total_control))  %>% ungroup()
tab_cc <- all_cc %>% mutate_if(is.numeric, round, 0) %>% mutate(Tab1_cc = paste(cases,' (',totperc_case,')',  sep = ""),Tab1_ctrl = paste(controls,' (',totperc_ctrl,')',  sep = "") )
# drop rows
drop = c("sexFemale", "urbanruralUrban", "pfmalariaPf-negative", "tetabNonreactive", "tetabIndeterminate","anemia_fMild-to-none", "anemia_fNot available", "modstunt_fNo stunting", "modstunt_fNot available", "v4808")
tab_cc2 <- tab_cc %>%
  filter(!covname %in% drop ) %>% 
  select(c("covname","levels", "Tab1_cc", "Tab1_ctrl"))
tab1_cc <- dplyr::bind_rows(list(Tab1_num_cc, tab_cc2)) %>% relocate(c("covname","levels", "Tab1_cc"))

##ExportCC Tab1-----
view(tab1_cc)
write.csv(tab1_cc, file = here("Data", "tab1cc.csv"))

#Table 3-------
svyby(~as.factor(exposed),~hbvresult5 , designf_dhs2cc, svytotal,na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()

ad_hbs <- svyglm(hbvresult5~as.factor(exposed), designf_dhs2cc, family=quasibinomial("logit"))
summary(ad_hbs)
exp(ad_hbs$coefficients)
exp(confint(ad_hbs))

# has mother who is hbsag+
svyby(~as.factor(exp_moth),~hbvresult5 , designf_dhs2cc, svytotal,na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()

ad_hbsm <- svyglm(hbvresult5~as.factor(exp_moth), designf_dhs2cc, family=quasibinomial("logit"))
summary(ad_hbsm)
exp(ad_hbsm$coefficients)
exp(confint(ad_hbsm))

