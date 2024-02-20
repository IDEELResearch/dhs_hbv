# Revised version of 05_famtreesdhs.R

library(tidyverse)
library(writexl)

# new dataset for kids created in 99_missing.R, after starting with kids in selected households (for male interview, these are the hhs with kids with biospecimen), kids eligible (age, consent, de facto resident), kids w sampled, kids with hbv result on sample
# this data is titled elig_kids_whbvres, has 5608 observations, OR elig_kids_2_whbvres which has 6851 observations
# here we need to connect the household members. and create multiple versions for the sensitivity analyses

# indicator variables for case hh status at s/co 5, 1, 2, and 100
# count of positive kids at each s/co cutoff

# START JAN 19 USING elig_kids_2_whbvres

kids_hh <- 
  elig_kids_2_whbvres %>% group_by(cluster_hh) %>% 
  summarise(n_poskids5 = sum(hbvresult5), 
            n_poskids1 = sum(hbvresult1), 
            n_poskids2 = sum(hbvresult2), 
            n_poskids100 = sum(hbvresult100), 
            n_kids = n()) %>% 
  mutate(casehh5 = case_when(
    n_poskids5 > 0 ~ 1, # case household if at least one pos kid
    n_poskids5 == 0 ~ 0, # control household if zero positive children
    TRUE ~ NA_real_),
    casehh1 = case_when(
      n_poskids1 > 0 ~ 1, # case household if at least one pos kid
      n_poskids1 == 0 ~ 0, # control household if zero positive children
      TRUE ~ NA_real_),
    casehh2 = case_when(
      n_poskids2 > 0 ~ 1, # case household if at least one pos kid
      n_poskids2 == 0 ~ 0, # control household if zero positive children
      TRUE ~ NA_real_),
    casehh100 = case_when(
      n_poskids100 > 0 ~ 1, # case household if at least one pos kid
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

# merge counts back onto df with all kids (likely a way to do this in a single step, but this works for now)
elig_kids_2_whbvres <- left_join(elig_kids_2_whbvres, kids_hh[, c("cluster_hh","ccstat5fin", "ccstat1fin","ccstat2fin","ccstat100fin")], by = "cluster_hh")
view(elig_kids_whbvres)

# case-control substudy investigating hbsag prev among household members-----------
# subset kids df 
cc_kids5 <- elig_kids_2_whbvres %>% filter(!is.na(ccstat5fin))
cc_kids1 <- elig_kids_2_whbvres %>% filter(!is.na(ccstat1fin))
cc_kids2 <- elig_kids_2_whbvres %>% filter(!is.na(ccstat2fin))
cc_kids100 <- elig_kids_2_whbvres %>% filter(!is.na(ccstat100fin))

nrow(hhmemb5)
# find correct dataset of adults
view(adults4mrg) # `both` contains kids and adult results
hhmemb5 <- dhsmeta %>% filter(cluster_hh %in% cc_kids5$cluster_hh) %>%  
  select(hv001, hv002, hvidx, hv005, hv006, hv009, hv010, hv011, hv012, hv013, hv014, cluster_hh, latnum, longnum, dbsbarcode, shnprovin, hv105, hv104, hv025, hv026, hv027, hv028, hv040,
         hv201, hv270, hv246, hml1, hml10, hml20, ha64, hb64,
         pfldh_kids, pfldh_adult, pv18s_adult, pv18s, po18s)
table(hhmemb5$hb64, useNA = "always")
table(hhmemb5$ha64, useNA = "always")
class(hhmemb5$ha64)
class(hhmemb5$hb64)

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
    TRUE ~ NA_real_))
table(hhmemb5$eligtesting, hhmemb5$consent_add, useNA = "always")   
hhmemb5 %>% filter(eligtesting==1 & is.na(consent_add)) %>% reframe(dbsbarcode, ha64, hb64)
# hbv on 1/2 instead of 0/1
adults4mrg$hbv_adcorr <- adults4mrg$hbv - 1

hhmemb5 %>% filter(eligtesting==1 & consent_add==1) %>% group_by()
hhmemb5 <- left_join(hhmemb5,adults4mrg[,c("dbsbarcode", "hbv_adcorr", "clus_hh_ind")], by = "dbsbarcode")

table(adultstat = hhmemb5$hbv_adcorr,casecontrol =hhmemb5$ccstat5fin, useNA = "always")

#sum pos counts at hh level
ad_sum_clusthh <- 
  adults4mrg %>% group_by(cluster_hh) %>% 
  summarise(n_posad5 = sum((hbv_adcorr)),
            n_ad_tested = n(),
            prev5_adtested = n_posad5/n_ad_tested)
table(poscount = ad_sum_clusthh$prev5_adtested)
# add pos counts and total tested from hh onto list of household members
hhmemb5 <- left_join(hhmemb5, ad_sum_clusthh, by = "cluster_hh")
# counts by case/control
hhmemb5 <- left_join(hhmemb5, kids_hh[, c("cluster_hh", "n_poskids5","ccstat5fin" )], by = "cluster_hh")
table(hhmemb5$ccstat5fin, useNA = "always")

table(ccstat = hhmemb5$ccstat5fin,hhmemb5$hbv_adcorr, useNA = "always" )
# still seeing 8% prevalence among control households

# missingness more among control hh?
hhmemb5 %>% filter(eligtesting==1 & consent_add==1) %>% group_by(ccstat5fin, hbv_adcorr) %>%  count()
table(hhmemb5$n_posad5, hhmemb5$ccstat5fin)

# hh member status onto kids df
test <- left_join(elig_kids_2_whbvres, ad_sum_clusthh[, c("cluster_hh", "n_posad5","n_ad_tested" )], by = "cluster_hh")
test %>% group_by(ccstat5fin, n_posad5) %>% count()
test <- test %>% mutate(exposed = case_when(n_posad5 >0 ~ 1,
                                            n_posad5==0 ~ 0))
# get 2x2 table from this
nrow(test)
designf_cc <-svydesign(ids=test$hv001, strata=test$hv022 , weights=test$both_wt_new,  data=test)
options(survey.lonely.psu="adjust")
designf_dhs2cc <-as_survey_design(designf_cc)

table(exp = test$n_posad5, outcome = test$hbvresult5)
table(exp = test$exposed, outcome = test$hbvresult5)
as.data.frame(svyby(~hbvresult5,~n_posad5, designf_dhs2cc, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("wealth"))
as.data.frame(svyby(~hbvresult5,~exposed, designf_dhs2cc, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("wealth"))

svyby(~as.factor(exposed),~hbvresult5 , designf_dhs2cc, svytotal,na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()

ad_hbs <- svyglm(hbvresult5~as.factor(exposed), designf_dhs2cc, family=quasibinomial("logit"))
summary(ad_hbs)
exp(ad_hbs$coefficients)
exp(confint(ad_hbs))
