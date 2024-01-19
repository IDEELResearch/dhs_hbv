# 07_sensitivity analyses
library(tidyverse)
library(srvyr)
library(survey)
options(survey.lonely.psu="adjust")

# dataset to use: kid_hbv_kr_dis
## need to incorporate abbott differences in sensitivity analyses
abbott_sel_kid <- abbott_selected %>% filter(agegrp=="kid") %>% rename(hbvresult_abb = hbvresult)
colnames(abbott_sel_kid)

kid_hbv_kr_dis <- merge(x=kid_hbv_kr_dis, y=abbott_sel_kid[, c("dbsbarcode","hbvresult_abb")], by = "dbsbarcode", all.x=T)
table(ktest = kid_hbv_kr_dis$catresult, abbott = kid_hbv_kr_dis$hbvresult_abb, useNA = "always")

lowvols46 <- kid_hbv_kr_dis %>% filter(catresult=="low vol") %>% select(dbsbarcode, catresult)
set.seed(1027)
selaspos5up <- lowvols46[sample(nrow(lowvols46), 1), ] # make this DBS the one of 46 that is positive for the main findings, then bound
selaspos1 <- rbind(selaspos5up, lowvols46[sample(nrow(lowvols46), 1), ]) # make this DBS the one of 46 that is positive for the main findings, then bound
# should be consistent


# Main combined result variable - using the K testing point------------------------------
kid_hbv_kr_dis <- kid_hbv_kr_dis %>% mutate(
  catresult5 = case_when(
    dbsbarcode %in% selaspos5up$dbsbarcode ~ "reactive", # make the one randomly selected low vol positive
    catresult=="low vol" & !(dbsbarcode %in% selaspos5up$dbsbarcode) ~ "nonreactive", #the 45 low vols NOT selected to be the positive
    TRUE ~ catresult),
  # num indicator var    
  hbvresult5 = case_when(
    catresult5=="reactive" ~ 1,
    catresult5=="nonreactive" ~ 0,
    TRUE ~ NA_real_),
  hbvresultlow5 = case_when( # lower bound
    catresult=="low vol" ~ 0, # make the 46 low vol negative for the lower bound
    hbvresult_abb != hbvresult ~ hbvresult, # for the two samples that disagree between abbott and K, in the low bound use the negative (K)
    !is.na(hbvresult_abb) ~ hbvresult_abb,
    TRUE ~ hbvresult),
  hbvresultlowpos5 = case_when( # upper bound
    catresult=="low vol" ~ 1, # make the 46 low vol positive for the upper bound
    hbvresult_abb != hbvresult ~ hbvresult_abb, # for the two samples that disagree between abbott and K, in the upper bound use the pos (Abb)
    !is.na(hbvresult_abb) ~ hbvresult_abb, 
    TRUE ~ hbvresult) )
table(kid_hbv_kr_dis$catresult5, kid_hbv_kr_dis$hbvresultlow5 ,useNA = "always")
table(kid_hbv_kr_dis$catresult5, kid_hbv_kr_dis$hbvresultlowpos5 ,useNA = "always")

# Sensitivity analyses for cut-offs--------

# variables with hbv results: catresult, hbvresult, hbvresultlowna, hbvresultlowpos, round1call, round2call
# catresult is before abb results added, whereas hbvresult has abbott results
## if using raw catresults, need to add a line for abbott results
table(kid_hbv_kr_dis$catresult1, useNA = "always")

kid_hbv_kr_dis <- kid_hbv_kr_dis %>% 
  mutate(
 # S/CO cutoff of 1
    catresult1 = case_when(
      round1call == "NONREACTIVE" ~ "nonreactive", # nonreactive on round 1 = final result
      round2call == "Reactive" ~ "reactive", #if retested in single tube and has pos round 2 result, this is final call
      round2call == "Nonreactive" ~ "nonreactive", # since tube is gold standard, using this result (3 of 45 mismatch were >100)
      # next: for those not retested, what cutoff used
      round2call=="Not retested" & round1sco_1 >= 1 ~ "reactive", # not retested but first round >=1
      round2call=="searching" & round1sco_1 >= 1 ~ "reactive", # can't find result but first round >1
      round2call=="Not retested" & round1sco_1 < 1 ~ "nonreactive", # not retested but first round <1
      round2call=="searching" & round1sco_1 < 1 ~ "nonreactive", # can't find result but first round <1
      # low vol - 
      dbsbarcode %in% selaspos1$dbsbarcode ~ "reactive", # make the one randomly selected low vol positive
      catresult=="low vol" & !(dbsbarcode %in% selaspos1$dbsbarcode) ~ "nonreactive", #the 45 low vols NOT selected to be the positive
      # abbott
      hbvresult_abb != hbvresult ~ catresult, # for the two samples that disagree between abbott and K, in the low bound use the negative (K)
      TRUE ~ catresult),
# num indicator var    
    hbvresult1 = case_when(
      catresult1=="reactive" ~ 1,
      catresult1=="nonreactive" ~ 0,
      TRUE ~ NA_real_),
# lower bound with missing as neg
    hbvresultlow1 = case_when(
      catresult=="low vol" ~ 0, # when original var with low vol was that
      !is.na(hbvresult_abb) ~ hbvresult1,
      TRUE ~hbvresult1),
#  upper end with missing as pos
    hbvresultlowpos1 = case_when(
      catresult=="low vol" ~ 1,
      !is.na(hbvresult_abb) ~ hbvresult_abb, # make 2 discrepant the abbott value
      TRUE ~hbvresult1),

# S/CO cutoff of 2
    catresult2 = case_when(
            round1call == "NONREACTIVE" ~ "nonreactive", # nonreactive on round 1 = final result
            round2call == "Reactive" ~ "reactive", #if retested in single tube and has pos round 2 result, this is final call
            round2call == "Nonreactive" ~ "nonreactive", # since tube is gold standard, using this result (3 of 45 mismatch were >100)
            # next: important decision: for those not retested, what cutoff used
            round2call=="Not retested" & round1sco_1 >= 2 ~ "reactive", # not retested but first round >=2
            round2call=="searching" & round1sco_1 >= 2 ~ "reactive", # can't find result but first round >2
            round2call=="Not retested" & round1sco_1 < 2 ~ "nonreactive", # not retested but first round <2
            round2call=="searching" & round1sco_1 < 2 ~ "nonreactive", # can't find result but first round <2
            # low vol - 
            dbsbarcode %in% selaspos5up$dbsbarcode ~ "reactive", # make the one randomly selected low vol positive
            catresult=="low vol" & !(dbsbarcode %in% selaspos5up$dbsbarcode) ~ "nonreactive", #the 45 low vols NOT selected to be the positive
            # abbott
            hbvresult_abb != hbvresult ~ catresult, # for the two samples that disagree between abbott and K, in the low bound use the negative (K)
            TRUE ~ catresult),
    # num indicator var    
          hbvresult2 = case_when(
            catresult2=="reactive" ~ 1,
            catresult2=="nonreactive" ~ 0,
            TRUE ~ NA_real_),
    # lower bound with missing as neg
    hbvresultlow2 = case_when(
      catresult=="low vol" ~ 0, # when original var with low vol was that
      !is.na(hbvresult_abb) ~ hbvresult2,
      TRUE ~hbvresult2),
    #  upper end with missing as pos
    hbvresultlowpos2 = case_when(
      catresult=="low vol" ~ 1,
      !is.na(hbvresult_abb) ~ hbvresult_abb, # make 2 discrepant the abbott value
      TRUE ~hbvresult2),

# S/CO cutoff of 100
    catresult100 = case_when(
          round1call == "NONREACTIVE" ~ "nonreactive", # nonreactive on round 1 = final result
          round2call == "Reactive" ~ "reactive", #if retested in single tube and has pos round 2 result, this is final call
          round2call == "Nonreactive" ~ "nonreactive", # since tube is gold standard, using this result (3 of 45 mismatch were >100)
          # next: important decision: for those not retested, what cutoff used
          round2call=="Not retested" & round1sco_1 >= 100 ~ "reactive", # not retested but first round >= 100
          round2call=="searching" & round1sco_1 >= 100 ~ "reactive", # can't find result but first round >100
          round2call=="Not retested" & round1sco_1 < 100 ~ "nonreactive", # not retested but first round <100
          round2call=="searching" & round1sco_1 < 100 ~ "nonreactive", # can't find result but first round <100
          # low vol - 
          dbsbarcode %in% selaspos5up$dbsbarcode ~ "reactive", # make the one randomly selected low vol positive
          catresult=="low vol" & !(dbsbarcode %in% selaspos5up$dbsbarcode) ~ "nonreactive", #the 45 low vols NOT selected to be the positive
          # abbott
          hbvresult_abb != hbvresult ~ catresult, # for the two samples that disagree between abbott and K, in the low bound use the negative (K)
          TRUE ~ catresult),
# num indicator var    
      hbvresult100 = case_when(
          catresult100=="reactive" ~ 1,
          catresult100=="nonreactive" ~ 0,
          TRUE ~ NA_real_),
# lower bound with missing as neg
      hbvresultlow100 = case_when(
        catresult=="low vol" ~ 0, # when original var with low vol was that
        !is.na(hbvresult_abb) ~ hbvresult100,
        TRUE ~hbvresult100),
      #  upper end with missing as pos
      hbvresultlowpos100 = case_when(
        catresult=="low vol" ~ 1,
        !is.na(hbvresult_abb) ~ hbvresult_abb, # make 2 discrepant the abbott value
        TRUE ~hbvresult100))

kid_hbv_kr_dis %>%  count(catresult, catresult1, catresult2, catresult5, catresult100) 
kid_hbv_kr_dis %>%  count(hbvresult1, hbvresult2, hbvresult5, hbvresult100) 
kid_hbv_kr_dis %>%  count(hbvresultlow1, hbvresultlow2, hbvresultlow5, hbvresultlow100) 
kid_hbv_kr_dis %>%  count(hbvresultlowpos1, hbvresultlowpos2, hbvresultlowpos5, hbvresultlowpos100) 

# now run sensitivity analyses of maps, main results, forest plots
# make survey design object
designf <-svydesign(ids=kid_hbv_kr_dis$hv001, strata=kid_hbv_kr_dis$hv022 , weights=kid_hbv_kr_dis$hh_weight,  data=kid_hbv_kr_dis)
options(survey.lonely.psu="adjust")
designf_dhs2 <-as_survey_design(designf)

# S/CO of 1-----------
# same functions as 02_tablesfigures but different outcome variable
# survtable_all (counts for all n in dataset) doesn't change and no need to rerun
# survtable - stratified by outcome, need to create new for different cutoff
survtable1 <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresult1, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}
# survmean_all (mean for continous vars in dataset) - doesn't change and no need to rerun
# survmean - stratified by outcome, need to create new for different cutoff
survmean1 <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresult1, designf_dhs2, svymean, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# continuous data: hhmem_n (number of household members), agenum (age)
survmean1("agenum")
survmean1("hhmem_n")

# categorical data
catvars <- c("catresult5","catresult1", "catresult2", "catresult100","totalkidpos_f","hv104", "hv024", "hv025","hv026","hv270", "pfldh_kids","shtetaindasno","shtetaindasyes","prov2015") 
# resuming weighted counts "hv025","hv270","pfldh_kids", hv105 (1-year age)

# Age
survtable1("hv105") #
# Sex 0=female, 1=male
survtable1("sex") #
# urban rural:  hv025=urban(1)/rural(2)
survtable1("hv025") # 
# type of location hv026: 0=capital (provincial); 1=small city; 2=town; 3=countryside
survtable1("hv026") # 
# province
survtable1("prov2015") # 
# household wealth
survtable1("hv270") #
#pf malaria
survtable1("pfldh_kids") #
# tetanus - variable from dataset
survtable1("shteta")
# indeterminate as no antibodies
survtable1("shtetaindasno")
# indeterminant as yes antibodies
survtable1("shtetaindasyes")
# number of kids positive 
survtable1("totalkidpos_f") # since there are 0 counts by design, need to run outside the function
svyby(~totalkidpos_f,~hbvresult1, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") 

# nutritional status: sevstunt, modstunt, stunt,sevwasting, modwasting, wasting, underweight
# anemia: 1- severe; 2-moderate; 3 -mild; 4- not anemic; NaN - missing
survtable1("hc57")
# stunting: 0-no stunt; 1-moderate; 2-severe; 9-missing
survtable1("stunt")
# wasting: 0-no stunt; 1-moderate; 2-severe; 9-missing
survtable1("wasting")
# underweight
survtable1("underweight")

  
#S/CO of 2--------------
# same functions as 02_tablesfigures but different outcome variable
# survtable_all (counts for all n in dataset) doesn't change and no need to rerun
# survtable - stratified by outcome, need to create new for different cutoff
survtable2 <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresult2, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}
# survmean_all (mean for continous vars in dataset) - doesn't change and no need to rerun
# survmean - stratified by outcome, need to create new for different cutoff
survmean2 <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresult2, designf_dhs2, svymean, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# continuous data: hhmem_n (number of household members), agenum (age)
survmean2("agenum")
survmean2("hhmem_n")

# categorical data
catvars <- c("catresult5","catresult1", "catresult2", "catresult100","totalkidpos_f","hv104", "hv024", "hv025","hv026","hv270", "pfldh_kids","shtetaindasno","shtetaindasyes","prov2015") 
# resuming weighted counts "hv025","hv270","pfldh_kids", hv105 (1-year age)

# Age
survtable2("hv105") #
# Sex 0=female, 1=male
survtable2("sex") #
# urban rural:  hv025=urban(1)/rural(2)
survtable2("hv025") # 
# type of location hv026: 0=capital (provincial); 1=small city; 2=town; 3=countryside
survtable2("hv026") # 
# province
survtable2("prov2015") # 
# household wealth
survtable2("hv270") #
#pf malaria
survtable2("pfldh_kids") #
# tetanus - variable from dataset
survtable2("shteta")
# indeterminate as no antibodies
survtable2("shtetaindasno")
# indeterminant as yes antibodies
survtable2("shtetaindasyes")
# number of kids positive 
survtable2("totalkidpos_f") # since there are 0 counts by design, need to run outside the function
svyby(~totalkidpos_f,~hbvresult2, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") 

# nutritional status: sevstunt, modstunt, stunt,sevwasting, modwasting, wasting, underweight
# anemia: 1- severe; 2-moderate; 3 -mild; 4- not anemic; NaN - missing
survtable2("hc57")
# stunting: 0-no stunt; 1-moderate; 2-severe; 9-missing
survtable2("stunt")
# wasting: 0-no stunt; 1-moderate; 2-severe; 9-missing
survtable2("wasting")
# underweight
survtable2("underweight")

# S/CO 100----------------
# same functions as 02_tablesfigures but different outcome variable
# survtable_all (counts for all n in dataset) doesn't change and no need to rerun
# survtable - stratified by outcome, need to create new for different cutoff
survtable100 <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresult100, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}
# survmean_all (mean for continous vars in dataset) - doesn't change and no need to rerun
# survmean - stratified by outcome, need to create new for different cutoff
survmean100 <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresult100, designf_dhs2, svymean, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# continuous data: hhmem_n (number of household members), agenum (age)
survmean100("agenum")
survmean100("hhmem_n")

# categorical data
catvars <- c("catresult5","catresult1", "catresult2", "catresult100","totalkidpos_f","hv104", "hv024", "hv025","hv026","hv270", "pfldh_kids","shtetaindasno","shtetaindasyes","prov2015") 
# resuming weighted counts "hv025","hv270","pfldh_kids", hv105 (1-year age)

# Age
survtable100("hv105") #
# Sex 0=female, 1=male
survtable100("sex") #
# urban rural:  hv025=urban(1)/rural(2)
survtable100("hv025") # 
# type of location hv026: 0=capital (provincial); 1=small city; 2=town; 3=countryside
survtable100("hv026") # 
# province
survtable100("prov2015") # 
# household wealth
survtable100("hv270") #
#pf malaria
survtable100("pfldh_kids") #
# tetanus - variable from dataset
survtable100("shteta")
# indeterminate as no antibodies
survtable100("shtetaindasno")
# indeterminant as yes antibodies
survtable100("shtetaindasyes")
# number of kids positive 
survtable100("totalkidpos_f") # since there are 0 counts by design, need to run outside the function
svyby(~totalkidpos_f,~hbvresult100, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") 

# nutritional status: sevstunt, modstunt, stunt,sevwasting, modwasting, wasting, underweight
# anemia: 1- severe; 2-moderate; 3 -mild; 4- not anemic; NaN - missing
survtable100("hc57")
# stunting: 0-no stunt; 1-moderate; 2-severe; 9-missing
survtable100("stunt")
# wasting: 0-no stunt; 1-moderate; 2-severe; 9-missing
survtable100("wasting")
# underweight
survtable100("underweight")


