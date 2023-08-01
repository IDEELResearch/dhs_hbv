# load packages
library(readxl)
library(tidyverse)


#read.dta() is in the package "foreign", so you will need
library(foreign)
drchiv <- read.dta("/Users/camillem/Documents/GitHub/dhs_hbv/Data/CDAR61DT/CDAR61FL.DTA", convert.factors = FALSE)
# /Users/camillem/Documents/GitHub/dhs_hbv/Data/CDAR61DT

# load data
dhsmeta <- readRDS("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/DHS_pr_full_merge_backup.rds")

adultresults <- read_excel("results.xlsx", sheet = "Results") # reimport

kidresults <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/Epic Tracking 2022_jan2023.xlsx", 
                         sheet = "allclean")
# abbott results from previous testing
abbott <- readRDS("abbott.rds")
# from : abbott <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/Abbott_HBVresults_rnd2.xls")

# hh member tested by abbott yes/no (no results)
abbyesno <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/testedbyabbott.xlsx", 
                         sheet = "Sheet1")
#original case/control status
allhhmemb_ccstat <- read_excel("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/allhhmemtotest_check.xlsx", 
                       sheet = "import")

# clean up hbv lab results datasets before merging------------------
# adults:
# get format in same lowercase
adultresults$dbsbarcode <- tolower(adultresults$Barcodes)

# adults
adultresults$catresult <- tolower(adultresults$resultcall)
adultresults$agegrp <- "adult" #add indicator for kid vs adult


# kids:
# create all lowercase version of IDs for merging with metadata
kidresults$dbsbarcode <- tolower(kidresults$ID1)
#add indicator for kid vs adult
kidresults$agegrp <- "kid" 

# DECIDE S/CO cutoff--------------------------------------------------------------------------------
# cross tab of first and second round results 
table(kidresults$round1call,kidresults$round2call, useNA = "always")

# S/CO distribution for those reactive in first round and nonreactive in second
kidresults %>% filter(round1call=="REACTIVE" & round2call=="Nonreactive") %>% reframe(round1sco_1, round1sco_2, round2sco) %>% print(n=Inf)
# summary: 45 mismatches, 3 were over 100, 1 between 10-100, 1 between 5-10, the rest (n=40) below 5

# distribution of those not retested:
kidresults %>% filter(round2call=="Not retested" | round2call=="searching") %>% reframe(round1sco_1, round1sco_2, round2sco) %>% print(n=Inf)

kidresults <- kidresults %>% 
  mutate(noretestdist = case_when(
  round1sco_1 > 100 ~ ">100",
  round1sco_1 < 100 & round1sco_1 > 10 ~ "10-100",
  round1sco_1 <10 & round1sco_1 > 5 ~ "5-10",
  round1sco_1 < 5 ~ "<5",
  TRUE ~ NA_character_))

kidresults %>% filter(round2call=="Not retested" | round2call=="searching") %>% count(noretestdist)

# use cut off of 5 for those that couldn't be retested - can perform sensitivity analyses
table(kidresults$round1call, useNA = "always")

table(kidresults$round2call, useNA = "always")
table(kidresults$round1sco_1, useNA = "always")


kidresults <- kidresults %>% 
  mutate(catresult = case_when(
    round1call == "NONREACTIVE" ~ "nonreactive", # nonreactive on round 1 = final result
    round2call == "Reactive" ~ "reactive", #if retested in single tube and has pos round 2 result, this is final call
    round2call == "Nonreactive" ~ "nonreactive", # since tube is gold standard, using this result (3 of 45 mismatch were >100)
    # next: important decision: for those not retested, what cutoff used
    round2call=="Not retested" & round1sco_1 >= 5 ~ "reactive", # not retested but first round >=5
    round2call=="searching" & round1sco_1 >= 5 ~ "reactive", # can't find result but first round >5 (n=6)
    round2call=="Not retested" & round1sco_1 < 5 ~ "nonreactive", # not retested but first round <5
    round2call=="searching" & round1sco_1 < 5 ~ "nonreactive", # can't find result but first round <5
    # low vol - set to missing
    round1call == "low volume" ~ "low vol", # n=47
    round1call == "missing" ~ NA_character_, # n=1
    TRUE ~ NA_character_))

# check new variable
#table(kidresults$hbvresult, useNA = "always")
addmargins(table(kidresults$catresult, useNA = "always"))
table(kidresults$catresult, kidresults$round1call,useNA = "always")
table(kidresults$catresult, kidresults$round2call,useNA = "always")


# strip lab results to bare minimum
adults_trim <- adultresults[,c("dbsbarcode", "catresult", "agegrp")]
kids_trim <- kidresults[,c("dbsbarcode", "catresult", "agegrp")]


# merge hbv results back onto metadata----------------------------------------------------------------
# hh identifier
dhsmeta$cluster_hh <- paste(dhsmeta$hv001, dhsmeta$hv002,sep = "_")

# make single variable with barcodes
dhsmeta <- dhsmeta %>% 
  mutate_all(as.character) %>% 
  mutate(dbsbarcode = ifelse(kids_barcode=="", hivrecode_barcode, kids_barcode)) 


# merge adults and kids - wait until calls are made on SCO to use
k08testing <- rbind(kids_trim, adults_trim)

k08testing$k08orabb <- "K08 testing"
  
k08testing <- k08testing %>% mutate(
  hbvresult = case_when(
    catresult=="reactive" ~ 1,
    catresult=="nonreactive" ~ 0,
    TRUE ~ NA_real_))

# merge abbott results onto k08 results
# for abbott results: want all kid results, and only the adults that were case/control household members-use abbyesno for list of those selected
abbott$k08orabb <- "Abbott"
# make barcodes lowercase in abbott results dataset
abbott$dbsbarcode <- tolower(abbott$barcodes)
# make barcodes lowercase in abbott indicator dataset
abbyesno$dbsbarcode <- tolower(abbyesno$barcodes)

# select abbot results only for kids or for those who were selected for sampling
abbott_selected <- abbott %>% filter(KidSample==1 | dbsbarcode %in% abbyesno$dbsbarcode)
# 277 kids + 54 adults = correct

abbott_selected <- abbott_selected %>% mutate(
  agegrp = case_when(
    KidSample== 1 ~ "kid",
    KidSample== 0 ~ "adult",
    TRUE ~ NA_character_),
  catresult = case_when(
    HBV_pos==1 ~ "reactive",
    HBV_pos==0 ~ "nonreactive",
    TRUE ~ NA_character_
  ))
table(abbott_selected$agegrp, abbott_selected$KidSample)
table(abbott_selected$catresult, abbott_selected$HBV_pos)

abbott_selected$hbvresult <- abbott_selected$HBV_pos

# abbott trim for merging
abbott_trim <- abbott_selected[,c("dbsbarcode", "catresult", "agegrp", "hbvresult", "k08orabb")]
nrow(abbott_trim)
# merge abbott results only k08 testing results - only abbott results for kids or hh members selected
allhbvtesting <- rbind(k08testing, abbott_trim)
table(allhbvtesting$k08orabb, useNA = "always")

table(allhbvtesting$agegrp, useNA = "always")

allhbvtest <- allhbvtesting %>% 
  distinct_at(vars(dbsbarcode), .keep_all = TRUE) # remove duplicates
# check merge
allhbvtesting %>% group_by(agegrp) %>% count()
allhbvtesting %>% group_by(agegrp,catresult ) %>% count()

# need to identify household members by case/control status
# abbyesno has indicator for this 
allhbvtesting <- merge(allhbvtesting, abbyesno[,c("dbsbarcode", "case")] , all.x = T, by="dbsbarcode")
# case/control added for hh members but need to add for control kids

# merge
k08_all <- merge(dhsmeta, allhbvtesting, by = "dbsbarcode")
k08_nomiss <- k08_all %>% filter(catresult=="reactive" |catresult=="nonreactive" )

# which not merging - all kids since adults were selected based on who merged and sample could be found
nomerged <- allhbvtesting %>% filter(!(dbsbarcode %in% k08_all$dbsbarcode))

# merge original kid sco values on main dataset
k08_nomiss <- merge(k08_nomiss, kidresults[,c("dbsbarcode","plateposition","round1sco_1","round2sco","round1call","round2call")], all.x = T, by="dbsbarcode")
k08_nomiss$catresult5 <- k08_nomiss$catresult # in prep for sensitivity analyses with different cutoffs, add 5 to indicate the cat result call is using S/Co of 5 for those not retested
# merge sco onto main dataset
k08_nomiss <- merge(k08_nomiss, kidresults[,c("dbsbarcode","plateposition","round1sco_1","round2sco","round1call","round2call")], all.x = T, by="dbsbarcode")

# need to add case/control status of kids
k08_nomiss %>% group_by(case) %>%  count()

# kids who are reactive - cases
# kids whose samples we are calling contaminated (their hh were selected as case hhs but need to drop them)
# kids who were nonreactive and selected as controls - connect by hhmem case = 0 , make all cluster_hh same value
table(k08_nomiss$case)

control_dhs_merge$case_orig <- control_dhs_merge$case

controlhh <- control_dhs_merge[,c("cluster_hh", "case_orig")]
controlhh <- controlhh %>% 
  distinct_at(vars(cluster_hh), .keep_all = TRUE) # remove duplicates

dhs_hbv_allhh$case_orig <- dhs_hbv_allhh$case
casehh <- dhs_hbv_allhh[,c("cluster_hh", "case_orig")]
casehh <- casehh %>% 
  distinct_at(vars(cluster_hh), .keep_all = TRUE) # remove duplicates

casecontrollist <- rbind(casehh, controlhh) # from selection (does not include box 18 or abbott)


# trying another way---USE THIS WAY; decide how much of above to keep
kidcc <- merge(kidresults, dhsmeta[,c("dbsbarcode","cluster_hh")], by="dbsbarcode",all.x = T)
nrow(kidcc)
extra <- kidcc %>% filter(!(dbsbarcode %in% kidresults$dbsbarcode ))
extra <- kidcc %>% 
  distinct_at(vars(dbsbarcode), .keep_all = TRUE) # remove duplicates

extra2 <- kidcc[duplicated(kidcc$dbsbarcode),]
extra2_all <- kidcc %>% filter(dbsbarcode %in% extra2$dbsbarcode)
selected <- k08_nomiss_check %>% filter(cluster_hh %in% extra2_all$cluster_hh)

kidcc$catresult5 <- kidcc$catresult

# cases by new definition
kidcc_5 <- kidcc %>% group_by(cluster_hh) %>% 
  summarize(ct_react5 = sum(catresult5 == 'reactive'))

kidcc_5$case5 <- ifelse(kidcc_5$ct_react5 >0,1,0)
  
table(kidcc_5$case5)
table(kidcc_5$ct_react5, useNA = "always")

# kidcc is at household level (92 pos are unique hh, not pos kids)

# original - some NAs, need to figure this out
kidcc_orig <- kidcc %>% group_by(cluster_hh) %>% 
  summarize(ct_react_orig = sum(round1sco_1 >= 1))
kidcc_orig$case_orig <- ifelse(kidcc_orig$ct_react_orig >0,1,0)


table(kidcc_orig$ct_react_orig, useNA = "always")
table(kidcc_orig$case_orig, useNA = "always")

# merge case at 5 and case at original
kidcc_ <- merge(kidcc_5, kidcc_orig, by="cluster_hh")

addmargins(table(kidcc_$case5, kidcc_$case_orig, useNA = "always")) # this shows the difference in hh that remain cases versus are dropped
addmargins(table( kidcc_$case_orig, useNA = "always")) # this shows the difference in hh that remain cases versus are dropped

kidcc_$drop <- ifelse(kidcc_$case5==0 & kidcc_$case_orig==1,1,0) # indicator for hh to drop since they weren't in control sampling frame
nrow(kidcc_)
# import case/control status used with stane/ana

table(allhhmemb_ccstat$cc_staneana)
nrow(allhhmemb_ccstat) # t

kidcc_2 <- merge(kidcc_,allhhmemb_ccstat, by="cluster_hh", all.x = T )
kidcc_2 %>% group_by(cc_staneana, case5,case_orig,drop) %>% count()

kidcc_3 <- kidcc_2 %>% mutate(
  case5final = case_when(
    cc_staneana==0 & case5==0 & drop==0 ~ 0, #control hhs
    cc_staneana==0 & case5==0 & is.na(drop) ~ 0, #still control hhs
    cc_staneana==1 & case5==0 & drop==1 ~ 9, #contaminated - drop from analysis=9
    cc_staneana==1 & case5==1 & drop==0 ~ 1, #case hhs
    is.na(cc_staneana) & case5==1 ~ 1, # previously coded this as 2 - checked below to confirm these should be cases
    TRUE ~ NA_real_
  )
)
# check
table(kidcc_3$case5final, useNA = "always")
id <- kidcc_3 %>% filter(case5final==2) %>% summarise(cluster_hh)
getkidids <- merge(id,k08_all[,c("cluster_hh","dbsbarcode","agegrp","hbvresult","k08orabb")], by="cluster_hh") 

# final list of which hhs are cases and controls: kidcc_3, using variable `case5final`
k08_nomiss <- k08_nomiss %>% select(-c("case_orig","case_final"))
k08_nomiss_cc <- merge(k08_nomiss, kidcc_3, by="cluster_hh", all.x = T)

table(k08_nomiss_cc$case5final, k08_nomiss_cc$agegrp,useNA = "always")
table(k08_nomiss_cc$case_orig,useNA = "always")
table(k08_nomiss_cc$case5final, k08_nomiss_cc$agegrp,useNA = "always")
table(k08_nomiss_cc$case, k08_nomiss_cc$agegrp,useNA = "always")


# FINAL DATASET TO USE WITH CORRECT CASE/CONTROL USING 5 S/CO: k08_nomiss_cc
## feb 6 2023: NEED TO ERASE CODE ABOVE

nrow(k08_nomiss)
table(k08_nomiss$agegrp, k08_nomiss$case)
table(k08_nomiss_cc$agegrp, k08_nomiss_cc$case5final, useNA = "always")




# data exploration------------------------------------------------------------------------
# using dataset with no missing (ie no low volume)
k08_nomiss  %>% count(catresult) 
k08_nomiss %>% group_by(agegrp)  %>% count(catresult) 
k08_nomiss %>% group_by(shnprovin)  %>% count(catresult) 

# DRC province groupings
# kinshasa, kongo central, bandundu (driving distance)
# Equateur
# kasais
# Katanga
# orientale
# Kivus
# maniema
class(kid_dhs_int$hv024)

kid_dhs_int <- kid_dhs_int %>% mutate(provgrp = case_when(
  hv024 == "1" | hv024 == "2" | hv024 == "3" ~ 1,  #kinshasa, kongo central, bandundu (driving distance)
  hv024 == "4" ~ 2,  # Equateur
  hv024 == "5" | hv024 == "6" ~ 3,  # kasais
  hv024 == "7" ~ 4,   # Katanga
  hv024 == "8" ~ 5,# orientale
  hv024 == "9" |  hv024 == "11"  ~ 6,# Kivus
  hv024 == "10" ~ 7# maniema
  ),
  provgrp_kin = case_when(
    hv024 == "1"  ~ 1,  #kinshasa,  (capital)
    hv024 == "2" | hv024 == "3" ~ 2,  # kongo central, bandundu (driving distance)
    hv024 == "4" ~ 3,  # Equateur
    hv024 == "5" | hv024 == "6" ~ 4,  # kasais
    hv024 == "7" ~ 5,   # Katanga
    hv024 == "8" ~ 6,# orientale
    hv024 == "9" |  hv024 == "11"  ~ 7,# Kivus
    hv024 == "10" ~ 8# maniema
  ))
table(kid_dhs_int$provgrp, test$hv024)
table(test$provgrp_kin, test$hv024)

k08_nomiss$hv005 <- as.numeric(k08_nomiss$hv005)
k08_nomiss$hh_weight <- k08_nomiss$hv005/1000000
library(survey)
library(srvyr)

designf <-svydesign(ids=k08_nomiss$hv001, strata=k08_nomiss$hv022 , weights=k08_nomiss$hh_weight,  data=k08_nomiss)

options(survey.lonely.psu="adjust")

designf_dhs2 <-as_survey_design(designf)

# basic stats
# overall weighted hbv prevalence
prop.table(svytable(~catresult, designf_dhs2))
svyciprop(~catresult, designf_dhs2, method="lo")




