# look at household clusters as fam trees
library(tidyverse)
#hv112 - mother's line number, 0=mother not in hh
# HV117                  Eligibility for female interview                
# HV118                  Eligibility for male interview                  

# SH322                  Taking cta to treat the malaria    1  Yes, CTA; 2  Yes, other CTA; 3  Yes, other; 4  No
# sh327                  Result code of malaria treatment and referral: 1  Medication given; 2  Meds refused; 3  Severe malaria referral
#                        4  Already taking meds referral; 6  Other


# hiv dataset from 01_datacleaning.R
view(drchiv_sel)
# all household members analyzed
adults2023int_hiv_nodrop
# those dropped for contamination at s/co 5
adults2023int_wdrop

# investigate who was positive
addmargins(table(adults2023int_hiv_nodrop$hv112, useNA = "always"))
addmargins(table(adults2023int_hiv_nodrop$hv114))
table(adults2023int_hiv_nodrop$case5final)

# using d_k_or from CDKR61FL.DTA
# subset to those with sample

# birth order of child
addmargins(table(d_k_or$bord, useNA = "always"))
table(d_k_or$b8)
# beating justified if: 
table(d_k_or$v744a)

# 
addmargins(table(d_k_or$v012, useNA = "always"))
addmargins(table(d_k_or$v136, useNA = "always")) # number of hh members
addmargins(table(d_k_or$v137, useNA = "always")) # number of children < 5
addmargins(table(d_k_or$v138, useNA = "always")) # number of eligible women
addmargins(table(d_k_or$v150, useNA = "always")) # relationship to hh head
addmargins(table(d_k_or$s513a, useNA = "always")) #Given one tablet of mebendazole
addmargins(table(d_k_or$s1323, useNA = "always")) # "Anyone (besides partner) ever forced respondent to have intercourse"
addmargins(table(d_k_or$s1324, useNA = "always")) # "Anyone ever forced respondent to have intercourse"
addmargins(table(d_k_or$s1217, useNA = "always")) # child often too sick to play
addmargins(table(d_k_or$b8, useNA = "always")) # child's age
addmargins(table(d_k_or$seligdv, useNA = "always"))

head(d_k_or$caseid, useNA = "always") # "Anyone (besides partner) ever forced respondent to have intercourse"
d_k_or %>% filter(!is.na(h15h)) %>% count(hv105)

# merge KR and PR
# in KR: v001 = cluster, v002 = hh, v003 or b16 is child line number
# in PR: hv001 = cluster, hv002 = hh, hvidx is child line number
d_k_or$clus_hh_ind <- paste(d_k_or$v001, d_k_or$v002, d_k_or$b16, sep = "_")

# for hh member file, hvidx not hv003 appears to be the correct individual identifier: https://dhsprogram.com/data/Merging-datasets.cfm
head(kid_dhs_int$hv003)
head(kid_dhs_int$hvidx)
kid_dhs_int$clus_hh_ind <-  paste(kid_dhs_int$hv001, kid_dhs_int$hv002, kid_dhs_int$hvidx, sep = "_")

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

# variables of interest from KR (1000 vars so should reduce)

d_k_or_red <- d_k_or %>% select(c(clus_hh_ind,v001,v002,b16,midx,v006,v007,v008,v011,v012,v044, v136, v137, v138, v150, m15, m17, m18,
                                   bord, b0, b1, b2, b3, b8, b11, b12, b15, b16, seligdv, s1323, s1324, h3, h5, h7, h10, starts_with("h15"),
                                  v477, v478, v480, v501, v502, v503, v504, v505, v506, v507, v508, v525, v527, v528, v529, v530, v531, v532,
                                  v743f, starts_with("v744"),snprovin, v003, hw51, s1202, s1208, v034)) # s1323, s1324 not available for kids with dbs
table(d_k_or$s114) # ethnicity
#s114 ethnicity
# 11 "Bakongo Nord & Sud" 
# 12 "Bas-Kasai et Kwilu-Kwngo"
# 13 "Cuvette central"
# 14 "Ubangi et Itimbiri"
# 15 "Uele Lac Albert"
# 16 "Basele-K , Man. et Kivu"
# 17 "Kasai, Katanga, Tanganika"
# 18 "Lunda"
# 19 "PygmÈe"
# 96 "Other"

# then run:
kid_hbv_kr <- left_join(kid_dhs_int,d_k_or_red, by = "clus_hh_ind" )
# check hv111 (mother living), hv112 (living with mother)
addmargins(table(kid_hbv_kr$v044, useNA = "always")) # selected domestic violence mod - appears none?

kid_hbv_kr <- kid_hbv_kr %>% mutate(krabout = case_when(
  !is.na(v001) ~ 1, # has data (n=5122)
  hv112==0 & hv111==0 ~ 2, # mother not living (n=107)
  hv112==0 & hv111!=0 ~ 3, #  not living with mother (602 mother living but child not with mother, 3 NaN - group together)
  hv103==0 ~ 4 # #child didn't sleep in hh last night
))
table(kid_hbv_kr$krabout, useNA = "always")

# select those missing
kid_prnokr <- kid_hbv_kr %>% filter(is.na(v001))

addmargins(table(kid_prnokr$hv102,kid_prnokr$hv103, useNA = "always")) 
addmargins(table(kid_prnokr$hv103,useNA = "always")) 

# check cluster, hh, ind, age
kr <- d_k_or_red %>% select(c(v001, v002, b16,  b8,clus_hh_ind, midx,bord,snprovin))
pr_check  <- kid_dhs_int %>% select(c(hv001, hv002,hvidx,hc1,hv105,clus_hh_ind,hc64, hv112))

kid_prnokr_check <- kid_prnokr %>% filter(is.na(krabout)) %>%  select(c(hv001, hv002,hvidx,hc1,hc60,hv105,clus_hh_ind, hv112,hv103,krabout, hc64,shnprovin))
# mother's line number missing or not available on almost all of these: 43/1043 mother is not de facto; 53/1043 mother interview incomplete; 991/1043 missing
table(kid_prnokr_check$hc60, useNA = "always")


#This is achieved by
#1. convert variables v001, v002 and B16 to HV001, HV002 and HVIDX respectively, in the KR file.
#2. sort cases using the key identifier variables HV001, HV002 and HVIDX
#3. open the PR file and sort by HV001, HV002 and HVIDX
#4. select cases based on HVIDX>=2 & HVIDX<=17 & HV103=1 & 0<=HC1<=59
#5. Compute weight variable using HV005/1000000; and weigh the reduced dataset PR

kid_hbv_kr_dis <- kid_hbv_kr %>% distinct(dbsbarcode, .keep_all = TRUE)
# should use this in the below

# merge kids and hh members 
kids4mrg <- kid_hbv_kr %>% select(c(cluster_hh, hv001, hv002, hvidx, dbsbarcode, clus_hh_ind, agegrp, hv105, hv104, hbvresultlowna ))
kids4mrg <- kids4mrg %>% rename(hbv=hbvresultlowna) 
kids4mrg$case5final <- NA_real_
nrow(kids4mrg)
kids4mrg <- kids4mrg %>% distinct(dbsbarcode, .keep_all = TRUE)
nrow(kids4mrg)
# why are 6996 - 6851 = 145 duplicated?
view(adults4mrg)
nrow(adults4mrg)
adults4mrg <- adults2023int_hiv_nodrop %>% select(c(cluster_hh, hv001, hv002, hvidx, dbsbarcode, agegrp, hv105, hv104, case5final, hbvresult))
adults4mrg$clus_hh_ind <- paste(adults4mrg$hv001, adults4mrg$hv002, adults4mrg$hvidx, sep = "_")
adults4mrg <- adults4mrg %>% rename(hbv=hbvresult)
table(adults4mrg$case5final)
table(adults2023int_hiv_nodrop$hv105)

both <- rbind(kids4mrg,adults4mrg)
nrow(both)
both <- both %>% 
  dplyr::mutate(sex=factor(
    both$hv104, 
    levels = c(1, 2),
    labels = c("Male", "Female")))

table(both$case5final, useNA = "always")
table(both$agegrp)

casecontrollist <- adults4mrg %>% select(c(cluster_hh, case5final)) %>% distinct(cluster_hh, .keep_all = T)
nrow(casecontrollist)
table(casecontrollist$case5final)
casecontrollist <- casecontrollist %>% rename(casecontrol5=case5final)

both <- left_join(both, casecontrollist, by="cluster_hh")
view(both)

both %>% filter(agegrp == "kid") %>% count(casecontrol5)

# add case status to kids who were positive (since all hh w kid pos should be cases; need to do the same with controls)
both$casecontrol5 <- ifelse(is.na(both$casecontrol5) & both$agegrp == "kid" & both$hbv==1, 1, both$casecontrol5)

# only makes sense to calc hh prev for case/control hhs, need to assign control status to kid obs
hhsum <- both %>% filter(!is.na(casecontrol5)) %>% 
  dplyr::group_by(cluster_hh) %>%
  dplyr::summarise(totalpositive = sum(as.numeric(hbv), na.rm=TRUE), 
                   n_samp=n(), 
                   hhprev_samp = totalpositive/n_samp, 
                   totadusamp = sum(agegrp =="adult"), 
                   totkidsamp = sum(agegrp =="kid"),
                   totadult_pos = sum(as.numeric(hbv) & agegrp =="adult", na.rm=TRUE),
                   totkid_pos = sum(as.numeric(hbv) & agegrp =="kid", na.rm=TRUE))
view(hhsum)

# adults and kids in case-control hh study 
both_cc <- both %>% filter(!is.na(casecontrol5))
nrow(both_cc)
# merge hh stats onto ind level df
both_cc <- left_join(both_cc, totpos,  by = "cluster_hh")
view(both_cc)
# remove adult-only var for cc status
both_cc <- both_cc %>% select(-c(case5final))
nrow(both_cc)

# add original case/control status
both_cc_orig <- left_join(both_cc, allhhmemb_ccstat, by = "cluster_hh")
view(both_cc_orig)
nrow(both_cc_orig)
table(remerge = both_cc_orig$casecontrol5, staneana = both_cc_orig$cc_staneana, useNA = 'always')

both_cc_orig %>% 
  group_by(casecontrol5, cc_staneana) %>% 
  summarize(count = n())

check <- both_cc_orig %>% filter(is.na(cc_staneana)) %>% reframe(cluster_hh, agegrp, hv105, hbv, casecontrol5, dbsbarcode)

check <- left_join(check, kid_hbv_kr[,c("dbsbarcode","round1sco_1","round1sco_2","round1call", "round2call","hbvresultlowna","hbvresult")], by = "dbsbarcode")
view(check)
check$correct <- ifelse(check$round1call == "REACTIVE",1,NA_real_) # the adult in the same hh as one of the kids should also be assigned case status, but will do merge on cluster_hh of kids
check_reassign <- check %>% filter(agegrp=="kid") # in these 4 hh, can use casecontrol5 not cc_staneana
# now to figure out the others

check2 <- check_reassign %>% filter(is.na(correct))
view(check2)
# other 3 were tested by abbott previously - can use casecontrol5

# now check original case/control list
# list of case/control by cluster_hh
hh <- both_cc_orig %>% 
  distinct(cluster_hh,casecontrol5)
table(hh$casecontrol5)
nrow(hh)

removed <- allhhmemb_ccstat %>% filter(!(cluster_hh %in% hh$cluster_hh))
view(removed)
nrow(removed)
table(removed$cc_staneana)

# who are the kids/IDs that are in the original case/control list made at time of control selection (May 2022) but not in the hh list per kid-adult merge (likely a combo of those removed for contamination and control hh without hh member samples - verify)
d <- kid_hbv_kr %>% filter((cluster_hh %in% removed$cluster_hh)) %>% select(c(cluster_hh, dbsbarcode, hbvresult, hbvresultlowna, round1call, round2call, round1sco_1, round2sco))
nrow(d)
table(d$round1call, d$round2call,useNA = "always")

d2 <- d %>%
  dplyr::group_by(cluster_hh) %>%
  dplyr::summarise(n=n(), totcontam5 = sum(round1call =="REACTIVE"), sibcontam5 = sum(round1call =="NONREACTIVE")) 
view(d2)

# where left off before UW lecture 13 Oct - in d2 above, figure out how many hhs are dropped bc of a contaminated at sco 5 (if totcontam>0). subset those where totcontam5=0 to figure out what going on with sibcontam5 (not atualy siblings of contam sample)
table(d2$totcontam5) # 61 contaminated but other 19 are ???
d3 <- d2 %>% filter(totcontam5==0) 
nrow(d3)

# add data about the kids in the households that seemingly should be included
d4 <- kid_hbv_kr %>% filter(cluster_hh %in% d3$cluster_hh) %>% select(c(cluster_hh, dbsbarcode,agegrp, hbvresult, hbvresultlowna, round1call, round2call, round1sco_1, round2sco))
view(d4)
totpos$cluster_hh
d5 <- left_join(d4, totpos, by = "cluster_hh")
view(d5)
# maybe these hh had no adults with sample so the control status wasn't present? - yes
# n=108 from these 19 hh

a <- dhsmeta %>% filter(cluster_hh %in% d3$cluster_hh) %>% select(c(cluster_hh, hv105, hvidx, hv104,dbsbarcode))
a2 <- left_join(a, adultresults, by = "dbsbarcode")

a2 %>% summarise_all(~ sum(is.na(.)))
nrow(a2)
contr_hh_ad <- a2 %>% group_by(cluster_hh) %>% 
  dplyr::summarise(n=n(), # total adults in these hhs
                   nobarcode = sum(dbsbarcode ==""), # how many hh members weren't selected for DBS sample - 42 adults did not have samples collected
                   nodbsinlab = sum(dbsbarcode != "" & is.na(resultcall)),# dbs was apparently collected but not in ideel lab freezer
                   lowvol = sum(!is.na(resultcall))) # dbs analyzed but sample vol too low
# conclusion from above - the 19 hh should still be controls (just don't have hh members)
view(contr_hh_ad) # further below renames these columns to be able to merge onto hhsum above with summary stats about casecontrol5 hhs

controlhhwnoadultsample <- d3 %>% select(cluster_hh)
controlhhwnoadultsample$casecontrol5 <- 0
view(controlhhwnoadultsample)
# now append onto hh df above
hh <- rbind(hh, controlhhwnoadultsample)
hhcc5_fin <- hh

# save rds and excel version in case - 5 indicates this is the case/control list using S/CO 5 as cutoff - sens analyses will have diff set of cases/controls
write.csv(hhcc5_fin, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/hhcc5_fin")
saveRDS(hhcc5_fin, "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/hhcc5_fin")
hhcc5_fin <- readRDS("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/hhcc5_fin")
view(hhcc5_fin)
# make lists of case/control (only case hhs will be different) using other SCO cutoffs (2, 10)


df_sco5 <- dhsmeta %>% filter(cluster_hh %in% hhcc5_fin$cluster_hh) # or use both_cc_orig instead of dhsmeta
df_sco5 <- left_join(df_sco5, hhcc5_fin, by = "cluster_hh" )
table(df_sco5$casecontrol5) # how many hh members (kids, DBS adults, DBS non adults, others) are in each hh

# need to get adult results
df_sco5 <- left_join(df_sco5, both_cc_orig[, c("dbsbarcode", "hbv","agegrp")], by = "dbsbarcode")
table(df_sco5$hbv,df_sco5$casecontrol5, useNA = "always" )
df_sco5 %>% 
  group_by(casecontrol5, hbv, agegrp) %>% summarise(n=n())

view(df_sco5)
# add on hh size (sampled and unsampled) on main summary dataset about hhs
df_sco5$hv009 <- as.numeric(df_sco5$hv009)

# 4 hh summary datasets to merge: 1) contr_hh_ad (n=19 control households that had no adults with results); 2) hhcc5_fin - n=552 case/control status by cluster_hh; 
#                                 3) hhsum (n=533 for tot, totpos status by adult/kids - need to append n=19); 4) hhsize below which has data on sampled AND UNSampled hh members
hhsize <- df_sco5 %>% group_by(cluster_hh) %>% summarise(hhsize = mean(hv009), notsamp = sum(is.na(hbv))) # already verified that members of same household have the same value for hv009
view(hhsize)
# this has 552 hh, where as summary of infections has 533 (need to add 0s for the 19 hhs )
check19 <- left_join(contr_hh_ad, hhsize, by = "cluster_hh") # hhsize info totally wrong for these 19
view(check19)

check19k <- df_sco5 %>% group_by(cluster_hh) %>% #filter(cluster_hh %in% check19$cluster_hh)   %>% 
   summarise(kidsamp = sum(agegrp=="kid" & !is.na(hbv)), notsamp = sum(is.na(hbv))) # already verified that members of same household have the same value for hv009
view(check19k)

check19k <- dhsmeta %>% filter(cluster_hh %in% check19$cluster_hh) %>% select(cluster_hh, dbsbarcode, hivrecode_barcode,kids_barcode,hv105)
table(check19k$agegrp)

check19k_sum <- check19k %>% group_by(cluster_hh) %>% 
  dplyr::summarise(totkidsamp = sum(kids_barcode !=""), # kids selected for sampling
                   totadusamp = sum(hivrecode_barcode != ""),
                   notsamp = sum(dbsbarcode=="")) # adult selected for sampling

hhsize <- hhsize %>% filter(!(cluster_hh %in% check19k_sum$cluster_hh))
# remove the 19 to add back the correct data
check19k_sum_2 <- check19k_sum %>% select(c(cluster_hh, notsamp, hhsize))
hhsize <- rbind(hhsize, check19k_sum_2) # add back corrected data

colnames(hhsum)
colnames(check19k_sum)
check19k_sum_3 <- check19k_sum %>% select(-c(notsamp, hhsize))
hhsum <- rbind(hhsum, check19k_sum_3)

colnames(hhsize)

# summary: hhsum has "cluster_hh"    "totalpositive" "n_samp"        "hhprev_samp"   "totadusamp"    "totkidsamp"    "totadult_pos"  "totkid_pos"   
# hhsize has "cluster_hh" "hhsize"     "notsamp" 
### final hh summary stats sco5------
hhsum_all <- left_join(hhsum, hhsize, by = "cluster_hh")
hhsum_all <- left_join(hhsum_all, hhcc5_fin, by = "cluster_hh")
table(hhsum_all$totalpositive, hhsum_all$casecontrol5)
# add province
head(df_sco5$latnum)

uniq_prov <- df_sco5 %>% distinct(hv001, cluster_hh, prov2015,geometry,latnum, longnum, hv005, hv026) #hv005 is start of weight variable
view(uniq_prov)
# hv026 Place of residence, 0  Capital, large city, 1  Small city, 2  Town, 3  Countryside
hhsum_all <- merge(hhsum_all, uniq_prov[, c("geometry","hv001","cluster_hh","hv005","latnum","longnum","hv026","prov2015")], by = "cluster_hh", all.x=T)
colnames(hhsum_all)
hhsum_all$hh_weight <- as.numeric(hhsum_all$hv005)/1000000
#weighted prevalence
hhsum_all$wtdprev <- (hhsum_all$hhprev_samp)*(hhsum_all$hh_weight)
view(hhsum_all)

# save copy of these hh stats
write.csv(hhsum_all, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/hhsum_all.csv")
saveRDS(hhsum_all, "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/hhsum_all.rds")
hhsum_all <- readRDS("~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/hhsum_all.rds")
# to return to - adding level between sampling and result to indicate those not tested
view(hhsum_all)
nrow(hhcc5_fin)



#subset households with more than one case--------------------------
anypos <- hhsum_all %>% filter(totalpositive > 0)
table(anypos$casecontrol5)
nrow(anypos)

df_sco5$hv105 <- as.numeric(df_sco5$hv105)
table(df_sco5$hv104, useNA = "always") # 1 is male, 2 is female

# make category for age/sampled/vs not
df_sco5 <- df_sco5 %>% mutate(agegrp = case_when(
  hv105 <=5 & kids_barcode != "" ~ "kid, samp", # sampled kids
  hv105 <=5 & kids_barcode == "" ~ "kid, not samp", # not sampled kids
  hv105 > 5 & hv105 < 15 ~ "teen, not samp",
  hv105 >= 15 & hv105 <= 59 & hv104 == "1" & hivrecode_barcode != "" ~ "adult, samp", # male adults 15-59 sampled
  hv105 >= 15 & hv105 <= 49 & hv104 == "2" & hivrecode_barcode != "" ~ "adult, samp", # female adults 15-49 sampled
  hv105 >= 15 & hv105 <= 59 & hv104 == "1" & hivrecode_barcode == "" ~ "adult, not samp", # male adults 15-59 sampled
  hv105 >= 15 & hv105 <= 49 & hv104 == "2" & hivrecode_barcode == "" ~ "adult, not samp", # female adults 15-49 sampled
  hv105 > 59 & hv104 == "1" ~ "older adult, not samp", # male adults over the sampling age - labeling differently but could also call "adult, samp"
  hv105 > 49 & hv104 == "2" ~ "older adult, not samp" # female adults over the sampling age - labeling differently but could also call "adult, samp"
  ))
table(df_sco5$agegrp, useNA = "always")

anypos_ind <- df_sco5 %>% filter(cluster_hh %in% anypos$cluster_hh)
nrow(anypos) # 156 households with at least one pos (97 case hh, 59 control hh)
nrow(anypos_ind) # 1095 individuals in those 156

df_sco5 %>% 
  group_by(casecontrol5, hbv, agegrp) %>% summarise(n=n()) # the NAs for HBV are hh members interviewed (and on PR recode) but not sampled
# double check distincts
df_sco5_ds <- df_sco5 %>% distinct(dbsbarcode, .keep_all = TRUE)
test <- df_sco5 %>% filter(!is.na(hbv))


# fam tree attempt using line numbers-----------------------
# where left off 13 Oct: add variable on df_sco5 for hhmemb not selected for DBS, DBS not avail at UNC, sample vol too low
# Based on the 2x2 table, who are the adults who are positive? bring in mother's line number
df_sco5 %>% filter(casecontrol5==1 & hbv==1 & agegrp=="adult") %>% #(selecting adults in exposed hh who were positive)
  ggplot()+
  geom_histogram(aes(x=as.numeric(hv105)))

# hv101 is relationship to head of hh (but doesn't give relationship with others, namely the child)

# hw51 is line number of care taker - may need to bring in from KR df
# s1202    "Line number of the youngest child" EMPTY
# s1208    "Line number of the youngest child of 3 or 4 years" EMPTY
# v034     "Line number of husband"
# v003     "Respondent's line number"
# all together: hw51, s1202, s1208, v034, v003

# maybe before merge of KR data, make ID for mother and others
kid_hbv_kr$respond_id <- paste(kid_hbv_kr$v001, kid_hbv_kr$v002, kid_hbv_kr$v003, sep = "_")
kid_hbv_kr$respondhusb_id <- paste(kid_hbv_kr$v001, kid_hbv_kr$v002, kid_hbv_kr$v034, sep = "_")
kid_hbv_kr$caretaker_id <- paste(kid_hbv_kr$v001, kid_hbv_kr$v002, kid_hbv_kr$hw51, sep = "_")

# df_sco5 has the correct (n=3607) number of rows but the leftjoin below 
# origin of dups in kid_hbv_kr -- check and then restrict to non-dups
kid_hbv_kr %>% group_by(dbsbarcode) %>% mutate(duplicated = n() > 1) %>% filter(duplicated==T)  %>%  select(dbsbarcode, cluster_hh, duplicated, clus_hh_ind,respond_id )
kid_hbv_kr_dis <-  kid_hbv_kr %>% distinct(dbsbarcode, .keep_all = TRUE)

# correct merge of KR data onto hh mem data
df_sco5 <- left_join(df_sco5, kid_hbv_kr_dis[,c("dbsbarcode", "v150","b0","b16","hw51", "s1202", "s1208", "v034", "v003","clus_hh_ind","respond_id","respondhusb_id","caretaker_id")], by = "dbsbarcode")

df_sco5 <- left_join(df_sco5, kid_hbv_kr_dis[,c("dbsbarcode", "h3", "h5", "h7","h10", "h15b", "h15c", "h15d", "h15h", "h15i","v525", "v531","v505", "v506", "v744a", "v744b", "v744c", "v744d", "v744e")], by = "dbsbarcode")
# remove duplicated column: df_sco5 <- subset(df_sco5, select = -c(h10.1))

# from PR dataset: 
# HA51                   Line number of parent/caretaker
# SH276                  Child line number in the Household               
# HV114                  Father's line number      
# HV112                  Mother's line number

# new clus_hh_ind for ALL
df_sco5$clus_hh_ind2 <-  paste(df_sco5$hv001, df_sco5$hv002, df_sco5$hvidx, sep = "_")

df_sco5 %>% filter(casecontrol5==1 & !is.na(hbv)) %>%
  select(cluster_hh,hv001,hv002,hvidx, clus_hh_ind, clus_hh_ind2,respond_id, respondhusb_id, caretaker_id, agegrp, hbv, dbsbarcode) %>% 
  head(20)

# check for duplicates - this has been corrected above so none should print. keeping so that code is there for filter/grepl
df_sco5 %>% group_by(dbsbarcode,clus_hh_ind2 ) %>% filter(n() > 1 & dbsbarcode != "" & !grepl("999",dbsbarcode)) %>% 
  select(clus_hh_ind, clus_hh_ind2, agegrp, hbv, dbsbarcode) %>% print(n = Inf)

# add hh level data to this dataset from hhsum to subset hh with >0 pos, to investigate family trees
df_sco5 <- left_join(df_sco5, hhsum_all[, c(1:10)], by = "cluster_hh") # leaving off casecontrol5 since that already on the df

df_sco5 %>% filter(totalpositive > 0) %>%
  select(cluster_hh, agegrp, b16, clus_hh_ind2,respond_id, respondhusb_id, caretaker_id, hv105, hv104, hbv, dbsbarcode) %>% 
  head(50)
df_sco5 %>% filter(totalpositive > 0) %>% group_by(casecontrol5) %>%  summarise(n_dis=n_distinct(cluster_hh))
  
df_sco5 %>% filter(totalpositive > 1) %>% group_by(casecontrol5) %>%  summarise(n_dis=n_distinct(cluster_hh))

# investigate households
options(max.print = 99999)

# labels : relationship to head of hh
df_sco5 <- df_sco5 %>% 
  dplyr::mutate(reltoheadhh=factor(
    df_sco5$hv101, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,98),
    labels = c("Head", "Spouse","Son/daughter","Son/daughter-in-law","Grandchild","Parent","In-laws","Brother/sister","Co-spouse","Other","Adopted/in custody","Not related","Nephew/niece","Nephew/niece by marriage","Don't know")))

df_sco5 <- df_sco5 %>% 
  dplyr::mutate(sex=factor(
    df_sco5$hv104, 
    levels = c(1, 2),
    labels = c("Male", "Female")))
# provinces
df_sco5 <- df_sco5 %>% 
  dplyr::mutate(prov2015=factor(
    df_sco5$shnprovin, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
    labels = c("Kinshasa", "Kwango","Kwilu","Mai-Ndombe","Kongo Central","Equateur","Mongala","Nord-Ubangi","Sud-Ubangi","Tshuapa","Kasai","Kasai-Central","Kasai-Oriental","Lomami","Sankuru","Haut-Katanga","Haut-Lomami","Lualaba","Tanganyka","Maniema","Nord-Kivu","Bas-Uele","Haut-Uele","Ituri","Tshopo","Sud-Kivu")))

# add hiv status (of adults tested)
df_sco5 <- left_join(df_sco5, adults2023int_hiv[, c("dbsbarcode","hiv03" )], by = "dbsbarcode")
nrow(df_sco5)

# focus on clusters with ≥2 infections
#hepbclust <- 
df_sco5 %>% filter(totalpositive > 1) %>%
  select(prov2015, cluster_hh, casecontrol5, hhsize,totalpositive,totadult_pos, totkid_pos, agegrp, hv105, sex, reltoheadhh, b0, clus_hh_ind2,respond_id, respondhusb_id, caretaker_id,  hbv,hiv03, dbsbarcode, h3, h5, h7, h10, h15b, h15c, h15d, h15h, h15i,v525, v531,v505, v506, v744a, v744b, v744c, v744d, v744e) %>% arrange(casecontrol5)
library(writexl)
write_xlsx(hepbclust,  path = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/hepbclust.xlsx")

# deep dive manually in powerpoint


