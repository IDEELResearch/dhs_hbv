# look at household clusters as fam trees
library(tidyverse)
#hv112 - mother's line number, 0=mother not in hh
# HV117                  Eligibility for female interview                
# HV118                  Eligibility for male interview                  

# SH322                  Taking cta to treat the malaria    1  Yes, CTA; 2  Yes, other CTA; 3  Yes, other; 4  No
# sh327                  Result code of malaria treatment and referral: 1  Medication given; 2  Meds refused; 3  Severe malaria referral
#                        4  Already taking meds referral; 6  Other
# SHROUG - measles Ab 0=neg, 1=pos, 3=undetermined
# SHOREI - mumps Ab
# SHRUBE - rubella Ab
# SHVARI - varicella/chicken pox
# SHTETA - tetatnus Ab

table(dhsmeta$hv112) 
table(kid_dhs_int$sh322, kid_dhs_int$hbvresultlowna, useNA = "always")
addmargins(table(Measles = kid_dhs_int$shroug, Mumps = kid_dhs_int$shorei, useNA = "always"))
addmargins(table(Rubella = kid_dhs_int$shrube, Mumps = kid_dhs_int$shorei, useNA = "always"))

library(eulerr)
fit <- euler(c(A = kid_dhs_int$shroug, B = kid_dhs_int$shorei, "A&B" = 230))
plot(fit)
library(ggvenn)

vacc <- kid_dhs_int %>% select(c("dbsbarcode","shroug","shorei","shrube","shvari", "shteta","hbvresultlowna"))

venn.diagram(x=list(kid_dhs_int))
vacc %>%
  mutate(across(starts_with("sh"), as.logical)) %>%
  ggplot() +
  geom_venn(aes(A = shroug, B = shorei, C = shrube))


table(dhsmeta$hv112) 
table(dhsmeta$v003)

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


table(kid_dhs_int$sh252c)

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

class(d_k_or$b3)
d_k_or %>% filter(!is.na(h15h)) %>% count(hv105)
dhsmeta$cluster_hh <- paste(dhsmeta$hv001, dhsmeta$hv002,sep = "_")

d_k_or$clus_hh_ind <- paste(d_k_or$v001, d_k_or$v002, d_k_or$v003, sep = "_")

# for hh member file, hvidx not hv003 appears to be the correct individual identifier: https://dhsprogram.com/data/Merging-datasets.cfm
head(kid_dhs_int$hv003)
head(kid_dhs_int$hvidx)
kid_dhs_int$clus_hh_ind <-  paste(kid_dhs_int$hv001, kid_dhs_int$hv002, kid_dhs_int$hvidx, sep = "_")
nrow(kid_dhs_int)
nrow(d_k_or)
# per https://userforum.dhsprogram.com/index.php?t=msg&th=11867&goto=24903&S=Google, also need to account for b16 since neither PR nor KR is a subset of the other
# instructional video https://www.youtube.com/watch?v=SJkJmtgaqBc
addmargins(table(d_k_or$b16, useNA = "always"))
# b16 label variable b16      "Child's line number in household"
# from https://userforum.dhsprogram.com/index.php?t=msg&th=8499&start=0&S=Google

# If you try to merge the KR file with the PR file, there are four reasons why you may appear to lose cases:
#  (1) Children in the KR file will not be in the PR file if they have died (b16=.)
# (2) Children in the KR file will not be in the PR file if they are not living with the mother (b16=0)
# (3) Children in the PR file will not be in the KR file if their mother has died (hv112=.)
# (4) Children in the PR file will not be in the KR file if they are not living with their mother (hv112=0)


# incorporate these to see if any merge (where left off Oct 10)

# then run:

kid_hbv_kr <- left_join(kid_dhs_int,d_k_or, by = "clus_hh_ind" )
addmargins(table(kid_hbv_kr$b8, useNA = "always")) # child often too sick to play

# figure out why the KR data 




