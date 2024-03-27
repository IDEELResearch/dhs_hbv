# 02_tablesfigures.R

#Load packages----------------
library(tidyverse)
library(tableone)
library(survey)
library(srvyr)
# after 01_datacleaning.R has been compiled, use the data frame _____ for analysis

# TO DO Feb 2024
### 1) connect line number of child mother/father


#Weighted children------------------------
# add weight variable
# kid_dhs_int$hh_weight <- as.numeric(kid_dhs_int$hv005)/1000000
# elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate_at(c('poskids'), as.factor)
# elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate_at(c('v480'), as.factor)

elig_kids_whbvres_wt_kr %>% filter(hbvresult1==1 & hbvresult5==0) %>% group_by(prov2015, hv024, sex) %>% summarise(n=n(), nsum=nrow(.) )
elig_kids_whbvres_wt_kr %>% filter(hbvresult5==1 & hbvresult100==0) %>% group_by(prov2015, hv024, sex) %>% summarise(n=n(), nsum=nrow(.) )
elig_kids_whbvres_wt_kr %>% filter(hbvresult5==1 & hbvresult100==0) %>% group_by(prov2015, hv024, sex) %>% summarise(n=n(), nsum=nrow(.) )
elig_kids_whbvres_wt_kr %>% group_by(hv101) %>% count() %>% print(n=Inf)

# go through 01_datacleaning_rev.R) to obtain elig_kids_whbvres_wt_kr
# make survey design object
# weight variable decisions: hh_weight (frmo hv005), hv028_div, both_wt_new (hv005 + propens score), both_wt_old (hv028 + propens score)
designf <-svydesign(ids=elig_kids_whbvres_wt_kr$hv001, strata=elig_kids_whbvres_wt_kr$hv022 , weights=elig_kids_whbvres_wt_kr$both_wt_new,  data=elig_kids_whbvres_wt_kr)
options(survey.lonely.psu="adjust")
designf_dhs2 <-as_survey_design(designf)

# survey design for households
designhh <-svydesign(ids=elig_kids_whbvres_wt_kr$cluster_hh, strata=elig_kids_whbvres_wt_kr$hv022 , weights=elig_kids_whbvres_wt_kr$hv028_div,  data=elig_kids_whbvres_wt_kr)
designhh2 <-as_survey_design(designhh)
svymean(~hv009,designhh2, na.rm=T, survey.lonely.psu="adjust") #%>% clipr::write_clip()

# which HBV outcome var: main analysis is hbvresult5, hbvresultlow5, hbvresultlowpos5
# sensitivity analyses: 1, 2, 100

# create functions to calculate weighted n------------
# running functions pastes the results to clipboard which you can then copy into excel

# counts for all n in dataset
survtable_all <- function(var){ 
  svytotal(as.formula(paste0('~', var)), designf_dhs2, na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()
}

# counts for n in dataset, stratified by HBV Y or N
survtable <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresult5, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()
}

# mean for continous vars in dataset
survmean_all <- function(var){ 
  svymean(as.formula(paste0('~', var)),designf_dhs2, na.rm=T, survey.lonely.psu="adjust") #%>% clipr::write_clip()
}

# mean for continuos vars in dataset, stratified by HBV Y or N
survmean <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresult5, designf_dhs2, svymean, na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()
}

## cont data, overall----------
## vars: hv105_fromhc1 (age in years from months var),  hv009 (number of household members), hv014 (number of children in hh (de jure))
survmean_all("hv105_fromhc1")
survmean_all("hc1")
survmean_all("hv009") 
age_av <- as.data.frame(survmean_all("hv105_fromhc1")) %>% rownames_to_column(var = "cov")  %>%  
  mutate(levels = "Age, mean (SD)") %>% rename(se = hv105_fromhc1)
hhmem_av <- as.data.frame(survmean_all("hv009")) %>% rownames_to_column(var = "cov")  %>%  
  mutate(levels = "Household members, mean (SD)") %>% rename(se = hv009)
kids_av <- as.data.frame(survmean_all("hv014")) %>% rownames_to_column(var = "cov")  %>%  
  mutate(levels = "Children in household, mean (SD)") %>% rename(se = hv014)
avgs_tot <- dplyr::bind_rows(list(age_av, hhmem_av, kids_av), .id = 'source') %>% mutate(Tab1tot = paste(round(mean,1),' (',round(se,2),')', sep = ""))
Tab1_num_tot <- avgs_tot %>% select(-c(source, mean,se ))

## cont data, by hbv----------
age_av_by <- as.data.frame(survmean("hv105_fromhc1")) %>% rownames_to_column(var = "covname") %>% rename(mean = hv105_fromhc1) %>% mutate(cov = "hv105_fromhc1", levels = "Age, mean (SD)")
hhmem_av_by <- as.data.frame(survmean("hv009")) %>% rownames_to_column(var = "covname") %>% rename(mean = hv009) %>% mutate(cov = "hv009", levels = "Household members, mean (SD)")
kids_av_by <- as.data.frame(survmean("hv014")) %>% rownames_to_column(var = "covname") %>% rename(mean = hv014) %>% mutate(cov = "hv014", levels = "Children in household, mean (SD)")
#combine all by vars
avgs_by_all <- dplyr::bind_rows(list(age_av_by, hhmem_av_by, kids_av_by), .id = 'source') %>% mutate(concat = paste(round(mean,1),' (',round(se,2),')', sep = ""))
# pivot wide to get column for hbv+ and hbv-
Tab1_num_by <- avgs_by_all %>% mutate(hbvresult5_lab = case_when(hbvresult5 == 0 ~ "Tab1hbvneg",
                                                          hbvresult5 == 1 ~ "Tab1hbvpos")) %>% 
  select(-c(source, covname, mean, se, hbvresult5)) %>% pivot_wider(names_from = c(hbvresult5_lab), values_from =  concat)

## combine cont tot and by for numeric vars---------
Tab1_num <- left_join(Tab1_num_by, Tab1_num_tot, by = c("cov", "levels"))
view(Tab1_num)

## cat data, overall----------
# list of cat vars in case useful
# catvars <- c("hbvresult5","hbvresult1", "hbvresult2", "hbvresult100","hv104", "sex", "hc1", "hv105", "hv105_fromhc1_f", 
#             "hv024", "hv025","hv026","hv270", "urbanrural", "location", "wealth", "pfmalaria", "tetab",
#             "pfldh_kids","shtetaindasno","shtetaindasyes","prov2015") 
# Age
age <- as.data.frame(survtable_all("hv105_fromhc1_f")) %>% rownames_to_column(var = "covname") %>%  
      mutate(levels = str_split_fixed(covname, "hv105_fromhc1_f", 2)[,2], cov = "hv105_fromhc1_f") 
# Sex 0=female, 1=male
sex <- as.data.frame(survtable_all("sex")) %>% rownames_to_column(var = "covname") %>% 
      mutate(levels = str_split_fixed(covname, "sex", 2)[,2], cov = "sex") 
# relationship to head of household
relhh <- as.data.frame(survtable_all("reltoheadhh_simp")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "reltoheadhh_simp", 2)[,2], cov = "reltoheadhh_simp") 
# urban rural:  hv025=urban(1)/rural(2)
urbrur <- as.data.frame(survtable_all("urbanrural")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "urbanrural", 2)[,2], cov = "urbanrural") 
# type of location hv026: 0=capital (provincial); 1=small city; 2=town; 3=countryside
location <- as.data.frame(survtable_all("location")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "location", 2)[,2], cov = "location")  
# province
prov <- as.data.frame(survtable_all("prov2015")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "prov2015", 2)[,2], cov = "prov2015")  
# household wealth
wealth <- as.data.frame(survtable_all("wealth")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "wealth", 2)[,2], cov = "wealth") 
#pf malaria
pfmal <- as.data.frame(survtable_all("pfmalaria")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "pfmalaria", 2)[,2], cov = "pfmalaria") 
# tetanus - variable from dataset
tetorig <- as.data.frame(survtable_all("tetab")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "tetab", 2)[,2], cov = "tetab") 
# indeterminate as no antibodies
#tetlower <- as.data.frame(survtable_all("shtetaindasno")) %>% rownames_to_column(var = "covname") %>% 
##  mutate(levels = str_split_fixed(covname, "shtetaindasno", 2)[,2], cov = "shtetaindasno") 
# indeterminant as yes antibodies
#tetupper <- as.data.frame(survtable_all("shtetaindasyes")) %>% rownames_to_column(var = "covname") %>% 
##  mutate(levels = str_split_fixed(covname, "shtetaindasyes", 2)[,2], cov = "shtetaindasyes") 
# number of kids positive 
poskids <- as.data.frame(survtable_all("poskids")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "poskids", 2)[,2], cov = "poskids") 
anem <- as.data.frame(survtable_all("anemia_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "anemia_f", 2)[,2], cov = "anemia_f") 
modstu <- as.data.frame(survtable_all("modstunt_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "modstunt_f", 2)[,2], cov = "modstunt_f") 
dpt_dos <- as.data.frame(survtable_all("dpt_doses_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "dpt_doses_f", 2)[,2], cov = "dpt_doses_f") 
inje <- as.data.frame(survtable_all("injec_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "injec_f", 2)[,2], cov = "injec_f") 
beat <- as.data.frame(survtable_all("beat_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "beat_f", 2)[,2], cov = "beat_f") 
reused <- as.data.frame(survtable_all("v480")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "v480", 2)[,2], cov = "v480") 
view(reused)
reused <- reused %>% group_by(cov) %>% mutate(totperc = 100*(total / sum(total)))  %>% ungroup()


all_tot <- dplyr::bind_rows(list(age, sex, relhh, urbrur, location, prov, wealth, pfmal, tetorig, poskids, anem, modstu, dpt_dos, inje, beat), .id = 'source') #tetlower, tetupper,
all_tot <- all_tot %>% filter(total>0) %>% select(-SE)
all_tot <- all_tot %>% group_by(source) %>% mutate(totperc = 100*(total / sum(total)))  %>% ungroup()
view(all_tot)

## hbv n, by counts------
tothbv <- survtable_all("hbvresult5") 
tothbv <- as.data.frame(tothbv) %>% rownames_to_column(var = "covname") %>% rename(hbv_pos = total) %>% select(-hbvresult5)
tothbvneg <- as.data.frame(tab1 %>% group_by(source) %>% summarise(totneg = sum(hbv_neg))) %>% summarise(hbv_neg = median(totneg))
tot_by <- cbind(tothbv,tothbvneg)

## hbv n, overall------
tot_by$total <- tot_by$hbv_pos + tot_by$hbv_neg 
totn <- tot_by %>% select(-c("hbv_neg", "hbv_pos"))
totn$source <- "0"
totn$totperc <- NA_real_
totn$levels <- "HBsAg-positive"
totn$cov <- totn$covname

## hbv n onto total df------
all_tot <- dplyr::bind_rows(list(totn, all_tot))
view(tab1_simp)

## Simplify Tab1
tab1 <- all_tot %>% mutate_if(is.numeric, round, 0) %>% mutate(Tab1tot = paste(total,' (',totperc,')',  sep = ""))
tab1_simp <- tab1 %>% select(c("covname", "levels", "Tab1tot"))

# Export simplified Table 1------
write.csv(tab1_simp, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/K manuscript/tab1_feb24.csv")

## cat data, by hbv----------
svyciprop(~hbvresult5, designf_dhs2, method = "me",na.rm=T, survey.lonely.psu="adjust")

age_by <- as.data.frame(svyby(~hbvresult5,~hv105_fromhc1_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("hv105_fromhc1_f"))
sex_by <- as.data.frame(svyby(~hbvresult5,~sex, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("sex"))
reltoheadhh_by <- as.data.frame(svyby(~hbvresult5,~reltoheadhh_simp, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("reltoheadhh_simp"))
urbru_by <- as.data.frame(svyby(~hbvresult5,~urbanrural, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("urbanrural"))
location_by <- as.data.frame(svyby(~hbvresult5,~location, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("location"))
prov2015_by <- as.data.frame(svyby(~hbvresult5,~prov2015, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("prov2015"))
wealth_by <- as.data.frame(svyby(~hbvresult5,~wealth, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("wealth"))
pfmal_by <- as.data.frame(svyby(~hbvresult5,~pfmalaria, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("pfmalaria"))
shteta_by <- as.data.frame(svyby(~hbvresult5,~tetab, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("tetab"))
anem_by <- as.data.frame(svyby(~hbvresult5,~anemia_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("anemia_f"))
modstu_by <- as.data.frame(svyby(~hbvresult5,~modstunt_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("modstunt_f"))
dpt_dos_by <- as.data.frame(svyby(~hbvresult5,~dpt_doses_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("dpt_doses_f"))
inje_by <- as.data.frame(svyby(~hbvresult5,~injec_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("injec_f"))
beat_by <- as.data.frame(svyby(~hbvresult5,~beat_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("beat_f"))
view(wealth_by)
# combine
all_by <- dplyr::bind_rows(list(age_by, sex_by, reltoheadhh_by, urbru_by, location_by, prov2015_by, wealth_by, pfmal_by, shteta_by, anem_by, modstu_by,dpt_dos_by, inje_by, beat_by)) %>% 
  filter(!grepl("se\\.", covname) & !grepl("hbvresult5", covname))
write.csv(all_by, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/K manuscript/tab2_prevcis.csv")

# =anemia
svyby(~hbvresult5,~anemia_f, designf_dhs2, svytotal,method = "me",na.rm=T, survey.lonely.psu="adjust")

# covariates by hbv status
age_by <- as.data.frame(t(as.data.frame(survtable("hv105_fromhc1_f")))) %>% rownames_to_column(var = "covname") #%>% select(-one_of("hv105_fromhc1_f"))
sex_by <-  as.data.frame(t(as.data.frame(survtable("sex")))) %>% rownames_to_column(var = "covname")
reltoheadhh_by <-  as.data.frame(t(as.data.frame(survtable("reltoheadhh_simp")))) %>% rownames_to_column(var = "covname")
urbru_by <-  as.data.frame(t(as.data.frame(survtable("urbanrural")))) %>% rownames_to_column(var = "covname")
location_by <-  as.data.frame(t(as.data.frame(survtable("location")))) %>% rownames_to_column(var = "covname")
prov2015_by <-  as.data.frame(t(as.data.frame(survtable("prov2015")))) %>% rownames_to_column(var = "covname")
wealth_by <-  as.data.frame(t(as.data.frame(survtable("wealth")))) %>% rownames_to_column(var = "covname")
pfmal_by <-  as.data.frame(t(as.data.frame(survtable("pfmalaria")))) %>% rownames_to_column(var = "covname")
shteta_by <-  as.data.frame(t(as.data.frame(survtable("tetab")))) %>% rownames_to_column(var = "covname")
anem_by <-  as.data.frame(t(as.data.frame(survtable("anemia_f")))) %>% rownames_to_column(var = "covname")
modstu_by <-  as.data.frame(t(as.data.frame(survtable("modstunt_f")))) %>% rownames_to_column(var = "covname")
dpt_dos_by <-  as.data.frame(t(as.data.frame(survtable("dpt_doses_f")))) %>% rownames_to_column(var = "covname")
inje_by <-  as.data.frame(t(as.data.frame(survtable("injec_f")))) %>% rownames_to_column(var = "covname")
beat_by <-  as.data.frame(t(as.data.frame(survtable("beat_f")))) %>% rownames_to_column(var = "covname")
view(wealth_by)
svyby(~pfmalaria,~hbvresult5, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()

# survtable("poskids") # since there are 0 counts by design, need to run outside the function
poskids_by <- as.data.frame(svyby(~hbvresult5,~poskids, designf_dhs2, svyciprop, na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("poskids"))
view(wealth_by)

## combine cat data----------
all_by <- dplyr::bind_rows(list(age_by, sex_by, reltoheadhh_by, urbru_by, location_by, prov2015_by, wealth_by, pfmal_by, shteta_by, anem_by, modstu_by,dpt_dos_by, inje_by, beat_by)) %>% 
  filter(!grepl("se\\.", covname) & !grepl("hbvresult5", covname))
view(all_by)
colnames(all_by)[2] <- "hbv_neg"
colnames(all_by)[3] <- "hbv_pos"

all_ci <- all_by %>% filter(!is.na(hbvresult5)) %>% select(-c("hbv_pos", "hbv_neg")) %>% 
  mutate(prev100 = round(hbvresult5*100, 1),
         ci_l100 = round(ci_l*100, 1),
         ci_u100 = round(ci_u*100, 1),
         Tab2prevci = paste(prev100,' (',ci_l100,', ', ci_u100,')', sep = ""))

all_ci_s <- all_ci %>% select(c("covname", "Tab2prevci"))

write.csv(all_ci_s, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/K manuscript/tab2prcis.csv")

# all_by <- all_by %>% filter(hbv_neg>0 | hbv_pos>0)

## hbv n onto by df-----
all_by <- dplyr::bind_rows(list(tot_by, all_by))

## Tab2 merg totals and bys-----
tab2 <- left_join(all_by, all_tot, by = c("covname"))
view(tab2)
###col percent
### tab1 <- tab1 %>% group_by(source) %>% mutate(hbv_pos_perc = 100*(hbv_pos/sum(hbv_pos)), hbv_neg_perc = 100*(hbv_neg/sum(hbv_neg))) %>% ungroup()
colnames(tab2)
# row percent - to report in table 2 (prevalence)
tab2_simp <- tab2 %>% mutate(hbv_prev = 100*(hbv_pos/total))  %>% select(c(source,cov, levels, hbv_pos, total, hbv_prev)) %>% mutate_if(is.numeric, round, 1) # or round to 1
view(tab2_simp)

# Export Table 2------
write.csv(tab2_simp, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/K manuscript/tab2_feb24.csv")

##combine counts and CI est----
options(scipen = n)
# this merge works but could break
tab2_comb <- cbind(tab2_simp[tab2_simp$cov!="hbvresult5",], all_by)
#--- correct way to merge, but ignoring for now---
all_by2 <- all_by %>% rename(levels = covname)
tab2_comb <- merge(tab2_simp[tab2_simp$cov!="hbvresult5",], all_by2[], by = "levels")
#---

tab2_comb2 <- tab2_comb %>% mutate(hbv_prev_check = 100*hbvresult5,
                                  ci_l_perc = 100*ci_l,
                                  ci_u_perc = 100*ci_u) %>% 
                          mutate_if(is.numeric, round, 1) %>% 
                          select(c(source,cov, levels, hbv_pos, total, hbv_prev,hbv_prev_check,ci_l_perc,ci_u_perc)) %>% 
  filter(cov != "poskids")
tab2_comb3 <- tab2_comb2 %>% mutate(Tab2prevci = paste(hbv_prev,' (',ci_l_perc,', ', ci_u_perc,')', sep = "")) %>% select(source,cov, levels, hbv_pos, total, Tab2prevci)
write.csv(tab2_comb3, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/K manuscript/tab2comp_feb24.csv")

view(tab2_comb3)







##Anthro diff denom------------
# anemia
table(kid_hbv_kr_dis$hc57,kid_hbv_kr_dis$stunt, useNA = "always" )
anthro_kid_hbv_kr_dis <- kid_hbv_kr_dis %>% filter(!is.nan(hc57))
# make survey design object
designf_a <-svydesign(ids=anthro_kid_hbv_kr_dis$hv001, strata=anthro_kid_hbv_kr_dis$hv022 , weights=anthro_kid_hbv_kr_dis$hh_weight,  data=anthro_kid_hbv_kr_dis)
designf_dhs2a <-as_survey_design(designf_a)
# stunting with different denom
svyby(~hc57,~hbvresult5, designf_dhs2a, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()


# misc checks
# 
table(adults2023int$hv041, adults2023int$hbvresult)
table(adults2023int$hc16, useNA = "always")

# investigate further the two questions on marital status/union - check those with NaN are low age?
table(adults2023int$ha60, adults2023int$hv115, useNA = "always")
table(adults2023int$hb60, adults2023int$hv115, useNA = "always")
table(adults2023int$ha60, adults2023int$hb60, useNA = "always")
table(adults2023int$ha60, useNA = "always")

checkmarital <- adults2023int %>% filter(ha60=="NaN") # ha60 is female reporting and hb60 is male reporting
table(checkmarital$hv105)

table(kid_dhs_int$hv105)
table(kid_dhs_int$ha57)

survtable_all("totalkidpos_f") 
survtable("totalkidpos_f")
svytotal(~totalkidpos_f, designf_dhs2, na.rm=T, survey.lonely.psu="adjust")
svyby(~totalkidpos_f,~hbvresultlowna, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust")

table(kid_dhs_int$sh318f, useNA = "always")
survtable_all("sh318f") # 1 male, 2 female
survtable("sh318f")
svyby(~sh318f,~hbvresultlowna, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust")
table(kid_dhs_int$sh318f,kid_dhs_int$hbvresult, useNA = "always")

table(kid_dhs_int$sh318g,kid_dhs_int$hbvresult, useNA = "always")
survtable_all("sh318g") # 1 male, 2 female
survtable("sh318g")
svyby(~hbvresult,~sh318g, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust")

table(kid_dhs_int$shnprovin, kid_dhs_int$hbvresult,kid_dhs_int$hv104 ,useNA = "always")

# province
survtable_all("shnprovin") #
survtable("shnprovin") # not working - too small of counts?
table(kid_dhs_int$shnprovin, kid_dhs_int$hbvresultlowna)%>% clipr::write_clip()
table(kid_dhs_int$hv024, kid_dhs_int$hbvresultlowna)%>% clipr::write_clip()
svyby(~shnprovin,~hbvresultlowna, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust",covmat=TRUE) # %>% clipr::write_clip()

svytable(~ shnprovin + hbvresultlowna, designf_dhs2) %>% clipr::write_clip()

#original results (Abbott and pre contam)
table(kid_dhs_int$round1call, useNA = "always")
kid_dhs_int$originalcall <- ifelse(kid_dhs_int$k08orabb=="Abbott",kid_dhs_int$hbvresultlowna,kid_dhs_int$round1call)

kid_dhs_int <- kid_dhs_int %>% mutate(originalcallclean = case_when(
  originalcall==0 ~ 0,
  originalcall==1 ~ 1,
  originalcall=="low volume" ~ 0,
  originalcall=="NONREACTIVE" ~ 0,
  originalcall=="REACTIVE" ~ 1,
))
addmargins(table(kid_dhs_int$originalcall, kid_dhs_int$originalcallclean))
table(kid_dhs_int$shnprovin, kid_dhs_int$originalcallclean)%>% clipr::write_clip()


table(kid_dhs_int$hbvresult)
pos <- kid_dhs_int %>% filter(hbvresult==1)
dfkid <- pos[!duplicated(pos$cluster_hh),]
table(kid_dhs_int$cluster_hh)

#ADULTS ------------------------------
adults2023int <- k08_nomiss_cc %>% filter(agegrp =="adult") #don't use k08_nomiss - doesn't have updated case/control status based on s/co 5
table(adults2023int$case5final)
table(adults2023int$case)
table(adults2023int$case_orig, useNA = "always")
#clean vars for table 1
#make age numeric
class(adults2023int$hv105)
adults2023int$agenum <- as.numeric(adults2023int$hv105)

# make hh size numeric
class(adults2023int$hv009)
adults2023int$hhmem_n <- as.numeric(adults2023int$hv009)

# add var created above for number of children positive (should only be for adults in case hh)
adults2023int <- left_join(adults2023int, totalpos, by="cluster_hh")
adults2023int %>% count(totalkidpos,catresult,case)
# analyze as categorical since median would be 0
adults2023int$totalkidpos_f <- as.character(adults2023int$totalkidpos)

table(adults2023int$totalkidpos_f, adults2023int$case5final)

#add HIV
drchiv$hiv01 <- tolower(drchiv$hiv01)
drchiv_sel <- drchiv %>% filter(hiv01 %in% adults2023int$dbsbarcode)
drchiv_sel$dbsbarcode <- drchiv_sel$hiv01
adults2023int_hiv <- left_join(adults2023int,drchiv_sel, by="dbsbarcode")

table(adults2023int_hiv$hiv03, adults2023int_hiv$hbvresult, useNA = "always")
table(adults2023int_hiv$shnprovin, adults2023int_hiv$hiv03,useNA = "always")

drchiv$hh_weight <- drchiv$hiv05/1000000

adults2023int_hiv$cluster_hh_2 <- paste(adults2023int_hiv$hivclust, adults2023int_hiv$hivnumb,sep = "_")
adults2023int_hiv %>% head(cluster_hh, cluster_hh_2)


adults2023int_hiv <- adults2023int_hiv %>% 
  dplyr::mutate(prov2015=factor(
    adults2023int_hiv$shnprovin, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
    labels = c("Kinshasa", "Kwango","Kwilu","Mai-Ndombe","Kongo Central","Equateur","Mongala","Nord-Ubangi","Sud-Ubangi","Tshuapa","Kasai","Kasai-Central","Kasai-Oriental","Lomami","Sankuru","Haut-Katanga","Haut-Lomami","Lualaba","Tanganyka","Maniema","Nord-Kivu","Bas-Uele","Haut-Uele","Ituri","Tshopo","Sud-Kivu")))

table(adults2023int_hiv$case5final)


# Weighted
adults2023int_hiv$hh_weight <- as.numeric(adults2023int_hiv$hv005)/1000000
adults2023int_hiv$hbvresult <- as.factor(adults2023int_hiv$hbvresult)
adults2023int_hiv$hiv03 <- as.factor(adults2023int_hiv$hiv03)

adults2023int_hiv$case5final <- as.numeric(adults2023int_hiv$case5final)

adults2023int_hiv <- adults2023int_hiv %>% 
  dplyr::mutate(reltoheadhh=factor(
    adults2023int_hiv$hv101, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,98),
    labels = c("Head", "Spouse","Son/daughter","Son/daughter-in-law","Grandchild","Parent","In-laws","Brother/sister","Co-spouse","Other","Adopted/in custody","Not related","Nephew/niece","Nephew/niece by marriage","Don't know")))

library(survey)
library(srvyr)

# create factor var for certain vars
adults2023int_hiv <- adults2023int_hiv %>% 
  dplyr::mutate(reltoheadhh=factor(
    adults2023int_hiv$hv101, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,98),
    labels = c("Head", "Spouse","Son/daughter","Son/daughter-in-law","Grandchild","Parent","In-laws","Brother/sister","Co-spouse","Other","Adopted/in custody","Not related","Nephew/niece","Nephew/niece by marriage","Don't know")))
table(adults2023int_hiv$reltoheadhh, useNA = 'always')
table(kid_hbv_kr_dis$hv101)

# simple reltoheadhh
adults2023int_hiv <- adults2023int_hiv %>% mutate(reltoheadhh_simp = case_when(
  reltoheadhh == "Head" ~ "Head",
  reltoheadhh == "Spouse" ~ "Spouse",
  reltoheadhh == "Son/daughter" ~ "Son/daughter",
  reltoheadhh == "Son/daughter-in-law" | reltoheadhh == "In-laws" | reltoheadhh == "Nephew/niece by marriage" ~ "In-law fam",
  reltoheadhh == "Brother/sister" | reltoheadhh == "Parent" ~ "Parent/sib",
  reltoheadhh == "Other" | reltoheadhh == "Nephew/niece"  | reltoheadhh == "Adopted/in custody" | reltoheadhh == "Not related" | reltoheadhh == "Grandchild" ~ "Other"
))

adults2023int_hiv <- adults2023int_hiv %>% 
  dplyr::mutate(prov2015=factor(
    adults2023int_hiv$shnprovin, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
    labels = c("Kinshasa", "Kwango","Kwilu","Mai-Ndombe","Kongo Central","Equateur","Mongala","Nord-Ubangi","Sud-Ubangi","Tshuapa","Kasai","Kasai-Central","Kasai-Oriental","Lomami","Sankuru","Haut-Katanga","Haut-Lomami","Lualaba","Tanganyka","Maniema","Nord-Kivu","Bas-Uele","Haut-Uele","Ituri","Tshopo","Sud-Kivu")))

adults2023int_hiv <- adults2023int_hiv %>% mutate(provgrp = case_when(
  hv024 == "1" | hv024 == "2" | hv024 == "3" ~ 1,  #kinshasa, kongo central, bandundu (driving distance)
  hv024 == "4" ~ 2,  # Equateur
  hv024 == "5" | hv024 == "6" ~ 3,  # kasais
  hv024 == "7" ~ 4,   # Katanga
  hv024 == "8" ~ 5,# orientale
  hv024 == "9" |  hv024 == "11"  ~ 6,# Kivus
  hv024 == "10" ~ 7# maniema
) %>% as.character(),
provgrp_kin = case_when(
  hv024 == "1"  ~ 1,  #kinshasa,  (capital)
  hv024 == "2" | hv024 == "3" ~ 2,  # kongo central, bandundu (driving distance)
  hv024 == "4" ~ 3,  # Equateur
  hv024 == "5" | hv024 == "6" ~ 4,  # kasais
  hv024 == "7" ~ 5,   # Katanga
  hv024 == "8" ~ 6,# orientale
  hv024 == "9" |  hv024 == "11"  ~ 7,# Kivus
  hv024 == "10" ~ 8# maniema
)%>% as.character())

adults2023int_hiv <- adults2023int_hiv %>% 
  dplyr::mutate(sex=factor(
    adults2023int_hiv$hv104, 
    levels = c(1, 2),
    labels = c("Male", "Female")))

adults2023int_hiv$hv105 <- as.numeric(adults2023int_hiv$hv105)

# subset to those as case/controls and remove those being dropped bc of s/co5
ad_5f <- adults2023int_hiv %>% filter(case5final < 3)
nrow(adults2023int_hiv)
table(ad_5f$totalkidpos_f, ad_5f$catresult)

# exposed and unexposed household members
ad_5f_exp <- ad_5f %>% filter(case5final==1) 
nrow(ad_5f_exp)
ad_5f_unexp <- ad_5f %>% filter(case5final==0) 
nrow(ad_5f_unexp)

# make survey design object
designf_ad <-svydesign(ids=ad_5f$hv001, strata=ad_5f$hv022 , weights=ad_5f$hh_weight,  data=ad_5f)
options(survey.lonely.psu="adjust")
designf_dhs2_ad <-as_survey_design(designf_ad)

# basic stats
# overall weighted hbv prevalence among children with results
prop.table(svytable(~hbvresult, designf_dhs2_ad))
svyciprop(~hbvresult,  designf_dhs2_ad, method="lo")

# which to have as columns - exposure status ("case")
table(outcome=ad_5f$hbvresult, exposure=ad_5f$case5final)
# create functions to calculate weighted n
# running functions pastes the results to clipboard which you can then copy into excel

# counts for all n in dataset
survtable_all_ad <- function(var){ 
  svytotal(as.formula(paste0('~', var)), designf_dhs2_ad, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# counts for n in dataset, stratified by exposure
survtable_ad_exp <- function(var){ 
  svyby(as.formula(paste0('~', var)),~ case5final, designf_dhs2_ad, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# counts for n in dataset, stratified by outcome and exposure
survtable_ad_both <- function(var){ 
  svyby(as.formula(paste0('~', var)),~catresult + case5final, designf_dhs2_ad, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# mean for continous vars in dataset
survmean_all_ad <- function(var){ 
  svymean(as.formula(paste0('~', var)),designf_dhs2_ad, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# mean for continuos vars in dataset, stratified by HBV Y or N
survmean_ad <- function(var){ 
  svyby(as.formula(paste0('~', var)),~case5final, designf_dhs2_ad, svymean, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# mean for continuos vars in dataset, stratified by HBV Y or N
survmean_ad_both <- function(var){ 
  svyby(as.formula(paste0('~', var)),~ catresult + case5final, designf_dhs2_ad, svymean, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}
# get results for vars of interest
# overall
survtable_ad("case5final")
class(adults2023int_hiv$case5final)
survtable_all_ad("case5final") # overall n
survtable_ad_exp("catresult")

# continuous data: hhmem_n (number of household members), agenum (age)
survmean_ad_both("agenum")
survmean_ad("agenum")

survmean_ad_both("hhmem_n") 
survmean_ad("hhmem_n")

# categorical data
catvars <- c("catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net


survtable_ad_exp("hv104")
survtable_ad_both("hv104") # 1 male, 2 female

table(adults2023int_hiv$reltoheadhh, adults2023int_hiv$catresult, useNA = "always")
survtable_ad_exp("reltoheadhh")
survtable_ad_both("reltoheadhh") # rel to head of hh


table(adults2023int$hv025)
survtable_ad_exp("hv025")
survtable_ad_both("hv025") # hv025=urban(1)/rural(2)

survtable_ad_exp("hv270")
survtable_ad_both("hv270") # hv025=urban(1)/rural(2)

table(adults2023int$pfldh)
survtable_ad_exp("pfldh") # 
survtable_ad_both("pfldh")

survtable_ad_exp("pv18s") # 
survtable_ad_both("pv18s")

survtable_ad_exp("po18s") # 
survtable_ad_both("po18s")

survtable_ad_exp("po18s") # 
survtable_ad_both("po18s")

survtable_ad_exp("hiv03") # hiv
survtable_ad_both("hiv03")

survtable_ad_exp("ha54") # currently pregnant

survtable_ad_both("ha54")
table(adults2023int$ha54, adults2023int$hbvresult, adults2023int$hv104)

survtable_ad_exp("ha57") # 
survtable_ad_both("ha57")

survtable_ad_exp("prov2015") # 
survtable_ad_both("prov2015")

survtable_ad_exp("totalkidpos_f") # 
survtable_ad_both("totalkidpos_f")
svyby(~totalkidpos_f,~ catresult, designf_dhs2_ad, svytotal, na.rm=T, survey.lonely.psu="adjust")

table(adults2023int$case5final)
table(adults2023int$pfldh)
table(adults2023int$pv18s)
table(adults2023int$po18s_adult) 
table(adults2023int$po18s)

# province
survtable_all_ad("shnprovin") #
survtable_ad("shnprovin") # not working - too small of counts?

table(kid_dhs_int$shnprovin, kid_dhs_int$hbvresultlowna)%>% clipr::write_clip()
table(kid_dhs_int$hv024, kid_dhs_int$hbvresultlowna)%>% clipr::write_clip()
svyby(~shnprovin,~hbvresultlowna, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust",covmat=TRUE) # %>% clipr::write_clip()

svytable(~ shnprovin + case5final, designf_dhs2_ad) %>% clipr::write_clip()



# create table one vars
numvars_adult <- c("agenum", "hhmem_n") # age, household size, 
catvars_adult <- c("case5final","catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "pfldh_adult") # sex, province, urbal/rural, wealth, children <5 slept under net
allvars_adult <- c("case5final","catresult","agenum", "hv104","totalkidpos_f","hhmem_n", "hv024", "hv025","hv270",  "pfldh_adult") #hv104=sex, hv024=prov, hv025=urban(1)/rural(2), hv270 wealth,hv228(kids<5 slept under net)

#Export table 1
k08_int_2023_adults <- CreateTableOne(vars=allvars_adult, factorVars = catvars_adult,  data=adults2023int, strata ="case5final", addOverall = TRUE, test = TRUE) # kruskal.test
#print to output to view
print(k08_int_2023_adults, nonnormal = allvars , exact = allvars ,quote = FALSE, noSpaces = TRUE, varLabels = T, showAllLevels = TRUE)
# save as object to export
k08_int_2023_adults <- print(k08_int_2023_adults, nonnormal = allvars , exact = allvars ,quote = FALSE, noSpaces = TRUE, varLabels = T  ,printToggle = FALSE, showAllLevels = TRUE)
## Save to a CSV file
write.csv(k08_int_2023_adults, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/k08_int_2023_adults.csv")


## adults map---------------------------------------------------------------
# case/control hh of adults
table(adults2023int$case5final, adults2023int$hbvresult, useNA = "always")

adults2023int <- adults2023int %>% mutate(twobytwo = case_when(
  hbvresult==1 & case5final==1 ~ 3, # exposed and positive
  hbvresult==0 & case5final==1 ~ 2, # exposed and negative
  hbvresult==1 & case5final==0 ~ 1, # unexposed and positive
  hbvresult==0 & case5final==0 ~ 0, # unexposed and negative
  hbvresult==1 & case5final==9 ~ NA_real_, # drop - case before contamination
  hbvresult==0 & case5final==9 ~ NA_real_, # drop - case before contamination
  TRUE ~ NA_real_))
table(adults2023int$case5final, adults2023int$hbvresult, useNA = "always")
table(adults2023int$twobytwo, adults2023int$hbvresult, useNA = "always")
table(adults2023int$twobytwo, adults2023int$case5final, useNA = "always")

adults2023int_wdrop <- adults2023int %>% filter(!(is.na(twobytwo)))
nrow(adults2023int)
ncol(adults2023int_wdrop)
ncol(ad_5f)

table(ad_5f$twobytwo)
# redo sf geometry
hhsum_all_sf = st_as_sf(hhsum_all[!is.na(hhsum_all$latnum) &!is.na(hhsum_all$longnum),], coords = c("longnum", "latnum"), crs = 4326) 
# check
view(hhsum_all_sf)
# aggregated cluster prevalence (since clusters have same GPS - no need for plotting overlapping points)


nogpsad <- hhsum_all_sf %>% filter(!(cluster_hh %in% adults2023int_dropsf$cluster_hh)) %>% dplyr::select(c("cluster_hh","latnum", "longnum","shnprovin","adm1fips","adm1name","catresult" ))

table(adults2023int_wdrop$case5final, useNA = "always")
table(adults2023int_dropsf$case5final, useNA = "always")

# subset cluster level proportions
output_df_ad <- adults2023int_dropsf %>% #filter(case5final != 9) %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   # npos = n(hbvresultlowna),
                   casecount = sum(case5final==1),
                   controlcount = sum(case5final==0),
                   tot2by2 = count(as.factor(twobytwo)))
table(output_df_ad$casecount)
table(output_df_ad$controlcount)
class(adults2023int_dropsf$twobytwo)

# check GPS missingness by province, check counts by province
output_df_ad <- adults2023int_dropsf %>% #filter(case5final != 9) %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   poscount = sum(hbvresult==1),
                   negcount = sum(hbvresult==0),
                   adultprev = mean(hbvresult, na.rm=T)*100)
view(test)
test1 <- adults2023int_wdrop %>% filter(longnum=="0") %>% select(c("hv001","latnum","longnum","hbvresult","case5final","shnprovin")) %>% distinct(hv001, .keep_all = T)
view(test1)
distinctclust <- adults2023int_wdrop %>% distinct(hv001, .keep_all = T)
nrow(distinctclust)
addmargins(table(test1$shnprovin))
addmargins(table(distinctclust$shnprovin))
addmargins(table(adults2023int_wdrop$shnprovin))
addmargins(table(adults2023int_wdrop$shnprovin, adults2023int_wdrop$twobytwo))

output_df <- merge(output_df, kid_dhs_int[,c("hh_weight","hv001")],by="hv001", all.x = TRUE)
view(hhsum_all_sf)
#A1 <-
hhsum_all_sf$hhprev_samp
# exposed households
view(drcprov) # adm1_viz is prov label - need to append 0/1 of whether province had exposed households (kid positive) or no
view(hhsum_all_sf)
library(readxl)
library(sf)
provlabels <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/Results discussions/prov counts.xlsx",
                         sheet = "provlabels")
drcprov <- left_join(drcprov, provlabels, by = "ADM1_VIZ_N")
# add indicator for if prov had exp hh
expprov <- hhsum_all %>% filter(casecontrol5==1) %>%  distinct(prov2015) %>% rename(prov_name = prov2015)
expprov$hasexphh <- 1

drcprov <- left_join(drcprov, expprov, by = "prov_name")
drcprov$hasexphh <- ifelse(is.na(drcprov$hasexphh), 0, drcprov$hasexphh)
# all provinces have unexposed households

#drc cities # https://data.humdata.org/dataset/democratic-republic-of-the-congo-major-cities?
drccities = st_read("/Users/camillem/Documents/GitHub/dhs_hbv/Data/cod_cities_20180906h/COD_CITIES_20180906H.shp", stringsAsFactors = FALSE) %>%   st_transform(4326)
top6 <- drccities %>% arrange(desc(estimate20)) %>%  slice(1:6) %>% filter(name != "Kananga")


ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, aes(fill=as.factor(hasexphh)), color="tan4", size=0.5) + 
  scale_fill_manual(values = c("lightgray","cornsilk3"))+
  geom_sf(data=st_jitter(hhsum_all_sf[hhsum_all_sf$casecontrol5==1,], factor = 0.005), aes(color=hhprev_samp)) + 
  geom_sf(data=top6, shape=17)+
  geom_sf_text(data = top6, aes(label = name), nudge_y = 0.7, nudge_x = 0.6)+
   #labs(color='HBV prevalence \nin exposed households') + 
  theme_bw(base_size=14) + 
  #scale_color_brewer(palette = 'Reds') + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("All household members")+
  theme(#legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))
  
#unexposed households
  ggplot() + 
    geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
    geom_sf(data=DRC, fill="cornsilk") +
    geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
    geom_sf(data=hhsum_all_sf[hhsum_all_sf$casecontrol5==0,],  alpha=0.8) + 
    #labs(color='HBV prevalence \nin children < 5') + 
    theme_bw(base_size=14) + 
    #scale_color_brewer(palette = 'Reds') + 
    scale_color_distiller(palette = 'Spectral') + 
    scale_x_continuous(limits=c(12,31)) + 
    scale_y_continuous(limits=c(-13.5,5.4)) + 
    ggtitle("All household members")+
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.ticks=element_blank(), 
          axis.text.x=element_blank(), 
          axis.text.y=element_blank(),
          panel.background = element_rect(fill="#daeff8", color=NA))


#adult prev by exp and unexp hhs
output_df_ad_exp <- adults2023int_dropsf %>% filter(case5final == 1) %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   poscount = sum(hbvresult==1),
                   negcount = sum(hbvresult==0),
                   adultprev = mean(hbvresult, na.rm=T)*100)
output_df_ad_unexp <- adults2023int_dropsf %>% filter(case5final == 0) %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   poscount = sum(hbvresult==1),
                   negcount = sum(hbvresult==0),
                   adultprev = mean(hbvresult, na.rm=T)*100)
# prev among case hhs
#B1 <- 
  ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_df_ad_exp[output_df_ad_exp$adultprev > 0,], aes(color=(adultprev)), alpha=0.8) + 
  #labs(color='HBV prevalence \nin children < 5') + 
  theme_bw(base_size=14) + 
  #scale_color_brewer(palette = 'Reds') + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Case household members")+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))
B1
# prev among control hhs
#C1 <- 
  ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_df_ad_unexp[output_df_ad_unexp$adultprev > 0,], aes(color=(adultprev)), alpha=0.8) + 
  labs(color='HBV prevalence \nin adults') + 
  theme_bw(base_size=14) + 
  #scale_color_brewer(palette = 'Reds') + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Control household members")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

A1 + B1 + C1 + plot_layout(nrow=1, ncol = 3) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/adults3.png', width=18, height=6)

## All saved together---------------
A + A1 + B1 + C1 + plot_layout(nrow=1, ncol = 4) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/kid1adults3.png', width=16, height=4)

# penta by age

table(kidmapsf$agenum, useNA = "always")
# penta by age group, using tetanus coverage
kidmapsf_byage <- kidmapsf %>% group_by(agenum) %>% 
  dplyr::summarize(n=n(),
                   tetlowbyage = mean(shtetaindasno, na.rm=T)*100,
                   hbvbyage = mean(hbvresultlowna, na.rm=T)*100)

output <- kidmapsf %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   npos = sum(hbvresultlowna==1),
                   prev = mean(hbvresultlowna, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)



table(kidmapsf$agenum, kidmapsf$shtetaindasno, useNA = "always")
table(kidmapsf$agenum, kidmapsf$hbvresultlowna, useNA = "always")




# adults choropleth
casehh <- adults2023int_hiv %>% filter(case5final == 1)
controlhh <- adults2023int_hiv %>% filter(case5final == 0)

# or ad_5f_exp, ad_5f_unexp

designf_adcase <-svydesign(ids=ad_5f_exp$hv001, strata=ad_5f_exp$hv022 , weights=ad_5f_exp$hh_weight,  data=ad_5f_exp)
options(survey.lonely.psu="adjust")
designf_dhs2_adcase <-as_survey_design(designf_adcase)

svytotal(~shnprovin, designf_dhs2_adcase, na.rm=T, survey.lonely.psu="adjust") 
svytable(~ shnprovin + hbvresult, designf_dhs2_adcase) %>% clipr::write_clip()

# controls
designf_adcont <-svydesign(ids=controlhh$hv001, strata=controlhh$hv022 , weights=controlhh$hh_weight,  data=controlhh)
options(survey.lonely.psu="adjust")
designf_dhs2_adcont <-as_survey_design(designf_adcont)

svytotal(~shnprovin, designf_dhs2_adcont, na.rm=T, survey.lonely.psu="adjust") 
svytable(~ shnprovin + hbvresult, designf_dhs2_adcont) %>% clipr::write_clip()

# import formatted results
wtdprovad <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/Results discussions/prov counts.xlsx",
                         sheet = "adultschoro")

wtdprovad$ADM1_NAME <- toupper(wtdprovad$provnamesimp)
drcprov_hbvad <- left_join(drcprov,wtdprovad, by="ADM1_NAME")
view(drcprov_hbvad)

ggplot()+
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=drcprov_hbvad,  mapping=aes(fill=caseprev))+
  scale_fill_distiller(palette = 'Spectral',breaks=seq(0,75,20)) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Weighted HBsAg prevalence among exposed adults")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

ggplot()+
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=drcprov_hbvad,  mapping=aes(fill=controlprev))+
  scale_fill_distiller(palette = 'Spectral', limits = c(0,75))+
  #scale_fill_gradientn(colours = "viridis", limits = c(0,75))+
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Weighted HBsAg prevalence among unexposed adults")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

# Adults kriging---------------------------
# fit for cases, df: output_df_ad_case
# and controls, df: output_df_ad_control
# kriging using gstat: https://rpubs.com/nabilabd/118172 
# https://mgimond.github.io/Spatial/interpolation-in-r.html#generate-the-variance-and-confidence-interval-maps

# ad_5f_exp, ad_5f_unexp

output_df_ad_case

output_adca <- output_df_ad_case %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   npos = sum(hbvresultlowna==1),
                   prev = mean(hbvresultlowna, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)
output_df_ad_control <- adults2023int_dropsf %>% filter(case5final == 0) %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   poscount = sum(hbvresult==1),
                   negcount = sum(hbvresult==0),
                   adultprev = mean(hbvresult, na.rm=T)*100)

output_points_adca <- st_join(output_df_ad_case, DRC, join = st_intersects) %>% filter(!is.na(Country))
output_points_adco <- st_join(output_df_ad_control, DRC, join = st_intersects) %>% filter(!is.na(Country))

# make variogram
m.vgm <- gstat::variogram(adultprev~1, output_points_adca)
plot(m.vgm)
# fit a model to the sample variogram
# https://gisgeography.com/semi-variogram-nugget-range-sill/
m.fit <- gstat::fit.variogram(m.vgm, model=vgm(psill=1200,"Exp",range=500, nugget=600))

# plot
plot(m.vgm,m.fit)

# simple kriging
spDRC <- as_Spatial(DRC)
grd <- makegrid(spDRC, n = 50000)# making grid of points
colnames(grd) <- c('x','y')
grd_pts <- SpatialPoints(coords = grd, 
                         proj4string=CRS(proj4string(spDRC)))

# find all points in `grd_pts` that fall within DRC outline
grd_pts_in <- grd_pts[spDRC, ]

# transform grd_pts_in back into a data frame
gdf <- as.data.frame(coordinates(grd_pts_in)) 

# conduct kriging: household hbv prev
m.kriged <- gstat::krige(adultprev~1, output_points_adca, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged$var1.pred)

# assign points into bins
krige <- m.kriged %>% cbind(gdf$x, gdf$y) %>% mutate(
  #var1.pred = cut(var1.pred, breaks=seq(0,15,by=1)), 
  var1.pred_cut = case_when(
    var1.pred <= 0 ~ 0,
    var1.pred > 0 & var1.pred <= 2 ~ 2,
    var1.pred > 2 & var1.pred <= 7 ~ 7,
    var1.pred > 7 & var1.pred ~ 9),
  var1.pred_largegrp = cut(var1.pred, breaks = seq(0,14, by=1)),
  var1.pred_largegrp = ifelse(is.na(var1.pred_largegrp),0,var1.pred_largegrp),
  var1.pred_largegrp2 = cut(var1.pred, breaks = seq(0,14, by=2)),
  var1.pred_largegrp2 = ifelse(is.na(var1.pred_largegrp2),0,var1.pred_largegrp2),
  #var1.pred_largegrp2 = cut(var1.pred, breaks = c(0,1,2,3,4,5,6,7,8,9,10), include.lowest = T),
  se = sqrt(var1.var),
  se = cut(se, breaks=seq(0,24,by=4))) %>% filter(!is.na(var1.pred))

table(krige$var1.pred_cut)
table(krige$var1.pred_cut,krige$var1.pred_largegrp, useNA = "always")
table(krige$var1.pred_largegrp2, useNA = "always")

# factor for prev
ggplot() + 
  geom_tile(data=(krige %>% as.data.frame), aes(x=gdf.x,y=gdf.y,fill=as.factor(var1.pred_cut))) + 
  geom_sf(data=admin0 %>% filter(ISO != 'COD'), fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  labs(fill="Predicted HBV \nprevalence, \nchildren ≤5", x='', y='') + 
  theme_bw(base_size=14) + 
  scale_fill_manual(values = c("#3288BD","#FFFFBF","#FDAE61", "#9E0142") , labels=c("0","≤2%","2-7%",">7%")) +
  #scale_fill_brewer(palette ="Spectral", direction=-1 , labels=c("0","≤2%","2-7%",">7%")) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

B <- 
  ggplot() + 
  geom_tile(data=(krige %>% as.data.frame), aes(x=gdf.x,y=gdf.y,fill=as.factor(var1.pred_largegrp2))) + 
  geom_sf(data=admin0 %>% filter(ISO != 'COD'), fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  labs(fill="Predicted HBV \nprevalence, \nchildren ≤5", x='', y='') + 
  theme_bw(base_size=14) + 
  scale_fill_brewer(palette ="Spectral", direction=-1, labels=c("0","1-2","3-4","5-6","7-8","9-10","11-12","13-14"))+
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))
ggsave("./Plots/krighbvprev.png", width = 9, height = 6)

# Figure for Table 2 regression estimates--------
library(readxl)
unadjkids <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/Results discussions/Tables july2023.xlsx",
                         sheet = "tab2kidsimport")

ggplot(unadjkids, aes(x=level, y=prevdiff100)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_pointrange(aes(x=level, y=prevdiff100, ymin=lowerciprevdiff100, ymax=upperciprevdiff100), shape=15, size=0.8, color="black", show.legend=F, fatten=0.2) + 
  geom_point(shape=15, size=5, aes(color=variable), show.legend=F, alpha=0.9) +
  scale_color_brewer(palette = "Dark2")+
  coord_flip() + theme_bw() +
  #scale_x_continuous(trans = "reverse") + 
  labs(x="", y="Unadjusted prevalence difference per 100 kids") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank()) 

ggsave("./Plots/aasldpd.png", width = 9, height = 6)

# Exploratory DA---------------------
# kids: kid_dhs_int
# adults: adults2023int

summary(output_points_adca$adultprev)
hist(output_points_adca$adultprev)

table(kid_dhs_int$hbvresultlowna)
table(kid_dhs_int$hbvresultlowna)

# output_df_ad_case, output_df_ad_control
table(adults2023int$case5final)
table(adults2023int$hbvresult)

hist(adults2023int$hbvresult)

test <- adults2023int %>% group_by(cluster_hh) %>% dplyr::summarize(n=n(),
                                                              poscount = sum(hbvresult==1),
                                                              negcount = sum(hbvresult==0),
                                                              adultprev = mean(hbvresult, na.rm=T)*100)
View(test)
nrow(test)
nrow(adults2023int)
# add case-control status to cluster_hh
test <- left_join(test, adults2023int[,c("cluster_hh","case5final")], by="cluster_hh")

hist(test$adultprev)
hist(test$poscount)

test %>% 
  ggplot()+
  geom_histogram(aes(poscount), binwidth = 1)+
  theme_bw()+
  facet_wrap(~as.factor(case5final))

test %>% 
  ggplot()+
  geom_histogram(aes(adultprev), binwidth = 5)+
  theme_bw()+
  facet_wrap(~as.factor(case5final))

table(adults2023int$hbvresult)
hhwcases <- adults2023int_hiv %>% filter(hbvresult==1 & case5final==1) # case households with case in adults
hhwcases <- adults2023int_hiv %>% filter(case5final==1) # all case households
View(hhwcases)

hhwcases <- hhwcases %>% 
  dplyr::mutate(reltoheadhh=factor(
    hhwcases$hv101, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,98),
    labels = c("Head", "Spouse","Son/daughter","Son/daughter-in-law","Grandchild","Parent","In-laws","Brother/sister","Co-spouse","Other","Adopted/in custody","Not related","Nephew/niece","Nephew/niece by marriage","Don't know")))

hhwcases <- hhwcases %>% 
  dplyr::mutate(prov2015=factor(
    hhwcases$shnprovin, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
    labels = c("Kinshasa", "Kwango","Kwilu","Mai-Ndombe","Kongo Central","Equateur","Mongala","Nord-Ubangi","Sud-Ubangi","Tshuapa","Kasai","Kasai-Central","Kasai-Oriental","Lomami","Sankuru","Haut-Katanga","Haut-Lomami","Lualaba","Tanganyka","Maniema","Nord-Kivu","Bas-Uele","Haut-Uele","Ituri","Tshopo","Sud-Kivu")))

hhwcases <- hhwcases %>% 
  dplyr::mutate(sex=factor(
    hhwcases$hv104, 
    levels = c(1, 2),
    labels = c("Male", "Female")))
# deep dive on households
hhwcases %>% count(cluster_hh)
hhwcases %>% reframe(cluster_hh,totalkidpos, hv105, sex,reltoheadhh,prov2015,hbvresult)

# add kids
table(kid_dhs_int$catresult)
table(kids2023$case5final)

kidscase <- kid_dhs_int %>% filter(cluster_hh %in% hhwcases$cluster_hh)
kidscase$hbvresult <- as.factor(kidscase$hbvresult)
class(kidscase$hbvresult)
class(hhwcases$hbvresult)

hhwcases_kids <- bind_rows(hhwcases,kidscase)

hhwcases_kids %>% reframe(cluster_hh,totalkidpos, hv105, sex,reltoheadhh,prov2015,hbvresult)
table(hhwcases_kids$reltoheadhh, useNA = "always")

hhwcases_kids <- hhwcases_kids %>% 
  dplyr::mutate(reltoheadhh=factor(
    hhwcases_kids$hv101, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,98),
    labels = c("Head", "Spouse","Son/daughter","Son/daughter-in-law","Grandchild","Parent","In-laws","Brother/sister","Co-spouse","Other","Adopted/in custody","Not related","Nephew/niece","Nephew/niece by marriage","Don't know")))

hhwcases_kids <- hhwcases_kids %>% 
  dplyr::mutate(sex=factor(
    hhwcases_kids$hv104, 
    levels = c(1, 2),
    labels = c("Male", "Female")))
hhwcases_kids <- hhwcases_kids %>% 
  dplyr::mutate(prov2015=factor(
    hhwcases_kids$shnprovin, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
    labels = c("Kinshasa", "Kwango","Kwilu","Mai-Ndombe","Kongo Central","Equateur","Mongala","Nord-Ubangi","Sud-Ubangi","Tshuapa","Kasai","Kasai-Central","Kasai-Oriental","Lomami","Sankuru","Haut-Katanga","Haut-Lomami","Lualaba","Tanganyka","Maniema","Nord-Kivu","Bas-Uele","Haut-Uele","Ituri","Tshopo","Sud-Kivu")))

hhwcases_kids <- hhwcases_kids %>% arrange(prov2015,desc(totalkidpos))
hhtrees <- hhwcases_kids %>% reframe(cluster_hh,totalkidpos, hv105, sex,reltoheadhh,prov2015,hbvresult) 

