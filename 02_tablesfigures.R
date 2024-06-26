# 02_tablesfigures.R

#Load packages----------------
library(tidyverse)
library(tableone)
library(survey)
library(srvyr)
library(readxl)
library(sf)
library(janitor)
# after 01_datacleaning.R has been compiled, use the final data frame for analysis

#Weighted children------------------------
# elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate_at(c('poskids'), as.factor)
elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate_at(c('v480', 'injec','beat'), as.factor)
elig_kids_whbvres_wt_kr <- elig_kids_whbvres_wt_kr %>% mutate_at(c('v480'), as.factor)

elig_kids_whbvres_wt_kr %>% filter(hbvresult1==1 & hbvresult5==0) %>% group_by(prov2015, hv024, sex) %>% summarise(n=n(), nsum=nrow(.) )
elig_kids_whbvres_wt_kr %>% filter(hbvresult5==1 & hbvresult100==0) %>% group_by(prov2015, hv024, sex) %>% summarise(n=n(), nsum=nrow(.) )
elig_kids_whbvres_wt_kr %>% filter(hbvresult5==1 & hbvresult100==0) %>% group_by(prov2015, hv024, sex) %>% summarise(n=n(), nsum=nrow(.) )
elig_kids_whbvres_wt_kr %>% group_by(hv101) %>% count() %>% print(n=Inf)

# go through 01_datacleaning_rev.R) to obtain elig_kids_whbvres_wt_kr
# make survey design object
# weight variable decisions: hh_weight (frmo hv005), hv028_div, both_wt_new (hv028 + propens score), both_wt_old (hv005 + propens score)
designf <-svydesign(ids=elig_kids_whbvres_wt_kr$hv001, strata=elig_kids_whbvres_wt_kr$hv022 , weights=elig_kids_whbvres_wt_kr$both_wt_new,  data=elig_kids_whbvres_wt_kr)
options(survey.lonely.psu="adjust")
designf_dhs2 <-as_survey_design(designf)

# survey design for householdsv480# survey design for households
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
age_av <- as.data.frame(survmean_all("hc1")) %>% rownames_to_column(var = "cov")  %>%  
  mutate(levels = "Age (months), mean (SD)") %>% rename(se = hc1)
hhmem_av <- as.data.frame(survmean_all("hv009")) %>% rownames_to_column(var = "cov")  %>%  
  mutate(levels = "Household members, mean (SD)") %>% rename(se = hv009)
kids_av <- as.data.frame(survmean_all("hv014")) %>% rownames_to_column(var = "cov")  %>%  
  mutate(levels = "Children in household, mean (SD)") %>% rename(se = hv014)
avgs_tot <- dplyr::bind_rows(list(age_av, hhmem_av, kids_av), .id = 'source') %>% mutate(Tab1tot = paste(round(mean,1),' (',round(se,2),')', sep = ""))
Tab1_num_tot <- avgs_tot %>% select(-c(source, mean,se ))
view(Tab1_num_tot)

## cont data, by hbv----------
age_av_by <- as.data.frame(survmean("hc1")) %>% rownames_to_column(var = "covname") %>% rename(mean = hc1) %>% mutate(cov = "hc1", levels = "Age (months), mean (SD)")
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
all_tot <- dplyr::bind_rows(list(age, sex, relhh, urbrur, location,  wealth, pfmal, tetorig, poskids, anem, modstu, dpt_dos, inje, reused, beat, prov), .id = 'source') #tetlower, tetupper,
all_tot <- all_tot %>% filter(total>0) %>% select(-SE)
all_tot <- all_tot %>% group_by(source) %>% mutate(totperc = 100*(total / sum(total)))  %>% ungroup()
view(all_tot)
# total counts
tot <- as.data.frame(all_tot %>% group_by(source) %>% summarise(tot = sum(total))) %>% summarise(Tab1tot = median(tot))
# weighted total is 5773, unweighted is 5679. ~100 more weighted; this makes sense as the mean weight (both_wt_new) is 1.016, increasing total by 1.6%

## hbv n, overall------
tot$levels <- "Overall n"
tot$covname <- "hbvresult5"
tot <- tot %>% relocate(covname, levels)
tot$Tab1tot <- as.character(round(tot$Tab1tot, 0))
view(tot)
## Simplify Tab1
tab1 <- all_tot %>% mutate_if(is.numeric, round, 0) %>% mutate(Tab1tot = paste(total,' (',totperc,')',  sep = ""))
tab1_simp <- tab1 %>% select(c("covname", "levels", "Tab1tot"))

#combine with numeric from above - in numeric, need to rename cov to covname
Tab1_num_tot2 <- Tab1_num_tot %>% dplyr::rename(covname = cov)

tab1_simp_all <- dplyr::bind_rows(list(tot, Tab1_num_tot2, tab1_simp))
view(tab1_simp_all)
tab1_simp_all$levels <- ifelse(tab1_simp_all$covname=="v4808", "Don't know", ifelse(tab1_simp_all$covname=="v4801","New, unopened", ifelse(tab1_simp_all$covname=="v4800","Opened/used" ,tab1_simp_all$levels)))
#drop = c(11,15,26,28,30,34,36,38,39)
drop = c("sexFemale", "urbanruralUrban", "pfmalariaPf-negative", "tetabNonreactive", "tetabIndeterminate","anemia_fMild-to-none", "anemia_fNot available", "modstunt_fNo stunting", "modstunt_fNot available", "v4808")
tab1_simp_all2 <- tab1_simp_all %>%
  filter(!covname %in% drop )
view(tab1_simp_all2)
# Export simplified Table 1------
write.csv(tab1_simp_all2, file = here("Data", "tab1_apr11.csv"))

## cat data, by hbv----------
svyciprop(~hbvresult5, designf_dhs2, method = "me",na.rm=T, survey.lonely.psu="adjust")

age_by <- as.data.frame(svyby(~hbvresult5,~hv105_fromhc1_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("hv105_fromhc1_f")) %>% mutate(cov = "hv105_fromhc1_f")
sex_by <- as.data.frame(svyby(~hbvresult5,~sex, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("sex")) %>% mutate(cov = "sex")
reltoheadhh_by <- as.data.frame(svyby(~hbvresult5,~reltoheadhh_simp, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("reltoheadhh_simp")) %>% mutate(cov = "reltoheadhh_simp")
urbru_by <- as.data.frame(svyby(~hbvresult5,~urbanrural, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("urbanrural")) %>% mutate(cov = "urbanrural")
location_by <- as.data.frame(svyby(~hbvresult5,~location, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("location")) %>% mutate(cov = "location")
prov2015_by <- as.data.frame(svyby(~hbvresult5,~prov2015, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("prov2015")) %>% mutate(cov = "prov2015")
wealth_by <- as.data.frame(svyby(~hbvresult5,~wealth, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("wealth")) %>% mutate(cov = "wealth")
pfmal_by <- as.data.frame(svyby(~hbvresult5,~pfmalaria, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("pfmalaria")) %>% mutate(cov = "pfmalaria")
shteta_by <- as.data.frame(svyby(~hbvresult5,~tetab, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("tetab")) %>% mutate(cov = "tetab")
anem_by <- as.data.frame(svyby(~hbvresult5,~anemia_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("anemia_f")) %>% mutate(cov = "anemia_f")
modstu_by <- as.data.frame(svyby(~hbvresult5,~modstunt_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("modstunt_f")) %>% mutate(cov = "modstunt_f")
dpt_dos_by <- as.data.frame(svyby(~hbvresult5,~dpt_doses_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("dpt_doses_f")) %>% mutate(cov = "dpt_doses_f")
inje_by <- as.data.frame(svyby(~hbvresult5,~injec_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("injec_f")) %>% mutate(cov = "injec_f")
beat_by <- as.data.frame(svyby(~hbvresult5,~beat_f, designf_dhs2, svyciprop, vartype="ci",na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "levels") %>% select(-one_of("beat_f")) %>% mutate(cov = "beat_f")
view(beat_by)
# combine
all_by <- dplyr::bind_rows(list(age_by, sex_by, reltoheadhh_by, urbru_by, location_by, wealth_by, pfmal_by, shteta_by, anem_by, modstu_by,dpt_dos_by, inje_by, beat_by,  prov2015_by)) 
view(all_by)
all_ci <- all_by %>% filter(!is.na(hbvresult5)) %>% #select(-c("hbv_pos", "hbv_neg")) %>% 
  mutate(prev100 = round(hbvresult5*100, 1),
         ci_l100 = round(ci_l*100, 1),
         ci_u100 = round(ci_u*100, 1),
         Tab2prevci = paste(prev100,' (',ci_l100,', ', ci_u100,')', sep = "")) %>% 
  relocate(cov, levels)

all_ci_s <- all_ci %>% select(c("cov","levels", "Tab2prevci"))
view(all_ci_s)
#write.csv(all_ci_s, file = here("Data","tab2prcis_apr11.csv"))

# =anemia
svyby(~hbvresult5,~anemia_f, designf_dhs2, svytotal,method = "me",na.rm=T, survey.lonely.psu="adjust")

# covariates by hbv status (col%, not giving prevalence within each group)
age_byc <- as.data.frame(t(as.data.frame(survtable("hv105_fromhc1_f")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "hv105_fromhc1_f", 2)[,2], cov = "hv105_fromhc1_f") 
sex_byc <-  as.data.frame(t(as.data.frame(survtable("sex")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "sex", 2)[,2], cov = "sex") 
reltoheadhh_byc <-  as.data.frame(t(as.data.frame(survtable("reltoheadhh_simp")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "reltoheadhh_simp", 2)[,2], cov = "reltoheadhh_simp") 
urbru_byc <-  as.data.frame(t(as.data.frame(survtable("urbanrural")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "urbanrural", 2)[,2], cov = "urbanrural") 
location_byc <-  as.data.frame(t(as.data.frame(survtable("location")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "location", 2)[,2], cov = "location") 
prov2015_byc <-  as.data.frame(t(as.data.frame(survtable("prov2015")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "prov2015", 2)[,2], cov = "prov2015") 
wealth_byc <-  as.data.frame(t(as.data.frame(survtable("wealth")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "wealth", 2)[,2], cov = "wealth") 
pfmal_byc <-  as.data.frame(t(as.data.frame(survtable("pfmalaria")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "pfmalaria", 2)[,2], cov = "pfmalaria") 
shteta_byc <-  as.data.frame(t(as.data.frame(survtable("tetab")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "tetab", 2)[,2], cov = "tetab") 
anem_byc <-  as.data.frame(t(as.data.frame(survtable("anemia_f")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "anemia_f", 2)[,2], cov = "anemia_f") 
modstu_byc <-  as.data.frame(t(as.data.frame(survtable("modstunt_f")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "modstunt_f", 2)[,2], cov = "modstunt_f") 
dpt_dos_byc <-  as.data.frame(t(as.data.frame(survtable("dpt_doses_f")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "dpt_doses_f", 2)[,2], cov = "dpt_doses_f") 
inje_byc <-  as.data.frame(t(as.data.frame(survtable("injec_f")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "injec_f", 2)[,2], cov = "injec_f") 
beat_byc <-  as.data.frame(t(as.data.frame(survtable("beat_f")))) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "beat_f", 2)[,2], cov = "beat_f") 

svyby(~pfmalaria,~hbvresult5, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()

# survtable("poskids") # since there are 0 counts by design, need to run outside the function
poskids_by <- as.data.frame(svyby(~hbvresult5,~poskids, designf_dhs2, svyciprop, na.rm=T, survey.lonely.psu="adjust")) %>% rownames_to_column(var = "covname") %>% select(-one_of("poskids"))
view(poskids_by)

## combine count cat data----------
all_byc <- dplyr::bind_rows(list(age_byc, sex_byc, reltoheadhh_byc, urbru_byc, location_byc,  wealth_byc, pfmal_byc, shteta_byc, anem_byc, modstu_byc,dpt_dos_byc, inje_byc, beat_byc,prov2015_byc)) %>% 
  filter(!grepl("se\\.", covname) & !grepl("hbvresult5", covname))
view(all_byc)
colnames(all_byc)[2] <- "hbv_neg"
colnames(all_byc)[3] <- "hbv_pos"
all_byc$total <- all_byc$hbv_neg + all_byc$hbv_pos

#prep Table 2 - drop hbv_neg from all_byct,  merge counts(all_byc) and prev (all_by), bind overall n,
all_byc2 <- left_join(all_byc, all_ci_s, by = c("cov", "levels"))
view(all_byc2)
all_byc2 <- all_byc2 %>% select(-c(covname, hbv_neg)) %>% relocate(cov, levels)

## hbv n, by counts------## hbv hv028_divn, by counts------
tothbv <- survtable_all("hbvresult5") 
tothbv <- as.data.frame(tothbv) %>% rownames_to_column(var = "covname") %>% dplyr::rename(hbv_pos = total) %>% select(-hbvresult5)
#tothbv$hbv_neg <- tot$Tab1tot - tothbv$hbv_pos
tot2 <- tot %>% select(-covname) %>% rename(total = Tab1tot)
tot_by <- cbind(tothbv,tot2)
tot_by <- tot_by %>% select(-covname) %>% relocate(cov, levels)
tot_by$Tab2prevci <- ""  # no prev diff for overall, but need var on dataframe for Tab2 merge
tot_by$total <- as.numeric(tot_by$total)

#overall sag positivity sco5
survtable_all("hbvresult5") 
prop.table(svytable(~hbvresult5, designf_dhs2))
svyciprop(~hbvresult5,  designf_dhs2, method="lo")
#sco1
survtable_all("hbvresult1") 
prop.table(svytable(~hbvresult1, designf_dhs2))
svyciprop(~hbvresult1,  designf_dhs2, method="lo")
#sco2
survtable_all("hbvresult2") 
prop.table(svytable(~hbvresult2, designf_dhs2))
svyciprop(~hbvresult2,  designf_dhs2, method="lo")
#sco100
survtable_all("hbvresult100") 
prop.table(svytable(~hbvresult100, designf_dhs2))
svyciprop(~hbvresult100,  designf_dhs2, method="lo")


## hbv n onto by df-----
all_byct <- dplyr::bind_rows(list(tot_by, all_byc2))
colnames(all_byct)

# tab2pds from 03_models.R
all_byct <- all_byct %>% mutate(term = paste(cov, levels,  sep = ""))
# round appropriate decimal before merge
all_byct <- all_byct %>% mutate_if(is.numeric, round, 0)
tab2pds <- tab2pds %>% mutate_if(is.numeric, round, 1)

tab2all <- left_join(all_byct, tab2pds, by = "term")
colnames(tab2all)
tab2all2 <- tab2all %>% mutate(pdcomb = paste(pd100,' (',pdcilow100,', ', pdciup100,')', sep = "")) %>% select(cov, levels, hbv_pos, total, Tab2prevci, pdcomb)
view(tab2all2)
tab2all2$pdcomb <- ifelse(tab2all2$pdcomb == "NA (NA, NA)", "Referent",tab2all2$pdcomb) # most but not all are referent (eg overall and provinces should just be empty)
tab2all2$pdcomb <- ifelse(tab2all2$cov == "prov2015" | tab2all2$cov == "hbvresult5", "",tab2all2$pdcomb)
tab2all2$pdcomb <- ifelse(tab2all2$Tab2prevci == "0 (NaN, NaN)" , "",tab2all2$pdcomb)
tab2all3 <- tab2all2 %>% filter(levels != "Not available")
tab2all3 <- tab2all3  %>%  mutate(desorder=row_number())
colnames(tab2all3)

# switch order value for sex and injec_f so that final table is in correct order
vFemale <- tab2all3$desorder[tab2all3$levels == "Female"]
vMale <- tab2all3$desorder[tab2all3$levels == "Male"]
tab2all3$desorder[tab2all3$levels == "Female"] <- vMale
tab2all3$desorder[tab2all3$levels == "Male"] <- vFemale

refinj <- tab2all3 %>% filter(cov == "injec_f") %>% summarise(min(desorder))

tab2all3$desorder <- ifelse(tab2all3$cov == "injec_f" & tab2all3$pdcomb == "Referent", as.integer(refinj), 
                            ifelse(tab2all3$cov == "injec_f" & tab2all3$pdcomb != "Referent", tab2all3$desorder + 1, tab2all3$desorder ))
library(data.table)
#insert blank row for tab2 manuscript formatting
tab2all4 <- setDT(tab2all3)[, rbind(.SD, data.table(cov = ""), fill = TRUE), by = cov]
# drop extra cov that is added
tab2all4 <- tab2all4[,1:(length(tab2all4)-1)]
# label rows for formatted table
tab2all5 <- tab2all4 %>% mutate(
  labeledlevel = case_when(
    cov == "hv105_fromhc1_f" & is.na(levels) ~ "Age (months)",
    cov == "sex" & is.na(levels) ~ "Sex",
    cov == "reltoheadhh_simp" & is.na(levels) ~ "Relationship to head of household",
    cov == "urbanrural" & is.na(levels) ~ "Rurality",
    cov == "location" & is.na(levels) ~ "Setting",
    cov == "wealth" & is.na(levels) ~ "Wealth quintile",
    cov == "pfmalaria" & is.na(levels) ~ "P. falciparum malaria",
    cov == "tetab" & is.na(levels) ~ "Tetanus antibody",
    cov == "anemia_f" & is.na(levels) ~ "Anemia",
    cov == "modstunt_f" & is.na(levels) ~ "Growth stunting",
    cov == "beat_f" & is.na(levels) ~ "Physical violence toward women justified in the household",
    cov == "dpt_doses_f" & is.na(levels) ~ "DPT vaccination",
    cov == "injec_f" & is.na(levels) ~ "Injections received in last 12 months",
    cov == "prov2015" & is.na(levels) ~ "Province",
    cov == "prov2015" & is.na(levels) ~ "Province",
    TRUE ~ paste('     ',levels, sep = "")))

class(tab2all5$desorder)
# trying to get desorder value to be 0.5 less than the minimum, but not working.
tab2all6 <- tab2all5 %>% group_by(cov) %>%  reframe(minord = pmin(desorder)) %>% ungroup() %>% group_by(cov) %>% 
                                            mutate(desorder2 = case_when(
                                                     is.na(desorder) ~ minord-0.5, 
                                                     TRUE ~ desorder )) %>% ungroup()
tab2all5_exp <- tab2all5 %>% relocate(cov, levels, labeledlevel) %>% select(-desorder)
view(tab2all5_exp)
write.csv(tab2all5_exp, file = here("Data", "tab2full_apr11.csv"))

## Tab2 merg totals and bys-----
view(all_byc)
###col percent
### tab1 <- tab1 %>% group_by(source) %>% mutate(hbv_pos_perc = 100*(hbv_pos/sum(hbv_pos)), hbv_neg_perc = 100*(hbv_neg/sum(hbv_neg))) %>% ungroup()
colnames(tab2)
# row percent - to report in table 2 (prevalence)
tab2_simp <- tab2 %>% mutate(hbv_prev = 100*(hbv_pos/total))  %>% select(c(source,cov, levels, hbv_pos, total, hbv_prev)) %>% mutate_if(is.numeric, round, 1) # or round to 1
view(tab2_simp)

# Export Table 2------
write.csv(tab2_simp, file = here("Data", "tab2_simp.csv"))

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
write.csv(tab2_comb3, file = here("Data", "tab2_comb3.csv"))

view(tab2_comb3)
