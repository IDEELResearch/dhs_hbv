# 02_tablesfigures.R

# load packages
library(tidyverse)
library(tableone)
# after 01_datacleaning.R has been compiled, use the data frame _____ for analysis

kids2023 <- k08_nomiss_cc %>% filter(agegrp=="kid")

# get kid file: kidresults (from Stane/Ana testing + Abbott$agegrp=="kid")
# k08_all has everything before drop of missing
table(k08_all$agegrp, useNA = "always")
addmargins(table(k08_all$agegrp,k08_all$catresult, useNA = "always"))

test <- k08_all %>% filter(agegrp=="kid" & catresult=="reactive") %>% summarise(dbsbarcode,k08orabb)
table(test$k08orabb)


kidresults$k08orabb <- "K08 testing"

# fill non overlapping columns with NAs
abbott_trim[setdiff(names(kidresults), names(abbott_trim))] <- NA
#df2[setdiff(names(df1), names(df2))] <- NA

table(kidresults$catresult, useNA = "always")

kidresults <- kidresults %>% mutate(
  hbvresult = case_when(
    catresult=="reactive" ~ 1,
    catresult=="nonreactive" ~ 0,
    TRUE ~ NA_real_))

test <- rbind(abbott_trim[abbott_trim$agegrp=="kid",], kidresults)

ncol(abbott_trim)
ncol(kidresults)
colnames(abbott_trim)
colnames(kidresults)
table(kidresults$hbvresult)

addmargins(table(test$catresult, test$k08orabb ,useNA = "always"))
addmargins(table(kidresults$catresult ,useNA = "always"))
addmargins(table(test$catresult ,useNA = "always"))

nrow(test)
kid_dhs_int <- merge(dhsmeta,test,by="dbsbarcode" )
nrow(kid_dhs_int)
# 86 not merging with DHS - which
whichno <- test %>% filter(!(dbsbarcode %in% kid_dhs_int$dbsbarcode))
addmargins(table(whichno$catresult ,useNA = "always"))

# use: kid_dhs_int
addmargins(table(kid_dhs_int$catresult ,useNA = "always"))
# remove the missing value
kid_dhs_int <- kid_dhs_int %>% filter(!(is.na(catresult)))
addmargins(table(kid_dhs_int$catresult ,useNA = "always"))


#clean vars for table 1
class(kid_dhs_int$hv105)
kid_dhs_int$agenum <- as.numeric(kid_dhs_int$hv105)

class(kid_dhs_int$hv009)
kid_dhs_int$hhmem_n <- as.numeric(kid_dhs_int$hv009)

# check kid with age 27
age27 <- kid_dhs_int %>% filter(hv105=="27")
age27 %>% reframe(dbsbarcode,k08orabb,hv104,kids,janko_full_kids,pfldh_kids,poselected_kids)
table(age27$kids)

# lives in household with another positive kid <5
table(kid_dhs_int$hbvresult, kid_dhs_int$catresult,useNA = "always")

kid_dhs_int$hbvresultlowna <- ifelse(kid_dhs_int$catresult=="low vol",0,kid_dhs_int$hbvresult)
table(kid_dhs_int$hbvresultlowna, kid_dhs_int$catresult,useNA = "always")
# make alt variable with low vol samples as 0
totalpos <- kid_dhs_int %>% group_by(cluster_hh) %>% summarise(totalkidpos = sum(hbvresultlowna))
totalpos %>% count(totalkidpos)
 
kid_dhs_int <- left_join(kid_dhs_int, totalpos, by="cluster_hh")
kid_dhs_int %>% count(totalkidpos,catresult)

# analyze as categorical since median would be 0
kid_dhs_int$totalkidpos_f <- as.character(kid_dhs_int$totalkidpos)

#Create Table 1 for 2023 interim---------------
numvars <- c("agenum", "hhmem_n") # age, household size, 

catvars <- c("catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net

allvars <- c("catresult","agenum", "hv104","totalkidpos_f","hhmem_n", "hv024", "hv025","hv270", "hv228", "pfldh_kids") #hv104=sex, hv024=prov, hv025=urban(1)/rural(2), hv270 wealth,hv228(kids<5 slept under net)

#Export table 1
k08_int_2023 <- CreateTableOne(vars=allvars, factorVars = catvars,  data=kid_dhs_int, strata ="catresult", addOverall = TRUE, test = TRUE) # kruskal.test
#print to output
print(k08_int_2023, nonnormal = allvars , exact = allvars ,quote = FALSE, noSpaces = TRUE, varLabels = T, showAllLevels = TRUE)
# save as object to export
k08_int_2023 <- print(k08_int_2023, nonnormal = allvars , exact = allvars ,quote = FALSE, noSpaces = TRUE, varLabels = T  ,printToggle = FALSE, showAllLevels = TRUE)
## Save to a CSV file
write.csv(k08_int_2023, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/k08_int_2023.csv")

# additional variables
catvars <- c("catresult","shnprovin")

allvars <- c("catresult","shnprovin")

addmargins(table( kid_dhs_int$shnprovin, kid_dhs_int$catresult,useNA = "always"))

table(kid_dhs_int$sh310a)
table(kid_dhs_int$catresult)

##Weighted kids------------------------
# add weight variable
nrow(kid_dhs_int)
kid_dhs_int <- kid_dhs_int %>% filter(hv105 != "27")
nrow(kid_dhs_int)

kid_dhs_int$hh_weight <- as.numeric(kid_dhs_int$hv005)/1000000
library(survey)
library(srvyr)

kid_dhs_int <- kid_dhs_int %>% mutate(
  jaundice = case_when(
    sh318f == 1 ~ 1,
    sh318f == 0 ~ 0,
    is.nan(sh318f) ~ NA_real_))  
table(kid_dhs_int$jaundice, kid_dhs_int$hbvresult, useNA = "always")

# make survey design object
designf <-svydesign(ids=kid_dhs_int$hv001, strata=kid_dhs_int$hv022 , weights=kid_dhs_int$hh_weight,  data=kid_dhs_int)
options(survey.lonely.psu="adjust")
designf_dhs2 <-as_survey_design(designf)

# which HBV outcome var
table(kid_dhs_int$hbvresultlowna) # low/na results included as 0 (low/na = 46)
table(kid_dhs_int$hbvresult) # low/na dropped
table(kid_dhs_int$catresult) # categorical with low/na as own column

# basic stats
# overall weighted hbv prevalence among children with results
prop.table(svytable(~hbvresult, designf_dhs2))
svyciprop(~hbvresult, designf_dhs2, method="lo")

# antibodies: shroug, shorei, shrube
svytotal(~shroug, designf_dhs2, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
svytotal(~shorei, designf_dhs2, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
svytotal(~shrube, designf_dhs2, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()

svyby(~shroug,~shorei, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
svyby(~shroug,~shrube, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
table(Mumps=kid_dhs_int$shorei, Measles=kid_dhs_int$shroug)
svyby(~shorei,~shrube, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()
table(Mumps=kid_dhs_int$shorei, Rubella=kid_dhs_int$shrube, useNA = "always")

class(kid_dhs_int$shrube)
class(kid_dhs_int$shroug)
class(kid_dhs_int$shorei)
table(kid_dhs_int$shorei)
table(Mumps=kid_dhs_int$shorei, Rubella=kid_dhs_int$shrube, Measles=kid_dhs_int$shroug,useNA = "always")
kid_dhs_int %>% count(shroug,shorei,shrube)


# create functions to calculate weighted n
# running functions pastes the results to clipboard which you can then copy into excel

# counts for all n in dataset
survtable_all <- function(var){ 
  svytotal(as.formula(paste0('~', var)), designf_dhs2, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# counts for n in dataset, stratified by HBV Y or N
survtable <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresultlowna, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# mean for continous vars in dataset
survmean_all <- function(var){ 
  svymean(as.formula(paste0('~', var)),designf_dhs2, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# mean for continuos vars in dataset, stratified by HBV Y or N
survmean <- function(var){ 
  svyby(as.formula(paste0('~', var)),~hbvresultlowna, designf_dhs2, svymean, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# get results for vars of interest
survtable_all("catresult") # overall n

# continuous data: hhmem_n (number of household members), agenum (age)
survmean_all("agenum")
survmean("agenum")

survmean_all("hhmem_n") 
survmean("hhmem_n")

# categorical data
catvars <- c("catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net
# other variables of interest: hv026 (place of residence: 0=capital, large city, 1=small city, 2=town, 3=countryside, 9=missing)
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

# resuming weighted counts "hv025","hv270", "hv228", "pfldh_kids", hv105 (1-year age)

survtable_all("hv105") #
survtable("hv105") #

survtable_all("hv025") #
survtable("hv025") #
survtable_all("hv270") #
survtable("hv270") #

# children sleeping under bednet
survtable_all("hv228") #
survtable("hv228") # 

survtable_all("pfldh_kids") #
survtable("pfldh_kids") #

# other vars: shteta
table(kid_dhs_int$shteta, useNA = "always")

kid_dhs_int <- kid_dhs_int %>% mutate(
  shtetaindasno = case_when(
    shteta=="0" ~ 0,
    shteta=="1" ~ 1,
    shteta=="3" ~ 0),
  shtetaindasyes = case_when(
    shteta=="0" ~ 0,
    shteta=="1" ~ 1,
    shteta=="3" ~ 1))

table(kid_dhs_int$shtetaindasyes, useNA = "always")
table(kid_dhs_int$shtetaindasno, useNA = "always")
table(kid_dhs_int$hbvresult, useNA = "always")

survtable_all("shteta") 
survtable("shteta")
# nutritional status: sevstunt, modstunt, stunt,sevwasting, modwasting, wasting, underweight
survtable_all("hc57") # anemia
survtable("hc57")
survtable_all("stunt") 
survtable("stunt")
survtable_all("wasting") 
survtable("wasting")
survtable_all("underweight") 
survtable("underweight")
# age by province by hbv

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

# Weighted
adults2023int_hiv$hh_weight <- as.numeric(adults2023int_hiv$hv005)/1000000
adults2023int_hiv$hbvresult <- as.factor(adults2023int_hiv$hbvresult)
adults2023int_hiv$hiv03 <- as.factor(adults2023int_hiv$hiv03)

adults2023int_hiv$case5final <- as.numeric(adults2023int_hiv$case5final)

library(survey)
library(srvyr)

# make survey design object
designf_ad <-svydesign(ids=adults2023int_hiv$hv001, strata=adults2023int_hiv$hv022 , weights=adults2023int_hiv$hh_weight,  data=adults2023int_hiv)
options(survey.lonely.psu="adjust")
designf_dhs2_ad <-as_survey_design(designf_ad)

# basic stats
# overall weighted hbv prevalence among children with results
prop.table(svytable(~hbvresult, designf_dhs2_ad))
svyciprop(~hbvresult, designf_dhs2_ad, method="lo")

# which to have as columns - exposure status ("case")
table(adults2023int_hiv$hbvresult, adults2023int_hiv$case5final)
# create functions to calculate weighted n
# running functions pastes the results to clipboard which you can then copy into excel

# counts for all n in dataset
survtable_all_ad <- function(var){ 
  svytotal(as.formula(paste0('~', var)), designf_dhs2_ad, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# counts for n in dataset, stratified by HBV Y or N
survtable_ad <- function(var){ 
  svyby(as.formula(paste0('~', var)),~case5final, designf_dhs2_ad, svytotal, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# mean for continous vars in dataset
survmean_all_ad <- function(var){ 
  svymean(as.formula(paste0('~', var)),designf_dhs2_ad, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# mean for continuos vars in dataset, stratified by HBV Y or N
survmean_ad <- function(var){ 
  svyby(as.formula(paste0('~', var)),~case5final, designf_dhs2_ad, svymean, na.rm=T, survey.lonely.psu="adjust") %>% clipr::write_clip()
}

# get results for vars of interest
# overall
survtable_ad("case5final")
class(adults2023int_hiv$case5final)
survtable_all_ad("catresult") # overall n

# continuous data: hhmem_n (number of household members), agenum (age)
survtable_all_ad("agenum")
survmean_ad("agenum")

survmean_all_ad("hhmem_n") 
survmean_ad("hhmem_n")

# categorical data
catvars <- c("catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net



survtable_all_ad("hv104") # 1 male, 2 female
survtable_ad("hv104")

table(adults2023int$hv025)
survtable_all_ad("hv025") # hv025=urban(1)/rural(2)
survtable_ad("hv025")

survtable_all_ad("hv270") # wealth
survtable_ad("hv270")

table(adults2023int$pfldh)
survtable_all_ad("pfldh") # 
survtable_ad("pfldh")

survtable_all_ad("pv18s") # 
survtable_ad("pv18s")

survtable_all_ad("po18s") # 
survtable_ad("po18s")

survtable_all_ad("po18s") # 
survtable_ad("po18s")

survtable_all_ad("hbvresult") # 
survtable_ad("hbvresult")

survtable_all_ad("hiv03") # hiv
survtable_ad("hiv03")

survtable_all_ad("ha54") # 
survtable_ad("ha54")
table(adults2023int$ha54, adults2023int$hbvresult, adults2023int$hv104)

survtable_all_ad("ha57") # 
survtable_ad("ha57")
table(adults2023int$ha57, adults2023int$hbvresult)

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



# Map-------------------------
library(sf)
library(gstat)
library(stars)
library(tidyverse)
library(patchwork)
library(sp)

dhsmeta <- readRDS("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/DHS_pr_full_merge_backup.rds")

## kids map-----
dhsmeta_kids <- dhsmeta %>% filter(kids==1)
nrow(dhsmeta_kids)
class(dhsmeta_kids$geometry) # saved as character not list
head(dhsmeta_kids$geometry) # saved as character not list

# redo sf geometry
kidmapsf = st_as_sf(kid_dhs_int[!is.na(kid_dhs_int$latnum) &!is.na(kid_dhs_int$longnum),], coords = c("longnum", "latnum"), crs = 4326) 
# evaluate the 6997-6973 that didn't map
head(kidmapsf$geometry)
nogpskid <- kid_dhs_int %>% filter(!(cluster_hh %in% kidmapsf$cluster_hh)) %>% select(c("cluster_hh","latnum", "longnum","shnprovin","adm1fips","adm1name","catresult" ))

# subset cluster level df to get weights
output_df <- kid_dhs_int %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                  # npos = n(hbvresultlowna),
                   prev = mean(hbvresultlowna, na.rm=T)*100)

output_df <- merge(output_df, kid_dhs_int[,c("hh_weight","hv001")],by="hv001", all.x = TRUE)

# GADM boundaries from: https://gadm.org/download_country_v3.html
admin0 <- readRDS('/Users/camillem/Documents/GitHub/animalaria/admin0.rds') %>%          # GADM admin0 boundaries
  st_transform(4326) %>% # set at ESPG 4326
  filter(grepl('Congo|Rwanda|Tanzania|Burundi|African Republic|Angola|Zambia|Uganda|Sudan|Gabon|Cameroon|Equatorial Guinea', Country)) 

st_crs(admin0) # view CRS

DRC <- admin0 %>% filter(Country=='Democratic Republic of the Congo') # DRC


summary(kidmapsf$hh_weight)

table(kidmapsf$shteta)

output <- kidmapsf %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   npos = sum(hbvresultlowna==1),
                   prev = mean(hbvresultlowna, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)



# remove points where geometry is outside of DRC outline (geometry=c(0,0))
output_points <- st_join(output, DRC, join = st_intersects) %>% filter(!is.na(Country))

#A <- 
  ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=prev), alpha=0.8) + 
  labs(color='HBV prevalence \nin children < 5') + 
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

ggsave("./Plots/clusterhbvprev.png", width = 9, height = 6)

C <- 
  ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=tetcovlower), alpha=0.8) + 
  labs(color='Pentavalent vaccination \nin children < 5') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral', direction=1) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

# kriging using gstat: https://rpubs.com/nabilabd/118172 
# https://mgimond.github.io/Spatial/interpolation-in-r.html#generate-the-variance-and-confidence-interval-maps

# make variogram
m.vgm <- gstat::variogram(prev~1, output_points)

# fit a model to the sample variogram
# https://gisgeography.com/semi-variogram-nugget-range-sill/
m.fit <- gstat::fit.variogram(m.vgm, model=vgm(psill=480,"Exp",range=300, nugget=250))

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

# conduct kriging: Pf prev
m.kriged <- gstat::krige(prev~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
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


# conduct kriging: tetanus vacc - lower bound
m.kriged.tet <- gstat::krige(tetcovlower~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged.tet$var1.pred)

# assign points into bins
krige_tetlow <- m.kriged.tet %>% cbind(gdf$x, gdf$y) %>% mutate(
  var1.pred = cut(var1.pred, breaks=seq(0,90,by=10)), 
  se = sqrt(var1.var),
  se = cut(se, breaks=seq(0,24,by=4))) %>% filter(!is.na(var1.pred))

D <- 
  ggplot() + 
  geom_tile(data=(krige_tetlow %>% as.data.frame), aes(x=gdf.x ,y=gdf.y, fill=var1.pred)) + 
  geom_sf(data=admin0 %>% filter(ISO != 'COD'), fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  labs(fill="Predicted pentavalent \nvaccination", x='', y='') + 
  theme_bw(base_size=14) + 
  scale_fill_brewer(palette ="Spectral", direction=1, labels=c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90")) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

A + B + C + D + plot_layout(nrow=2, ncol = 2) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/draftmaps.png', width=15, height=9)

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
nrow(adults2023int_wdrop)


# redo sf geometry
adults2023int_dropsf = st_as_sf(adults2023int_wdrop[!is.na(adults2023int_wdrop$latnum) &!is.na(adults2023int_wdrop$longnum),], coords = c("longnum", "latnum"), crs = 4326) 
# check
head(adults2023int_dropsf$geometry)
nogpsad <- adults2023int_wdrop %>% filter(!(cluster_hh %in% adults2023int_dropsf$cluster_hh)) %>% dplyr::select(c("cluster_hh","latnum", "longnum","shnprovin","adm1fips","adm1name","catresult" ))

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

A1 <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_df_ad, aes(color=(adultprev)), alpha=0.8) + 
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
A1
#adult prev by case and control hhs
output_df_ad_case <- adults2023int_dropsf %>% filter(case5final == 1) %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   poscount = sum(hbvresult==1),
                   negcount = sum(hbvresult==0),
                   adultprev = mean(hbvresult, na.rm=T)*100)
output_df_ad_control <- adults2023int_dropsf %>% filter(case5final == 0) %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   poscount = sum(hbvresult==1),
                   negcount = sum(hbvresult==0),
                   adultprev = mean(hbvresult, na.rm=T)*100)
# prev among case hhs
B1 <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_df_ad_case[output_df_ad_case$adultprev > 0,], aes(color=(adultprev)), alpha=0.8) + 
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
  geom_sf(data=output_df_ad_control[output_df_ad_control$adultprev > 0,], aes(color=(adultprev)), alpha=0.8) + 
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


# Choropleth ------------------
drcprov = st_read("/Users/camillem/Documents/GitHub/hbv_hover/adm1/GLOBAL_ADM1.shp", stringsAsFactors = FALSE) %>% filter(ADM0_NAME=="DEMOCRATIC REPUBLIC OF THE CONGO") %>%   st_transform(4326)

wtdctskids <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/Results discussions/prov counts.xlsx",
                         sheet = "May 5")

wtdctskids$ADM1_NAME <- toupper(wtdctskids$provnamesimp)
drcprov_hbvkids <- left_join(drcprov,wtdctskids, by="ADM1_NAME")
view(drcprov_hbvkids)

ggplot()+
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=drcprov_hbvkids,  mapping=aes(fill=prev))+
  scale_fill_distiller(palette = 'Spectral') +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Weighted HBV prevalence in kids ≤5")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

# adults choropleth
casehh <- adults2023int_hiv %>% filter(case5final == 1)
controlhh <- adults2023int_hiv %>% filter(case5final == 0)

designf_adcase <-svydesign(ids=casehh$hv001, strata=casehh$hv022 , weights=casehh$hh_weight,  data=casehh)
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

