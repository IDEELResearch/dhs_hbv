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

#Children 60-71 months (5yos)-----------------
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

#Supp table comparing those in survey and those studied for hepB------------------------------
# compare n=8547 to n=5679
# compare unweighted and weighted?

# catvars <- c("hbvresult5","hbvresult1", "hbvresult2", "hbvresult100","hv104", "sex", "hc1", "hv105", "hv105_fromhc1_f", 
#             "hv024", "hv025","hv026","hv270", "urbanrural", "location", "wealth", "pfmalaria", "tetab",
#             "pfldh_kids","shtetaindasno","shtetaindasyes","prov2015") 

# put labeled variables on elig_kids_2 df
elig_kids_2 <- elig_kids_2 %>% mutate(
  # new age in years based on months variable
    hv105_fromhc1 = case_when(
      hc1 < 12 ~ 0, # kids below 6mo = 0 years
      hc1 >= 12 & hc1 <24 ~ 1, #  kids 12-<24mo = 1 years
      hc1 >= 24 & hc1 <36 ~ 2, # kids 24=<36 = 2 years
      hc1 >= 36 & hc1 <48 ~ 3, # kids 36-<48 = 3 years
      hc1 >= 48 & hc1 <60 ~ 4, # kids 48-<60 = 4 years
      hc1 >= 60 ~ 5), # kids >=60mo = 5years
    hv105_fromhc1_f = as.factor(hv105_fromhc1),
    sex=factor(hv104, 
               levels = c(1, 2),
               labels = c("Male", "Female")),    
    reltoheadhh=factor(hv101, 
                       levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,98),
                       labels = c("Head", "Spouse","Son/daughter","Son/daughter-in-law","Grandchild","Parent","In-laws","Brother/sister","Co-spouse","Other","Adopted/in custody","Not related","Nephew/niece","Nephew/niece by marriage","Don't know")),
    reltoheadhh_simp = case_when(
      hv101 == "3" ~ "Child", # son/daughter
      hv101 == "5" ~ "Grandchild", # grandchild
      TRUE ~ "Other"), # all others since so few counts in each
    prov2015=factor(shnprovin, 
                    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
                    labels = c("Kinshasa", "Kwango","Kwilu","Mai-Ndombe","Kongo Central","Equateur","Mongala","Nord-Ubangi","Sud-Ubangi","Tshuapa","Kasai","Kasai-Central","Kasai-Oriental","Lomami","Sankuru","Haut-Katanga","Haut-Lomami","Lualaba","Tanganyka","Maniema","Nord-Kivu","Bas-Uele","Haut-Uele","Ituri","Tshopo","Sud-Kivu")
    ),
    location = factor(hv026,
                      levels = c(0, 1, 2, 3),
                      labels = c("Capital", "Small city", "Town", "Countryside")),
    urbanrural = factor(hv025,
                        levels = c(1,2),
                        labels = c("Urban", "Rural")),
    wealth = factor(hv270,
                    levels = c(1,2,3,4,5),
                    labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    pfmalaria = factor(pfldh_kids,
                       levels = c(0,1),
                       labels = c("Pf-negative", "Pf-positive")),
    tetab = factor(shteta,
                   levels = c(0,1,3),
                   labels = c("Nonreactive", "Reactive", "Indeterminate"))
)

# weight data: which to use -- use household one
elig_kids_2$hv028 <- as.numeric(elig_kids_2$hv028)
elig_kids_2$hv028_div <- elig_kids_2$hv028/1000000
elig_kids_2$hh_weight <- as.numeric(elig_kids_2$hv005)/1000000

summary(elig_kids_2$hv028_div)
summary(elig_kids_2$hh_weight)

elig_kids_2$both_wt_old <- (elig_kids_2$iptw_s)*(elig_kids_2$hh_weight)
elig_kids_2$both_wt_new <- (elig_kids_2$iptw_s)*(elig_kids_2$hv028_div)
summary(elig_kids_2$both_wt_old)
summary(elig_kids_2$both_wt_new)

elig_kids_2 <- elig_kids_2 %>% mutate_at(c('hv009', 'hv014'), as.numeric)

## add KR variables------
elig_kids_2$clus_hh_ind <-  paste(elig_kids_2$hv001, elig_kids_2$hv002, elig_kids_2$hvidx, sep = "_")
elig_kids_2_wt_kr <- left_join(elig_kids_2, d_k_or_red, by = "clus_hh_ind" )

elig_kids_2_wt_kr <- elig_kids_2_wt_kr %>% mutate_at(c('hc57'), as.numeric)

elig_kids_2_wt_kr <- elig_kids_2_wt_kr %>% mutate(
  #DPT vaccination (diptheria/pertussis/tetanus, which has been replaced with pentavalent that includes HBV)
  anemia = ifelse(!is.nan(hc57), 5 - hc57, NA_real_),
  anemia_f = case_when(
    anemia == "1" ~ "Mild-to-none", 
    anemia == "2" ~ "Mild-to-none", 
    anemia == "3" ~ "Moderate-to-severe", 
    anemia ==  "4" ~ "Moderate-to-severe", 
    is.na(anemia) ~ "Not available"),
  modstunt = case_when(
    hc70 < -200 ~ 1, # mod-to-severe stunting defined as ht/age  <2 SDs; hc70 has been multipled by 100
    hc70 >= -200 & hc70 < 2000 ~ 0, # not moderate; excludes the 9997 and up that are out of range
    hc70 >=2000 ~ 9, # set to 9 to include as separate category if desired
    is.nan(hc70) ~ NA_real_, # set NaNs to missing
    TRUE ~ NA_real_),
  modstunt_f = case_when(
    modstunt == 0 ~ "No stunting",
    modstunt == 1 ~ "Moderate-to-severe",
    modstunt == 9 ~ "Not available",
    is.na(modstunt) ~ "Not available"),
  dpt1 = case_when(
    h3 == 0 ~ 0, # did not receive DPT1 (reported as no and not on vac card)
    h3 == 8 ~ 0, # assign don't know as didn't receive
    h3 > 0 & h3 < 8 ~ 1, #h3==1 is vacc date on card, 2 is reported by mother, 3 is vacc on card
    is.na(h3) ~ NA_real_), # these are the values to impute - need help on this
  dpt2 = case_when(
    h5 == 0 ~ 0, # did not receive DPT1 (reported as no and not on vac card)
    h5 == 8 ~ 0, # assign don't know as didn't receive
    h5 > 0 & h5 < 8 ~ 1, #h3==1 is vacc date on card, 2 is reported by mother, 3 is vacc on card
    is.na(h5) ~ NA_real_), # these are the values to impute - need help on this
  dpt3 = case_when(
    h7 == 0 ~ 0, # did not receive DPT1 (reported as no and not on vac card)
    h7 == 8 ~ 0, # assign don't know as didn't receive
    h7 > 0 & h7 < 8 ~ 1, #h3==1 is vacc date on card, 2 is reported by mother, 3 is vacc on card
    is.na(h7) ~ NA_real_), # these are the values to impute - need help on this
  dpt_count = dpt1 + dpt2 + dpt3,
  dpt_doses = case_when(
    dpt_count==3 ~ 2, # reported or noted as received for all
    dpt_count==1 | dpt_count==2  ~ 1, # received 1 or 2 doses
    dpt_count==0 ~ 0), # received none
  dpt_any = case_when(
    dpt_count == 0 ~ 0, # no doses
    dpt_count > 0 ~ 1), # any doses
  dpt_doses_f = case_when(
    dpt_doses == 0 ~ "No doses received",
    dpt_doses == 1 ~ "Series incomplete",
    dpt_doses == 2 ~ "Series completed",
    is.na(dpt_doses) ~ "Not available"),
  dpt_any_f = factor(dpt_any,
                     levels = c(0,1),
                     labels = c("No doses", "Series initiated")),
  #2.  injections: h15[x] vars - very few responses so skipping; using v477 (count of injections received)
  injec = case_when(
    v477==0 ~ 0, # no injections in last 12 mo
    v477 > 0 & v477 <13 ~ 1, # 1-12 injections 
    v477 >= 13  & v477 <25 ~ 2, # 13-24 injections 
    v477 >= 25 ~ 3), # >=25 injections
  # 3. beating justified - any of the five questions answered yes/don't know (v744a=wife goes out without telling husband; v744b=wife neglects children; v744c=wife argues with husband; v744d=wife refuses sex with husband; v744e=wife burns food)
  beat = case_when(
    v744a > 0 | v744b > 0 | v744c > 0 | v744d > 0 | v744e > 0 ~ 1, 
    v744a == 0 | v744b ==  0 | v744c == 0 | v744d == 0 | v744e == 0 ~ 0
  ),
  injec_f = case_when(
    injec == 0 ~ "None",
    injec == 1 ~ "1-12",
    injec == 2 ~ "13-24",
    injec == 3 ~ "25+",
    is.na(injec) ~ "Not available"),
  beat_f = case_when(
    beat == 0 ~ "Never justified",
    beat == 1 ~ "Justified",
    is.na(beat) ~ "Not available"))

elig_kids_2_wt_kr <- elig_kids_2_wt_kr %>% mutate_at(c('v480', 'injec','beat'), as.factor)
# check new variables
elig_kids_2_wt_kr %>% group_by(dpt1, dpt2, dpt3, dpt_count, dpt_doses) %>% count()
elig_kids_2_wt_kr %>% group_by(v477, injec) %>% count()
elig_kids_2_wt_kr %>% group_by(v744a, v744b, v744c, v744d, v744e, beat) %>% count() %>% print(n=Inf)
elig_kids_2_wt_kr <- elig_kids_2_wt_kr %>% mutate(syringe = case_when(is.na(v480) ~ 0, !is.na(v480) ~ 1))

##make survey design object--------
designall <-svydesign(ids=elig_kids_2_wt_kr$cluster_hh, strata=elig_kids_2_wt_kr$hv022 , weights=elig_kids_2_wt_kr$hv028_div,  data=elig_kids_2_wt_kr) # or both_wt_new, both_wt_old - no should only be household weight
designall2 <-as_survey_design(designall)

# counts for all n in dataset
survtable_all <- function(var){ 
  svytotal(as.formula(paste0('~', var)), designall2, na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()
}
# mean for continous vars in dataset
survmean_all <- function(var){ 
  svymean(as.formula(paste0('~', var)),designall2, na.rm=T, survey.lonely.psu="adjust") #%>% clipr::write_clip()
}

###numeric vars-----
age_num_s <- as.data.frame(survmean_all("hc1")) %>% rownames_to_column(var = "covname")  %>%  
  mutate(levels = "Age (months), mean (SD)") %>% rename(se = hc1)
hhmem_s <- as.data.frame(survmean_all("hv009")) %>% rownames_to_column(var = "covname")  %>%  
  mutate(levels = "Household members, mean (SD)") %>% rename(se = hv009)
kids_s <- as.data.frame(survmean_all("hv014")) %>% rownames_to_column(var = "covname")  %>%  
  mutate(levels = "Children in household, mean (SD)") %>% rename(se = hv014)
avgs_s <- dplyr::bind_rows(list(age_num_s, hhmem_s, kids_s), .id = 'source') %>% mutate(supptab8k = paste(round(mean,1),' (',round(se,2),')', sep = ""))
SuppTab1_num_8547 <- avgs_s %>% select(-c(source, mean,se ))
view(SuppTab1_num_8547)

###cat vars------
# Age
age_s <- as.data.frame(survtable_all("hv105_fromhc1_f")) %>% rownames_to_column(var = "covname") %>%  mutate(levels = str_split_fixed(covname, "hv105_fromhc1_f", 2)[,2], cov = "hv105_fromhc1_f") 
# Sex 0=female, 1=male
sex_s <- as.data.frame(survtable_all("sex")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "sex", 2)[,2], cov = "sex") 
# relationship to head of household
relhh_s <- as.data.frame(survtable_all("reltoheadhh_simp")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "reltoheadhh_simp", 2)[,2], cov = "reltoheadhh_simp") 
# urban rural:  hv025=urban(1)/rural(2)
urbrur_s <- as.data.frame(survtable_all("urbanrural")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "urbanrural", 2)[,2], cov = "urbanrural") 
# type of location hv026: 0=capital (provincial); 1=small city; 2=town; 3=countryside
location_s <- as.data.frame(survtable_all("location")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "location", 2)[,2], cov = "location")  
# province
prov_s <- as.data.frame(survtable_all("prov2015")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "prov2015", 2)[,2], cov = "prov2015")  
# household wealth
wealth_s <- as.data.frame(survtable_all("wealth")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "wealth", 2)[,2], cov = "wealth") 
#pf malaria
pfmal_s <- as.data.frame(survtable_all("pfmalaria")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "pfmalaria", 2)[,2], cov = "pfmalaria") 
# tetanus - variable from dataset
tetorig_s <- as.data.frame(survtable_all("tetab")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "tetab", 2)[,2], cov = "tetab") 
anem_s <- as.data.frame(survtable_all("anemia_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "anemia_f", 2)[,2], cov = "anemia_f") 
modstu_s <- as.data.frame(survtable_all("modstunt_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "modstunt_f", 2)[,2], cov = "modstunt_f") 
dpt_dos_s <- as.data.frame(survtable_all("dpt_doses_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "dpt_doses_f", 2)[,2], cov = "dpt_doses_f") 
inje_s <- as.data.frame(survtable_all("injec_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "injec_f", 2)[,2], cov = "injec_f") 
beat_s <- as.data.frame(survtable_all("beat_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "beat_f", 2)[,2], cov = "beat_f") 
reused_s <- as.data.frame(survtable_all("v480")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "v480", 2)[,2], cov = "v480") 

all_tot_s <- dplyr::bind_rows(list(age_s, sex_s, relhh_s, urbrur_s, location_s, prov_s, wealth_s, pfmal_s, tetorig_s, anem_s, modstu_s, dpt_dos_s, inje_s, beat_s, reused_s), .id = 'source') #tetlower, tetupper,
all_tot_s <- all_tot_s %>% filter(total>0) %>% select(-SE)
all_tot_s <- all_tot_s %>% group_by(source) %>% mutate(totperc = 100*(total / sum(total)))  %>% ungroup()
all_tot_s <- all_tot_s %>% rename(total_8k = total, totperc_8k = totperc) %>% select(c("covname", "levels", "total_8k", "totperc_8k"))
view(all_tot_s)

options(survey.lonely.psu="adjust")



# now for analysis dataset
designf <-svydesign(ids=elig_kids_whbvres_wt_kr$hv001, strata=elig_kids_whbvres_wt_kr$hv022 , weights=elig_kids_whbvres_wt_kr$hv028_div,  data=elig_kids_whbvres_wt_kr) # both_wt_old, both_wt_new
designf_dhs2 <-as_survey_design(designf)
# counts for all n in dataset
survtable_all <- function(var){ 
  svytotal(as.formula(paste0('~', var)), designf_dhs2, na.rm=T, survey.lonely.psu="adjust") # %>% clipr::write_clip()
}
# mean for continous vars in dataset
survmean_all <- function(var){ 
  svymean(as.formula(paste0('~', var)),designf_dhs2, na.rm=T, survey.lonely.psu="adjust") #%>% clipr::write_clip()
}
age_num_s2 <- as.data.frame(survmean_all("hc1")) %>% rownames_to_column(var = "covname")  %>%  
  mutate(levels = "Age (months), mean (SD)") %>% rename(se = hc1)
hhmem_s2 <- as.data.frame(survmean_all("hv009")) %>% rownames_to_column(var = "covname")  %>%  
  mutate(levels = "Household members, mean (SD)") %>% rename(se = hv009)
kids_s2 <- as.data.frame(survmean_all("hv014")) %>% rownames_to_column(var = "covname")  %>%  
  mutate(levels = "Children in household, mean (SD)") %>% rename(se = hv014)
avgs_s2 <- dplyr::bind_rows(list(age_num_s2, hhmem_s2, kids_s2), .id = 'source') %>% mutate(supptab5k = paste(round(mean,1),' (',round(se,2),')', sep = ""))

SuppTab1_num_5679 <- avgs_s2 %>% select(-c(source, mean,se ))
view(SuppTab1_num_5679)

# Age
agex_s2 <- as.data.frame(survtable_all("hv105_fromhc1_f")) %>% rownames_to_column(var = "covname") %>%  mutate(levels = str_split_fixed(covname, "hv105_fromhc1_f", 2)[,2], cov = "hv105_fromhc1_f") 
# Sex 0=female, 1=male
sex_s2 <- as.data.frame(survtable_all("sex")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "sex", 2)[,2], cov = "sex") 
# relationship to head of household
relhh_s2 <- as.data.frame(survtable_all("reltoheadhh_simp")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "reltoheadhh_simp", 2)[,2], cov = "reltoheadhh_simp") 
# urban rural:  hv025=urban(1)/rural(2)
urbrur_s2 <- as.data.frame(survtable_all("urbanrural")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "urbanrural", 2)[,2], cov = "urbanrural") 
# type of location hv026: 0=capital (provincial); 1=small city; 2=town; 3=countryside
location_s2 <- as.data.frame(survtable_all("location")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "location", 2)[,2], cov = "location")  
# province
prov_s2 <- as.data.frame(survtable_all("prov2015")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "prov2015", 2)[,2], cov = "prov2015")  
# household wealth
wealth_s2 <- as.data.frame(survtable_all("wealth")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "wealth", 2)[,2], cov = "wealth") 
#pf malaria
pfmal_s2 <- as.data.frame(survtable_all("pfmalaria")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "pfmalaria", 2)[,2], cov = "pfmalaria") 
# tetanus - variable from dataset
tetorig_s2 <- as.data.frame(survtable_all("tetab")) %>% rownames_to_column(var = "covname") %>% mutate(levels = str_split_fixed(covname, "tetab", 2)[,2], cov = "tetab") 
anem_s2 <- as.data.frame(survtable_all("anemia_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "anemia_f", 2)[,2], cov = "anemia_f") 
modstu_s2 <- as.data.frame(survtable_all("modstunt_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "modstunt_f", 2)[,2], cov = "modstunt_f") 
dpt_dos_s2 <- as.data.frame(survtable_all("dpt_doses_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "dpt_doses_f", 2)[,2], cov = "dpt_doses_f") 
inje_s2 <- as.data.frame(survtable_all("injec_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "injec_f", 2)[,2], cov = "injec_f") 
beat_s2 <- as.data.frame(survtable_all("beat_f")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "beat_f", 2)[,2], cov = "beat_f") 
reused_s2 <- as.data.frame(survtable_all("v480")) %>% rownames_to_column(var = "covname") %>% 
  mutate(levels = str_split_fixed(covname, "v480", 2)[,2], cov = "v480") 

all_tot_s2 <- dplyr::bind_rows(list(age_s2, sex_s2, relhh_s2, urbrur_s2, location_s2, prov_s2, wealth_s2, pfmal_s2, tetorig_s2, anem_s2, modstu_s2, dpt_dos_s2, inje_s2, beat_s2, reused_s2), .id = 'source') #tetlower, tetupper,
all_tot_s2 <- all_tot_s2 %>% filter(total>0) %>% select(-SE)
all_tot_s2 <- all_tot_s2 %>% group_by(source) %>% mutate(totperc = 100*(total / sum(total)))  %>% ungroup()
all_tot_s2 <- all_tot_s2 %>% rename(total_5k = total, totperc_5k = totperc) %>% select(c("covname", "levels", "total_5k", "totperc_5k"))

all_supp <- merge(all_tot_s, all_tot_s2, by = c("covname", "levels"))
view(all_supp)

supptab1 <- all_supp %>% mutate_if(is.numeric, round, 0) %>% mutate(supptab8k = paste(total_8k,' (',totperc_8k,')',  sep = ""), supptab5k = paste(total_5k,' (',totperc_5k,')',  sep = ""))
supptab1_simp <- supptab1 %>% select(c( "covname","levels", "supptab8k", "supptab5k"))
view(supptab1_simp)

#add numeric vars
SuppTab1_num <- merge(SuppTab1_num_8547, SuppTab1_num_5679, by = c("covname","levels"))
supptab1_simpa <- rbind(SuppTab1_num, supptab1_simp)
view(supptab1_simpa)
# Export simplified Table 1------
write.csv(supptab1_simpa, file = here("Data", "supptab_8kcomp.csv"))





