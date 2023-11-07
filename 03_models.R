# 03_models
options(scipen = 999)
options(scipen = 0)
options(scipen = 100, digits = 3)
# kids---------------------
# add weight variable
kid_dhs_int$hh_weight <- as.numeric(kid_dhs_int$hv005)/1000000
library(survey)
library(srvyr)
library(tidyverse)
library(broom)

kid_dhs_int <- kid_dhs_int %>% mutate(
  jaundice = case_when(
    sh318f == 1 ~ 1,
    sh318f == 0 ~ 0,
    is.nan(sh318f) ~ NA_real_))  
table(kid_dhs_int$jaundice, kid_dhs_int$hbvresult, useNA = "always")

kid_hbv_kr_dis$modstunt <- ifelse(is.na(kid_hbv_kr_dis$modstunt), 9, kid_hbv_kr_dis$modstunt)
kid_hbv_kr_dis$modwasting <- ifelse(is.na(kid_hbv_kr_dis$modwasting), 9, kid_hbv_kr_dis$modwasting)

kid_hbv_kr_dis <- kid_hbv_kr_dis %>% mutate(stunt_mod = case_when(
      modstunt=="1" ~ "0",
      modstunt=="2" ~ "1",
      modstunt=="3" ~ "9",
      modstunt=="9" ~ "9"),
    wast_mod = case_when(
      modwasting=="1" ~ "0",
      modwasting=="2" ~ "1",
      modwasting=="3" ~ "9",
      modwasting=="9" ~ "9")
  )
table(kid_hbv_kr_dis$wast_mod, kid_hbv_kr_dis$modwasting)
table(kid_hbv_kr_dis$stunt_mod, kid_hbv_kr_dis$modstunt)

# Outcome vars: missingness and sens analyses------------------
# hbvresultlowna: 46 low vol counted as negative, S/CO = 5
# hbvresult: 46 as missing, S/CO = 5
# hbvresultlowpos: 46 as pos, S/CO = 5

# using the S/Co=5, put 46 low vol as pos
kid_hbv_kr_dis$hbvresultlowpos <- ifelse(kid_hbv_kr_dis$catresult=="low vol",1,kid_hbv_kr_dis$hbvresult)

table(kid_hbv_kr_dis$hbvresult, kid_hbv_kr_dis$hbvresultlowna, useNA = "always")
table(kid_hbv_kr_dis$hbvresult, kid_hbv_kr_dis$catresult, useNA = "always")
table(kid_hbv_kr_dis$hbvresultlowna, kid_hbv_kr_dis$catresult, useNA = "always")
table(kid_hbv_kr_dis$hbvresultlowpos, kid_hbv_kr_dis$catresult, useNA = "always")

k_df <- dhsmeta %>% filter(kids_barcode != '') %>% select(kids_barcode, hv001, hv002)
view(k_df)
k_df <- k_df %>% distinct(kids_barcode, .keep_all = TRUE)

k_df <- k_df %>% mutate(dbs999 = if_else(startsWith(kids_barcode, "999") | startsWith(kids_barcode, "?"), 1, 0))
table(k_df$dbs999)

knohbres <- k_df %>% filter(!(kids_barcode %in% kid_hbv_kr_dis$kids_barcode))
nrow(k_df) - nrow(knohbres)
nrow(kid_hbv_kr_dis)
# discuss with Jess about what to do with the 2908 - impute, leave out, etc.
# for now proceed with bounding the 46 with low vol

# save char vars as factors before survey design
kid_hbv_kr_dis$provgrp <- as.factor(kid_hbv_kr_dis$provgrp)
kid_hbv_kr_dis$provgrp_kin <- as.factor(kid_hbv_kr_dis$provgrp_kin)
kid_hbv_kr_dis$dpt_doses <- as.factor(kid_hbv_kr_dis$dpt_doses)
kid_hbv_kr_dis$injec <- as.factor(kid_hbv_kr_dis$injec)

# make char sex variable comparing Male to Female
kid_hbv_kr_dis <- kid_hbv_kr_dis %>% # 1=male, 2=female but make 0=female, 1=male
  dplyr::mutate(sex=case_when(
    hv104 == "1" ~ "1",
    hv104 == "2" ~ "0"
  ))


# make survey design object
designf <-svydesign(ids=kid_hbv_kr_dis$hv001, strata=kid_hbv_kr_dis$hv022 , weights=kid_hbv_kr_dis$hh_weight,  data=kid_hbv_kr_dis)
options(survey.lonely.psu="adjust")
designf_dhs2 <-as_survey_design(designf)

# vars to run - c("hbvresult","catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net

# run for hbvresultlowna and hbvresultlowpos as outcome vars

# age
kid_age <- svyglm(hbvresultlowpos ~ as.factor(hv105), designf_dhs2, family=quasibinomial("log"))
kid_age <- svyglm(hbvresultlowpos ~ as.factor(hv105), designf_dhs2, family=quasibinomial("identity"))
summary(kid_age)
confint(kid_age)

# sex
kid_mf <- svyglm(hbvresultlowna ~ as.factor(hv104), designf_dhs2, family=quasibinomial("log"))
kid_mf <- svyglm(hbvresultlowna ~ as.factor(hv104), designf_dhs2, family=quasibinomial("identity"))
summary(kid_mf)
confint(kid_mf)

# urban rural
kid_urb <- svyglm(hbvresultlowna ~ as.factor(hv025), designf_dhs2, family=quasibinomial("log"))
kid_urb <- svyglm(hbvresultlowna ~ as.factor(hv025), designf_dhs2, family=quasibinomial("identity"))
summary(kid_urb)
confint(kid_urb)

# large/small city/town/countryside
kid_loc <- svyglm(hbvresultlowna ~ as.factor(hv026), designf_dhs2, family=quasibinomial("log"))
kid_loc <- svyglm(hbvresultlowna ~ relevel(as.factor(hv026), ref="2"), designf_dhs2, family=quasibinomial("identity"))
summary(kid_loc)
confint(kid_loc)

# wealth
kid_weal <- svyglm(hbvresult ~ as.factor(hv270), designf_dhs2, family=quasibinomial("log"))
kid_weal <- svyglm(hbvresult ~ as.factor(hv270), designf_dhs2, family=quasibinomial("identity"))
summary(kid_weal)
confint(kid_weal)

# wealth
kid_pf <- svyglm(hbvresult ~ as.factor(pfldh_kids), designf_dhs2, family=quasibinomial("log"))
kid_pf <- svyglm(hbvresult ~ as.factor(pfldh_kids), designf_dhs2, family=quasibinomial("identity"))
summary(kid_pf)
confint(kid_pf)

table(kid_dhs_int$pfldh_kids, kid_dhs_int$hbvresult)

# jaundice
kid_jau <- svyglm(hbvresult ~ as.factor(jaundice), designf_dhs2, family=quasibinomial("log"))
kid_jau <- svyglm(hbvresult ~ as.factor(jaundice), designf_dhs2, family=quasibinomial("identity"))
summary(kid_jau)
confint(kid_jau)
table(kid_dhs_int$jaundice, kid_dhs_int$hbvresult)

# jaundice
kid_tet <- svyglm(hbvresult ~ as.factor(shteta), designf_dhs2, family=quasibinomial("log"))
kid_tet <- svyglm(hbvresult ~ as.factor(shteta), designf_dhs2, family=quasibinomial("identity"))
summary(kid_tet)
confint(kid_tet)

# province - shnprovin too many groups, provgrp_kin (8 levels), provgrp (7 levels)
kid_prov <- svyglm(hbvresult ~ as.factor(provgrp), designf_dhs2, family=quasibinomial("log"))
kid_prov <- svyglm(hbvresult ~ as.factor(provgrp_kin), designf_dhs2, family=quasibinomial("identity")) # not converging
summary(kid_prov)
confint(kid_prov)

# code var for other kid pos in hh
kid_clust <- svyglm(hbvresult ~ as.factor(totalkidpos_f), designf_dhs2, family=quasibinomial("log"))
kid_clust <- svyglm(hbvresult ~ as.factor(totalkidpos_f), designf_dhs2, family=quasibinomial("identity"))
summary(kid_clust)
confint(kid_clust)

# adjusted?
kid_adj <- svyglm(hbvresult ~  as.factor(shteta) + as.factor(pfldh_kids), designf_dhs2, family=quasibinomial("log")) # 
summary(kid_adj)
confint(kid_adj)


inj <- svyglm(hbvresultlowna ~ totalkidpos_f, designf_dhs2, family=quasibinomial("identity"))
summary(inj)
confint(inj)

# Frequencies
vars <- c("hv105", 'sex','hv025','hv026','hv270','jaundice',"stunt_mod", "wast_mod", 'pfldh_kids','shtetaindasno', "dpt_doses", "dpt_count", "injec", "beat", "provgrp_kin")
addmargins(table(kid_hbv_kr_dis$hv105, useNA = "always"))
addmargins(table(kid_hbv_kr_dis$wast_mod, useNA = "always"))
n=nrow(kid_hbv_kr_dis)
1372/n
addmargins(table(kid_hbv_kr_dis$beat, useNA = "always"))
1831/n
addmargins(table(kid_hbv_kr_dis$dpt_doses, useNA = "always"))
1843/n
addmargins(table(kid_hbv_kr_dis$hv270, useNA = "always"))
1833/n
13/n
6851+46
# as function----
vars <- c("hv105", 'sex','hv025','hv026','hv270','jaundice',"stunt_mod", "wast_mod", 'pfldh_kids','shtetaindasno', "dpt_doses", "dpt_count", "injec", "beat", "provgrp_kin")

bivs_asneg <- function(var){ # glm function
  m <- svyglm(as.formula(paste0('hbvresultlowna ~', var)), designf_dhs2, family=quasibinomial("identity"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}

bivs_aspos <- function(var){ # glm function
  m <- svyglm(as.formula(paste0('hbvresultlowpos ~', var)), designf_dhs2, family=quasibinomial("identity"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}

glmresults_low <- map_dfr(vars,bivs_asneg) 
glmresults_low %>% print(noSpaces=T) 

glmresults_up <- map_dfr(vars,bivs_aspos) 
glmresults_up %>% print(noSpaces=T) 

glmresults_low$bound <- "Lower"
glmresults_up$bound <- "Upper"

glmresults <- rbind(glmresults_low, glmresults_up)
view(glmresults)

glmresults$pd100 <- glmresults$estimate*100
glmresults$pdcilow100 <- glmresults$`2.5 %`*100
glmresults$pdciup100 <- glmresults$`97.5 %`*100

glmresults <- glmresults %>%  
  mutate(variable = case_when( # can ignore case with grepl("re", term, ignore.case = TRUE) ~"Group2"))
    grepl("hv270", term) ~ "Wealth",
    grepl("provgrp_kin", term) ~"Region",
    grepl("hv026", term) ~"Location",
    grepl("hv105", term) ~"Age",
    grepl("dpt", term) ~"DPT vaccine",
    grepl("beat", term) ~"Beating justified",
    grepl("hv025", term) ~"Rurality",
    grepl("sex", term) ~"Sex",
    grepl("injec", term) ~"Injections",
    grepl("pfldh_kids", term) ~"Pf",
    grepl("jaundice", term) ~"Jaundice",
    grepl("tet", term) ~"TetAb",
    grepl("mod", term) ~"Anthropometry", # stunting and wasting together
  ),
  caretakesubset = case_when( # variables only asked to caretakers/mothers of children sampled
    variable %in% c("Anthropometry","DPT vaccine", "Injections","Beating justified") ~ 1,
    TRUE ~ 0
  )) 
table(glmresults$caretakesubset)

rownames(glmresults) <- 1:nrow(glmresults)
glmresults <- glmresults %>% arrange(variable, term)
glmresults <- glmresults  %>%  mutate(desorder=row_number())

table(glmresults$term)
view(glmresults)

glmresults$term[glmresults$term == "beat"] <- "Beating justified vs not"
#glmresults$term[glmresults$term == "dpt_count"] <- "# of DPT doses (continuous)"
glmresults$term[glmresults$term == "dpt_count"] <- "Num of DPT doses (continuous)"
glmresults$term[glmresults$term == "dpt_doses1"] <- "DPT initiated (1-2 doses) vs not started (0 doses)"
glmresults$term[glmresults$term == "dpt_doses2"] <- "DPT series complete (3 doses) vs not started (0 doses)"
glmresults$term[glmresults$term == "hv0252"] <- "Rural vs urban"
glmresults$term[glmresults$term == "hv0261"] <- "Small city vs capital"
glmresults$term[glmresults$term == "hv0262"] <- "Town vs capital"
glmresults$term[glmresults$term == "hv0263"] <- "Countryside vs capital"
glmresults$term[glmresults$term == "hv1042"] <- "Female vs male"
glmresults$term[glmresults$term == "sex1"] <- "Male vs Female"
glmresults$term[glmresults$term == "hv1051"] <- "1 year vs <1 year"
glmresults$term[glmresults$term == "hv1052"] <- "2 years vs <1 year"
glmresults$term[glmresults$term == "hv1053"] <- "3 years vs <1 year"
glmresults$term[glmresults$term == "hv1054"] <- "4 years vs <1 year"
glmresults$term[glmresults$term == "hv1055"] <- "5 years vs <1 year"
glmresults$term[glmresults$term == "hv2702"] <- "Poorer vs poorest"
glmresults$term[glmresults$term == "hv2703"] <- "Middle vs poorest"
glmresults$term[glmresults$term == "hv2704"] <- "Richer vs poorest"
glmresults$term[glmresults$term == "hv2705"] <- "Richest vs poorest"
glmresults$term[glmresults$term == "injec1"] <- "1-12 injections vs none"
glmresults$term[glmresults$term == "injec2"] <- "12-24 injections vs none"
glmresults$term[glmresults$term == "injec3"] <- "≥25 injections"
glmresults$term[glmresults$term == "jaundice"] <- "Jaundice vs not"
glmresults$term[glmresults$term == "pfldh_kids1"] <- "Pf malaria+ vs -"
glmresults$term[glmresults$term == "provgrp_kin2"] <- "Kongo Central/Bandundu vs Kinshasa"
glmresults$term[glmresults$term == "provgrp_kin3"] <- "Equateur vs Kinshasa"
glmresults$term[glmresults$term == "provgrp_kin4"] <- "Kasais vs Kinshasa"
glmresults$term[glmresults$term == "provgrp_kin5"] <- "Katanga vs Kinshasa"
glmresults$term[glmresults$term == "provgrp_kin6"] <- "Orientale vs Kinshasa"
glmresults$term[glmresults$term == "provgrp_kin7"] <- "Kivus vs Kinshasa"
glmresults$term[glmresults$term == "provgrp_kin8"] <- "Maniema vs Kinshasa"
glmresults$term[glmresults$term == "shtetaindasno"] <- "Tetanus Ab+ vs -"
glmresults$term[glmresults$term == "stunt_mod1"] <- "Mod-to-severe stunting vs none"
glmresults$term[glmresults$term == "stunt_mod9"] <- "Missing stunting vs none"
glmresults$term[glmresults$term == "wast_mod1"] <- "Mod-to-severe wasting vs none"
glmresults$term[glmresults$term == "wast_mod9"] <- "Missing wasting vs none"


ggplot(glmresults, aes(x=term, y=pd100)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=pd100, ymin=pdcilow100, ymax=pdciup100), shape=15, size=0.8, color="black", show.legend=F, fatten=0.2, position=position_dodge2(width = 1.0) ) + 
  geom_point(shape=15, size=5, aes(color=variable, group=bound, alpha=bound), position=position_dodge2(width = 1.0) , show.legend=T) + #alpha=0.9
  #scale_color_brewer(palette = "Dark2")+
  coord_flip() + theme_bw() +
  scale_alpha_discrete(range = c(0.35, 0.9))+
  #scale_x_continuous(trans = "reverse") + 
  labs(x="", y="Unadjusted prevalence difference per 100 kids") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank()) +
  guides(color="none")

ggsave("./Plots/unajpt_bound.png", width = 9, height = 9)

# for lab meeting, remove bounding
glmresults %>% filter(bound=="Lower" & variable != "Jaundice" & caretakesubset==0) %>% 
ggplot(aes(x=term, y=pd100)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=pd100, ymin=pdcilow100, ymax=pdciup100), shape=15, size=0.8, color="black", show.legend=F, fatten=0.2, position=position_dodge2(width = 1.0) ) + 
  geom_point(shape=15, size=5, aes(color=variable, group=bound), position=position_dodge2(width = 1.0) , show.legend=F, alpha=0.9) + 
  #scale_color_brewer(palette = "Dark2")+
  coord_flip() + theme_bw() +
  #scale_x_continuous(trans = "reverse") + 
  labs(x="", y="Unadjusted prevalence difference per 100 kids") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank()) +
  guides(color="none")

ggsave("./Plots/unajpt_lowbd_all.png", width = 9, height = 7)

# for lab meeting, remove bounding
glmresults %>% filter(bound=="Lower" & variable != "Jaundice" & caretakesubset==1) %>% 
  ggplot(aes(x=term, y=pd100)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=pd100, ymin=pdcilow100, ymax=pdciup100), shape=15, size=0.8, color="black", show.legend=F, fatten=0.2, position=position_dodge2(width = 1.0) ) + 
  geom_point(shape=15, size=5, aes(color=variable, group=bound), position=position_dodge2(width = 1.0) , show.legend=F, alpha=0.9) + 
  #scale_color_brewer(palette = "Dark2")+
  coord_flip() + theme_bw() +
  #scale_x_continuous(trans = "reverse") + 
  labs(x="", y="Unadjusted prevalence difference per 100 kids") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank()) +
  guides(color="none")

ggsave("./Plots/unajpt_lowbd_care.png", width = 9, height = 5)


# adults-------------
survtable_all_ad("hiv03") # hiv
catvars <- c("catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net

adults2023int_hiv_nodrop <- adults2023int_hiv %>% filter(case5final != 9)
adults2023int_hiv_nodrop$hv105 <- as.numeric(adults2023int_hiv_nodrop$hv105)

# make survey design object
# original ad_5f
# exposed then unexposed: ad_5f_exp, ad_5f_unexp
options(survey.lonely.psu="adjust")

designf_ad <-svydesign(ids=ad_5f_exp$hv001, strata=ad_5f_exp$hv022 , weights=ad_5f_exp$hh_weight,  data=ad_5f_exp)

designf_ad_exp <-svydesign(ids=ad_5f_exp$hv001, strata=ad_5f_exp$hv022 , weights=ad_5f_exp$hh_weight,  data=ad_5f_exp)
designf_dhs2_ad_exp <-as_survey_design(designf_ad_exp)

designf_ad_un <-svydesign(ids=ad_5f_unexp$hv001, strata=ad_5f_unexp$hv022 , weights=ad_5f_unexp$hh_weight,  data=ad_5f_unexp)
designf_dhs2_ad_un <-as_survey_design(designf_ad_un)

table(ad_5f$case5final)

# sex 1=male, 2=female clipr::write_clip()
#ad_hbs <- svyglm(hbvresult ~ hv104, designf_dhs2_ad_exp, family=quasibinomial("log"))
ad_hbs <- svyglm(hbvresult ~ hv104, designf_dhs2_ad_exp, family=quasibinomial("identity"))
summary(ad_hbs)
confint(ad_hbs)

ad_hbs <- svyglm(hbvresult ~ hv104, designf_dhs2_ad_un, family=quasibinomial("log"))
ad_hbs <- svyglm(hbvresult ~ hv104, designf_dhs2_ad_un, family=quasibinomial("identity"))
summary(ad_hbs)
confint(ad_hbs)


# age - need to save as continuous
ad_age <- svyglm(case5final ~ hv105, designf_dhs2_ad, family=quasibinomial("log"))
ad_age <- svyglm(case5final ~ hv105, designf_dhs2_ad, family=quasibinomial("identity"))
summary(ad_age)
confint(ad_age)

# sex
ad_mf <- svyglm(case5final ~ hv104, designf_dhs2_ad, family=quasibinomial("log"))
ad_mf <- svyglm(case5final ~ as.factor(hv104), designf_dhs2_ad, family=quasibinomial("identity"))
summary(ad_mf)
confint(ad_mf)

# urban rural
ad_urb <- svyglm(hbvresult ~ as.factor(hv025), designf_dhs2_ad, family=quasibinomial("log"))
ad_urb <- svyglm(hbvresult ~ as.factor(hv025), designf_dhs2_ad, family=quasibinomial("identity"))
summary(ad_urb)
confint(ad_urb)

#prov
ad_urb <- svyglm(hbvresult ~ provgrp_kin, designf_ad_exp, family=quasibinomial("log"))
ad_urb <- svyglm(hbvresult ~ provgrp_kin, designf_ad_exp, family=quasibinomial("identity"))
summary(ad_urb)
confint(ad_urb)


# Next compare exposed and unexposed HBsAg+ adults, and exposed/unexposed HBsAg- adults
vars_ad <- c("hv105", 'sex','hv025','hv270','pfldh_adult', "reltoheadhh_simp", "provgrp") #'hv026', 'hiv03',

expad_bivs <- function(var){ # glm function
  m <- svyglm(as.formula(paste0('hbvresult ~', var)), designf_ad_exp, family=quasibinomial("identity"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}

unexpad_bivs <- function(var){ # glm function
  m <- svyglm(as.formula(paste0('hbvresult ~', var)), designf_ad_un, family=quasibinomial("identity"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}

ad_exp_glm <- map_dfr(vars_ad,expad_bivs) 
ad_unexp_glm <- map_dfr(vars_ad,unexpad_bivs) 

ad_exp_glm$exposure <- "Exposed"
ad_unexp_glm$exposure <- "Unexposed"

ad_glm <- rbind(ad_exp_glm, ad_unexp_glm)
view(ad_glm)

ad_glm$pd100 <- ad_glm$estimate*100
ad_glm$pdcilow100 <- ad_glm$`2.5 %`*100
ad_glm$pdciup100 <- ad_glm$`97.5 %`*100

table(ad_glm$term)
ad_glm$term[ad_glm$term == "hv0252"] <- "Rural vs urban"
ad_glm$term[ad_glm$term == "hv105"] <- "1-year increase in age"
ad_glm$term[ad_glm$term == "hv2702"] <- "Poorer vs poorest"
ad_glm$term[ad_glm$term == "hv2703"] <- "Middle vs poorest"
ad_glm$term[ad_glm$term == "hv2704"] <- "Richer vs poorest"
ad_glm$term[ad_glm$term == "hv2705"] <- "Richest vs poorest"
ad_glm$term[ad_glm$term == "pfldh_adult1"] <- "Pf+ vs Pf-"
ad_glm$term[ad_glm$term == "provgrp2"] <- "Equateur vs Kongo central/Bandundu"
ad_glm$term[ad_glm$term == "provgrp3"] <- "Kasais vs Kongo central/Bandundu"
ad_glm$term[ad_glm$term == "provgrp4"] <- "Katanga vs Kongo central/Bandundu"
ad_glm$term[ad_glm$term == "provgrp5"] <- "Orientale vs Kongo central/Bandundu"
ad_glm$term[ad_glm$term == "provgrp6"] <- "Kivus vs Kongo central/Bandundu"
ad_glm$term[ad_glm$term == "provgrp7"] <- "Maniema vs Kongo central/Bandundu"
ad_glm$term[ad_glm$term == "reltoheadhh_simpIn-law fam"] <- "In-laws of head vs head"
ad_glm$term[ad_glm$term == "reltoheadhh_simpOther"] <- "Other vs had of hh"
ad_glm$term[ad_glm$term == "reltoheadhh_simpParent/sib"] <- "Parent/sibling of head vs head"
ad_glm$term[ad_glm$term == "reltoheadhh_simpSon/daughter"] <- "Child of head vs head"
ad_glm$term[ad_glm$term == "reltoheadhh_simpSpouse"] <- "Spouse of head vs head"
ad_glm$term[ad_glm$term == "sexFemale"] <- "Female vs male"


ggplot(ad_glm, aes(x=term, y=pd100)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_pointrange(aes(x=term, y=pd100, ymin=pdcilow100, ymax=pdciup100), shape=15, size=0.8, color="black", show.legend=T, fatten=0.2, position=position_dodge2(width = 1.0) ) + 
  geom_point(shape=15, size=5, aes(color=exposure, group=exposure), position=position_dodge2(width = 1.0) , show.legend=T) + #alpha=0.9
  scale_color_manual(values =  c("#c23728","#22a7f0"))+
  #scale_color_brewer(palette = "Dark2")+
  coord_flip() + theme_bw() +
#  scale_alpha_discrete(range = c(0.35, 0.9))+
  #scale_x_continuous(trans = "reverse") + 
  labs(x="", y="Unadjusted prevalence difference per 100 adults") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank())
 + guides(color="none")
ggsave("./Plots/adults_forest.png", width = 9, height = 5)

