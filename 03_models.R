# 03_models
options(scipen = 999)
options(scipen = 0)
options(scipen = 100, digits = 3)

# code to separate var names from values
m <- glm(total ~  SE + source, data = df3)

(trm <- attr(m$terms, "term.labels")) # Getting original variables

(asgn <- attr(model.matrix(m$formula, data = df3), "assign")) # See ?model.matrix
summary(m)

cbind(Term = trm[asgn[-1]], 
      Category = str_replace(names(coef(m)[-1]), trm[asgn[-1]], ""))
#      Term    Category

# kids---------------------
# elig_kids_whbvres_wt_kr
library(survey)
library(srvyr)
library(tidyverse)
library(broom)

# Outcome vars: missingness and sens analyses------------------
# hbvresult5, hbvresult1, hbvresult2, hbvresult100
elig_kids_whbvres_wt_kr_nomiss <- elig_kids_whbvres_wt_kr_nomiss %>% mutate_at(c("beat","injec"), as.factor)
elig_kids_whbvres_wt_kr_nomiss <- elig_kids_whbvres_wt_kr %>% filter(anemia_f != "Not available")

elig_kids_whbvres_wt_kr_nomiss$sex <- relevel(elig_kids_whbvres_wt_kr_nomiss$sex, ref = "Female")

table(elig_kids_whbvres_wt_kr$beat_f)
table(elig_kids_whbvres_wt_kr$anemia_f, elig_kids_whbvres_wt_kr$hbvresult5)
# discuss with Jess about what to do with the 2908 - impute, leave out, etc.
# for now proceed with bounding the 46 with low vol

# go through 01_datacleaning_rev.R) to obtain elig_kids_whbvres_wt_kr
# make survey design object
# weight variable decisions: hh_weight (frmo hv005), hv028_div, both_wt_new (hv005 + propens score), both_wt_old (hv028 + propens score)
designf <-svydesign(ids=elig_kids_whbvres_wt_kr_nomiss$hv001, strata=elig_kids_whbvres_wt_kr_nomiss$hv022 , weights=elig_kids_whbvres_wt_kr_nomiss$both_wt_new,  data=elig_kids_whbvres_wt_kr_nomiss)
options(survey.lonely.psu="adjust")
designf_dhs2 <-as_survey_design(designf)
# vars to run - c("hbvresult","catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net

# age
kid_age <- svyglm(hbvresult5 ~ as.factor(hv105), designf_dhs2, family=quasibinomial("log"))
kid_age <- svyglm(hbvresultlowpos ~ as.factor(hv105), designf_dhs2, family=quasibinomial("identity"))
summary(kid_age)
confint(kid_age)

# sex
kid_mf <- svyglm(hbvresult5 ~ as.factor(hv104), designf_dhs2, family=quasibinomial("log"))
kid_mf <- svyglm(hbvresult5 ~ as.factor(hv104), designf_dhs2, family=quasibinomial("identity"))
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

# pf malaria
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
kid_prov <- svyglm(hbvresult5 ~ as.factor(prov2015), designf_dhs2, family=quasibinomial("log"))
kid_prov <- svyglm(hbvresult5 ~ as.factor(provgrp_kin_l), designf_dhs2, family=quasibinomial("identity")) # not converging
summary(kid_prov)
confint(kid_prov)
exp(kid_prov$coefficients)

# code var for other kid pos in hh
kid_clust <- svyglm(hbvresult ~ as.factor(totalkidpos_f), designf_dhs2, family=quasibinomial("log"))
kid_clust <- svyglm(hbvresult ~ as.factor(totalkidpos_f), designf_dhs2, family=quasibinomial("identity"))
summary(kid_clust)
confint(kid_clust)

inj <- svyglm(hbvresultlowna ~ totalkidpos_f, designf_dhs2, family=quasibinomial("identity"))
summary(inj)
confint(inj)

beat <- svyglm(hbvresult5 ~ beat_f, designf_dhs2, family=quasibinomial("identity"))
summary(beat)
confint(beat)

# as function----
vars <- c("hv105_fromhc1_f", 'sex','reltoheadhh_simp', 'urbanrural','location','wealth','anemia_f',"modstunt", 'pfmalaria','tetab', "dpt_doses_f", "injec_f", "beat_f")
# run separately: "injec_f", 'v480', provgrp_kin_l, ,

bivs_5 <- function(var){ # glm function
  m <- svyglm(as.formula(paste0('hbvresult5 ~', var)), designf_dhs2, family=quasibinomial("identity"))
  cbind(tidy(m, exponentiate = FALSE), confint(m, method = c("Wald"))) %>% filter(stringr::str_detect(term, var))}

glmresults_5 <- map_dfr(vars,bivs_5) 
glmresults_5 %>% print(noSpaces=T) 

glmresults_5 <- glmresults_5 %>% mutate(
  pd100 = estimate*100,
  pdcilow100 = `2.5 %`*100,
  pdciup100 = `97.5 %`*100)
  
glmresults_5 <- glmresults_5 %>%  
  mutate(variable = case_when( # can ignore case with grepl("re", term, ignore.case = TRUE) ~"Group2"))
    grepl("wealth", term) ~ "Wealth",
    grepl("provgrp_kin", term) ~"Region",
    grepl("location", term) ~"Location",
    grepl("hv105_fromhc1_f", term) ~"Age",
    grepl("reltoheadhh_simp", term) ~"Relation to head of house",
    grepl("dpt_doses_f", term) ~"DPT vaccine",
    grepl("beat", term) ~"Beating justified",
    grepl("urbanrural", term) ~"Rurality",
    grepl("sex", term) ~"Sex",
    grepl("anemia_f", term) ~"Nutrition",
    grepl("injec", term) ~"Injections",
    grepl("pfmalaria", term) ~"Pf",
    grepl("tetab", term) ~"TetAb",
    grepl("mod", term) ~"Nutrition", # stunting and wasting together
  ),
  caretakesubset = case_when( # variables only asked to caretakers/mothers of children sampled
    variable %in% c("Anthropometry","DPT vaccine", "Injections","Beating justified") ~ 1,
    TRUE ~ 0
  )) 
table(glmresults_5$caretakesubset)

rownames(glmresults_5) <- 1:nrow(glmresults_5)
glmresults_5 <- glmresults_5 %>% arrange(variable, term)
glmresults_5 <- glmresults_5  %>%  mutate(desorder=row_number())

table(glmresults_5$term)
view(glmresults_5)
#glmresults_5 <- glmresults_5 %>% mutate(setorder = )

glmresults_5$term[glmresults_5$term == "hv105_fromhc1_f1"] <- "1 year vs <1 year"
glmresults_5$term[glmresults_5$term == "hv105_fromhc1_f2"] <- "2 years vs <1 year"
glmresults_5$term[glmresults_5$term == "hv105_fromhc1_f3"] <- "3 years vs <1 year"
glmresults_5$term[glmresults_5$term == "hv105_fromhc1_f4"] <- "4 years vs <1 year"
glmresults_5$term[glmresults_5$term == "sexMale"] <- "Male vs Female"
#glmresults_5$term[glmresults_5$term == "hv1042"] <- "Female vs male"
glmresults_5$term[glmresults_5$term == "reltoheadhh_simpGrandchild"] <- "Grandchild vs child"
glmresults_5$term[glmresults_5$term == "reltoheadhh_simpOther"] <- "Other vs child"

glmresults_5$term[glmresults_5$term == "tetabReactive"] <- "Tetanus Ab+ vs -"
glmresults_5$term[glmresults_5$term == "dpt_doses_fNot available"] <- "Not available"
glmresults_5$term[glmresults_5$term == "dpt_doses_fSeries incomplete"] <- "DPT initiated (1-2 doses) vs not started (0 doses)"
glmresults_5$term[glmresults_5$term == "dpt_doses_fSeries completed"] <- "DPT series complete (3 doses) vs not started (0 doses)"
glmresults_5$term[glmresults_5$term == "injec1"] <- "1-12 injections vs none"
glmresults_5$term[glmresults_5$term == "injec2"] <- "12-24 injections vs none"
glmresults_5$term[glmresults_5$term == "injec3"] <- "≥25 injections vs none"
glmresults_5$term[glmresults_5$term == "anemia_fModerate-to-severe"] <- "Anemia: moderate-to-severe vs mild-to-none"
glmresults_5$term[glmresults_5$term == "pfmalariaPf-positive"] <- "Pf malaria+ vs -"
#glmresults_5$term[glmresults_5$term == "dpt_count"] <- "# of DPT doses (continuous)"
glmresults_5$term[glmresults_5$term == "modstunt1"] <- "Mod-to-severe stunting vs none"
glmresults_5$term[glmresults_5$term == "modstunt9"] <- "Missing stunting vs none"
glmresults_5$term[glmresults_5$term == "wast_mod1"] <- "Mod-to-severe wasting vs none"
glmresults_5$term[glmresults_5$term == "wast_mod9"] <- "Missing wasting vs none"

glmresults_5$term[glmresults_5$term == "urbanruralRural"] <- "Rural vs urban"
glmresults_5$term[glmresults_5$term == "locationSmall city"] <- "Small city vs capital"
glmresults_5$term[glmresults_5$term == "locationTown"] <- "Town vs capital"
glmresults_5$term[glmresults_5$term == "locationCountryside"] <- "Countryside vs capital"
glmresults_5$term[glmresults_5$term == "wealthPoorer"] <- "Poorer vs poorest"
glmresults_5$term[glmresults_5$term == "wealthMiddle"] <- "Middle vs poorest"
glmresults_5$term[glmresults_5$term == "wealthRicher"] <- "Richer vs poorest"
glmresults_5$term[glmresults_5$term == "wealthRichest"] <- "Richest vs poorest"
glmresults_5$term[glmresults_5$term == "beat1"] <- "Beating justified vs not"

glmresults_5$term[glmresults_5$term == "provgrp_kin2"] <- "Kongo Central/Bandundu vs Kinshasa"
glmresults_5$term[glmresults_5$term == "provgrp_kin3"] <- "Equateur vs Kinshasa"
glmresults_5$term[glmresults_5$term == "provgrp_kin4"] <- "Kasais vs Kinshasa"
glmresults_5$term[glmresults_5$term == "provgrp_kin5"] <- "Katanga vs Kinshasa"
glmresults_5$term[glmresults_5$term == "provgrp_kin6"] <- "Orientale vs Kinshasa"
glmresults_5$term[glmresults_5$term == "provgrp_kin7"] <- "Kivus vs Kinshasa"
glmresults_5$term[glmresults_5$term == "provgrp_kin8"] <- "Maniema vs Kinshasa"
table(glmresults_5$term)
view(glmresults_5)

glmresults_5 %>% filter(term != "tetabIndeterminate" & term != "Not available") %>% 
ggplot(aes(x=term, y=pd100)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_pointrange(aes(x=fct_rev(fct_reorder(term, desorder)), y=pd100, ymin=pdcilow100, ymax=pdciup100), shape=15, size=0.8, color="black", show.legend=F, fatten=0.2, position=position_dodge2(width = 1.0) ) + 
  geom_point(shape=15, size=5, aes(color=variable), position=position_dodge2(width = 1.0) , show.legend=T) + #alpha=0.9 aes(color=variable)
  scale_color_manual(values = c("#A6CEE3", "#33A02C","#E31A1C", "#FDBF6F","#6A3D9A", '#be214d', "#B2DF8A", "#1F78B4","#FB9A99", "#FF7F00", "#CAB2D6", '#93003a'))+
  #scale_color_manual(values = c('#00429d', '#ffd3bf','#3761ab','#ffa59e', '#5681b9','#f4777f', '#73a2c6', '#dd4c65','#93c4d2','#be214d', '#b9e5dd', '#93003a'))+
  #scale_color_manual(values = c('#00429d','#93003a', '#3761ab','#be214d', '#5681b9','#dd4c65', '#73a2c6', '#f4777f','#93c4d2','#ffa59e', '#b9e5dd','#ffd3bf' ))+
  # from https://www.vis4.net/palettes/#/13|d|00429d,96ffea,ffffe0|ffffe0,ff005e,93003a|1|1
  #scale_color_brewer(palette = "Dark2")+
  coord_flip() + theme_bw() +
  #scale_alpha_discrete(range = c(0.35, 0.9))+
  #scale_x_continuous(trans = "reverse") + 
  labs(x="", y="Prevalence difference per 100 children") + 
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 11),
        axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank()) +
  guides(color="none")

ggsave("./Plots/unadjpd_feb.png", width = 9, height = 9)

colnames(glmresults_5)
glmresults_5e <- glmresults_5 %>% mutate(pdsci = paste(round(pd100,1),' (',round(pdcilow100,1),', ', round(pdciup100,1),')', sep = "")) %>% 
  select(c("term","pdsci"))
view(glmresults_5e)
write.csv(glmresults_5e, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/glmresults5e.csv")

# stopped feb 18------

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

write.csv(ad_glm, file = "~/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/ad_glm.csv")

library(patchwork)
ggplot(ad_glm, aes(x=term, y=pd100)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_pointrange(aes(x=term, y=pd100, ymin=pdcilow100, ymax=pdciup100), shape=15, size=0.8, color="black", show.legend=T, fatten=0.2, position=position_dodge2(width = 0.5) ) + 
  geom_point(shape=15, size=5, aes(color=exposure, group=exposure), position=position_dodge2(width = 0.5) , show.legend=T) + #alpha=0.9
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
ggsave("./Plots/adults_forest.png", width = 9, height = 9)

