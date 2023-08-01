# 03_models

# kids---------------------
# add weight variable
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

# vars to run - c("hbvresult","catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net
table(kid_dhs_int$hbvresult, kid_dhs_int$catresult)

table(kid_dhs_int$hv105)
# age
kid_age <- svyglm(hbvresult ~ as.factor(hv105), designf_dhs2, family=quasibinomial("log"))
kid_age <- svyglm(hbvresult ~ as.factor(hv105), designf_dhs2, family=quasibinomial("identity"))
summary(kid_age)
confint(kid_age)

# sex
kid_mf <- svyglm(hbvresult ~ as.factor(hv104), designf_dhs2, family=quasibinomial("log"))
kid_mf <- svyglm(hbvresult ~ as.factor(hv104), designf_dhs2, family=quasibinomial("identity"))
summary(kid_mf)
confint(kid_mf)

# urban rural
kid_urb <- svyglm(hbvresult ~ as.factor(hv025), designf_dhs2, family=quasibinomial("log"))
kid_urb <- svyglm(hbvresult ~ as.factor(hv025), designf_dhs2, family=quasibinomial("identity"))
summary(kid_urb)
confint(kid_urb)

# large/small city/town/countryside
kid_loc <- svyglm(hbvresult ~ as.factor(hv026), designf_dhs2, family=quasibinomial("log"))
kid_loc <- svyglm(hbvresult ~ relevel(as.factor(hv026), ref="2"), designf_dhs2, family=quasibinomial("identity"))
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


# adults
survtable_all_ad("hiv03") # hiv
catvars <- c("catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net


