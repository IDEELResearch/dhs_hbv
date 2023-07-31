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

catvars <- c("catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net
survtable_all_ad("hbvresult") # 

# sex
kid_mf <- svyglm(hbvresult ~ as.factor(hv104), designf_dhs2, family=quasibinomial("log"))
kid_mf <- svyglm(hbvresult ~ as.factor(hv104), designf_dhs2, family=quasibinomial("identity"))
summary(kid_mf)
confint(kid_mf)

# 
kid_urb <- svyglm(hbvresult ~ as.factor(hv025), designf_dhs2, family=quasibinomial("log"))
kid_urb <- svyglm(hbvresult ~ as.factor(hv025), designf_dhs2, family=quasibinomial("identity"))
summary(kid_urb)
confint(kid_urb)

# code var for other kid pos in hh
kid_clust <- svyglm(hbvresult ~ as.factor(totalkidpos_f), designf_dhs2, family=quasibinomial("log"))
kid_clust <- svyglm(hbvresult ~ as.factor(totalkidpos_f), designf_dhs2, family=quasibinomial("identity"))
summary(kid_mf)
confint(kid_mf)

# adjusted?
cat5_adj <- svyglm(pfldh_adult ~ as.factor(cattleherd5)+ modernhousing + sex + 
                     treatedbednet + urbanrural + hv270, designf_dhs2, family=quasibinomial("log")) # identity NOT CONVERGING



# adults
survtable_all_ad("hiv03") # hiv
catvars <- c("catresult","totalkidpos_f","hv104", "hv024", "hv025","hv270", "hv228", "pfldh_kids") # sex, province, urbal/rural, wealth, children <5 slept under net


