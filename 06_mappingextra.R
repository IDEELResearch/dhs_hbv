# 06_mapping extra
library(tidyverse)
library(sf)
# for clusters with missing GPS but province given, impute GPS
kid_hbv_kr_dis <- kid_hbv_kr_dis %>% 
  dplyr::mutate(prov2015=factor(
    kid_hbv_kr_dis$shnprovin, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
    labels = c("Kinshasa", "Kwango","Kwilu","Mai-Ndombe","Kongo Central","Equateur","Mongala","Nord-Ubangi","Sud-Ubangi","Tshuapa","Kasai","Kasai-Central","Kasai-Oriental","Lomami","Sankuru","Haut-Katanga","Haut-Lomami","Lualaba","Tanganyka","Maniema","Nord-Kivu","Bas-Uele","Haut-Uele","Ituri","Tshopo","Sud-Kivu")))
# should be using cluster number to impute, not cluster/hh!!!

#originally using summary list of households, but maybe this isn't complete
gpsmiss <- hhsum_all %>% filter(latnum=="0" | is.na(latnum)) %>% select(hv001, prov2015, hv026,latnum, longnum) %>% rename(prov_name = prov2015) %>% distinct(hv001, .keep_all=T)
view(gpsmiss)
# n=28 clusters with missing when using this - concerning about hhsum_all?
gpsmiss <- kid_hbv_kr_dis %>% filter(latnum=="0"| is.na(latnum)) %>% select(hv001, prov2015, hv026,latnum, longnum) %>% rename(prov_name = prov2015) %>% distinct(hv001, .keep_all=T)
nrow(gpsmiss)
# n=46 clusters with missing
# count missing by cluster
gpsmiss %>% group_by(prov_name) %>% summarise(n=n())

# check hh hbv prev by clusters with missing gps - how differential (and is hhsum_all the correct one to use)
hhsum_all %>% filter(latnum==0) %>% group_by(prov2015) %>% summarise(mean(hhprev_samp))

# view counts by province 
ctsamp <- gpsmiss %>% group_by(prov_name) %>% count()
view(ctsamp)
# hv026 Place of residence, 0  Capital, large city, 1  Small city, 2  Town, 3  Countryside
write.csv(gpsmiss, file = "/Users/camillem/Documents/GitHub/dhs_hbv/Data/gpsmiss_impute.csv")

gpsmiss <- left_join(gpsmiss, drcprov[, c("prov_name", "geometry")], by = "prov_name")

# set seed
set.seed(10272023)
sf_use_s2(F)
points <- st_sample(gpsmiss$geometry, size = c(1,1), type = "random") #, by_polygon = T 
p1_sf = st_sf(points)

p1_joined = st_join(p1_sf, drcprov)

gpsmiss_imp <- cbind(p1_joined, gpsmiss)
view(gpsmiss_imp)

gpsmiss_imp <- gpsmiss_imp %>%
  dplyr::mutate(lon_imp = sf::st_coordinates(geometry)[,1],
                lat_imp = sf::st_coordinates(geometry)[,2])

# map imputed points
ggplot() + 
  geom_sf(data=drcprov, aes(fill=as.factor(hasexphh)), color="tan4", size=0.5) + 
  geom_sf(data=points) 

gpsmiss_imp$hv001 <- as.numeric(gpsmiss_imp$hv001)
kidsmap_imp <- left_join(kid_hbv_kr_dis[, c("hv001","cluster_hh","kids_barcode","prov2015","hbvresultlowna", "hbvresultlowpos","shtetaindasno", "shtetaindasyes","geometry", "latnum", "longnum", "hv105", "hv104")], gpsmiss_imp[, c("hv001","lat_imp","lon_imp")], by = "hv001" )

kidsmap_imp$lat_rev <- ifelse(kidsmap_imp$latnum=="0" | is.na(kidsmap_imp$latnum), kidsmap_imp$lat_imp, kidsmap_imp$latnum)
kidsmap_imp$long_rev <- ifelse(kidsmap_imp$longnum=="0"| is.na(kidsmap_imp$latnum), kidsmap_imp$lon_imp, kidsmap_imp$longnum)
view(kidsmap_imp)
kidsmap_imp <- kidsmap_imp %>% select(-c(geometry.x,geometry.y))
# count how many combined lat missing
kidsmap_imp %>% filter(is.na(lat_rev)) %>% count()

kidsmap_impsf <- st_as_sf(kidsmap_imp[!is.na(kidsmap_imp$lat_rev) &!is.na(kidsmap_imp$long_rev),], coords = c("long_rev", "lat_rev"), crs = 4326) 
view(kidsmap_impsf)

# none - all missing gps addressed
test2 <- kidsmap_imp %>% filter(is.na(lat_rev)) 
view(test2)
# none - all missing gps addressed
test3 <- test2 %>% group_by(hv001,prov2015) %>% summarise(n=n())
view(test3)


# maps by kids of each age---------
output_pts0 <- st_join(output0, DRC, join = st_intersects) %>% filter(!is.na(Country))

age0 <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_pts0, aes(color=prev), alpha=0.8) + 
  labs(color='HBV prevalence (%) \nin children < 1') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Children age < 1")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.position = "none")

## kids below 1------------
output0 <- kidmapsf %>% 
  filter(hv105==0) %>% 
  group_by(hv001) %>%
  select(hv001,hv105,hbvresultlowna,hbvresultlowpos, shtetaindasno, shtetaindasyes, geometry) %>% 
  dplyr::summarize(n=n(),
                   npos = sum(hbvresultlowna==1),
                   prev = mean(hbvresultlowna, na.rm=T)*100,
                   prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)

output_pts0 <- st_join(output0, DRC, join = st_intersects) %>% filter(!is.na(Country))
  
age0 <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_pts0, aes(color=prev), alpha=0.8) + 
  labs(color='HBV prevalence (%) \nin children < 1') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Children age < 1")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.position = "none")
age0
## kids aged 1------------------------------------------------
nrow(output0)
output1 <- kidmapsf %>% 
  filter(hv105==1) %>% 
  group_by(hv001) %>%
  select(hv001,hv105,hbvresultlowna,hbvresultlowpos, shtetaindasno, shtetaindasyes, geometry) %>% 
  dplyr::summarize(n=n(),
                   npos = sum(hbvresultlowna==1),
                   prev = mean(hbvresultlowna, na.rm=T)*100,
                   prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)

output_pts1 <- st_join(output1, DRC, join = st_intersects) %>% filter(!is.na(Country))

age1 <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output1, aes(color=prev), alpha=0.8) + 
  labs(color='HBV prevalence (%) \nin children = 1') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Children age 1")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.position = "none")

## children aged 2-----------
output2 <- kidmapsf %>% 
  filter(hv105==2) %>% 
  group_by(hv001) %>%
  select(hv001,hv105,hbvresultlowna,hbvresultlowpos, shtetaindasno, shtetaindasyes, geometry) %>% 
  dplyr::summarize(n=n(),
                   npos = sum(hbvresultlowna==1),
                   prev = mean(hbvresultlowna, na.rm=T)*100,
                   prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)

output_pts2 <- st_join(output2, DRC, join = st_intersects) %>% filter(!is.na(Country))

age2 <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output2, aes(color=prev), alpha=0.8) + 
  labs(color='HBV prevalence (%) \nin children = 2') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Children age 2")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.position = "none")
age2
## children aged 3-----------
output3 <- kidmapsf %>% 
  filter(hv105==3) %>% 
  group_by(hv001) %>%
  select(hv001,hv105,hbvresultlowna,hbvresultlowpos, shtetaindasno, shtetaindasyes, geometry) %>% 
  dplyr::summarize(n=n(),
                   npos = sum(hbvresultlowna==1),
                   prev = mean(hbvresultlowna, na.rm=T)*100,
                   prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)

output_pts3 <- st_join(output3, DRC, join = st_intersects) %>% filter(!is.na(Country))

age3 <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output3, aes(color=prev), alpha=0.8) + 
  labs(color='HBV prevalence (%) \nin children = 3') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Children age 3")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.position = "none")
age3
## children aged 4-----------
output4 <- kidmapsf %>% 
  filter(hv105==4) %>% 
  group_by(hv001) %>%
  select(hv001,hv105,hbvresultlowna,hbvresultlowpos, shtetaindasno, shtetaindasyes, geometry) %>% 
  dplyr::summarize(n=n(),
                   npos = sum(hbvresultlowna==1),
                   prev = mean(hbvresultlowna, na.rm=T)*100,
                   prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)

output_pts4 <- st_join(output4, DRC, join = st_intersects) %>% filter(!is.na(Country))

age4 <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output4, aes(color=prev), alpha=0.8) + 
  labs(color='HBV prevalence (%) \nin children = 4') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Children age 4")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.position = "none")
age4
# children aged 5------
output5 <- kidmapsf %>% 
  filter(hv105==5) %>% 
  group_by(hv001) %>%
  select(hv001,hv105,hbvresultlowna,hbvresultlowpos, shtetaindasno, shtetaindasyes, geometry) %>% 
  dplyr::summarize(n=n(),
                   npos = sum(hbvresultlowna==1),
                   prev = mean(hbvresultlowna, na.rm=T)*100,
                   prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)

output_pts5 <- st_join(output5, DRC, join = st_intersects) %>% filter(!is.na(Country))

age5 <- ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output5, aes(color=prev), alpha=0.8) + 
  labs(color='HBV prevalence (%) \nin children = 5') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral', limits = c(0, 100)) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Children age 5")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))
age5

age0 + age1 + age2 + age3 + age4 + age5 + plot_layout(nrow=1, ncol = 6) #+ plot_annotation(tag_levels = 'A')
ggsave('./Plots/byage.png', width=15, height=6)

# Consider using facet_wrap or grid()---------------------------
ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output, aes(color=prev), alpha=0.8) + 
  labs(color='HBV prevalence (%) \nin children = 5') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral', limits = c(0, 100)) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Children age 5")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))+
  facet_grid(~hv105)


# weighted prov prev by age group-----

kidsmap_impsf <- kidsmap_impsf %>% 
  dplyr::mutate(sex=factor(
    kidsmap_impsf$hv104, 
    levels = c(1, 2),
    labels = c("Male", "Female")))

wtdage <- kidsmap_impsf %>% select(hv001,cluster_hh, prov2015, hv105, sex, hbvresultlowna, hbvresultlowpos, geometry, shtetaindasno, shtetaindasyes) 
output <- wtdage %>% group_by(hv105, hv001) %>% dplyr::summarize(n=n(),
                                                                   npos = sum(hbvresultlowna==1),
                                                                   prev = mean(hbvresultlowna, na.rm=T)*100,
                                                                   prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                                                                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                                                                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)
view(output)
output_prov <- wtdage %>% group_by(hv105, prov2015) %>% dplyr::summarize(n=n(),
                                                                      npos = sum(hbvresultlowna==1),
                                                                      prev = mean(hbvresultlowna, na.rm=T)*100,
                                                                      prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                                                                      tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                                                                      tetcovupper = mean(shtetaindasyes, na.rm=T)*100) %>% 
                    rename(prov_name=prov2015)

output_prov <- wtdage %>% group_by(sex,hv105, prov2015) %>% 
                dplyr::summarize(n=n(),
                npos = sum(hbvresultlowna==1),
                prev = mean(hbvresultlowna, na.rm=T)*100,
                prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                tetcovupper = mean(shtetaindasyes, na.rm=T)*100) %>% 
            rename(prov_name=prov2015)
class(drcprov)
sf_use_s2(F)
test = st_join( drcprov[, c("prov_name", "geometry")], output_prov, by = "prov_name")
view(test)

ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=test, mapping=aes(fill=prev)) + 
  labs(color='HBV prevalence (%) \nin children = 5') + 
  theme_bw(base_size=14) + 
  scale_fill_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("HBsAg prevalence by province by age")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))+
  facet_grid(sex ~ hv105)
  facet_wrap(sex ~ hv105, ncol = 6, nrow=2)
ggsave('./Plots/provagesex.png', width=12, height=6)
  
# weight age/sex prov counts
kid_hbv_kr_dis <- kid_hbv_kr_dis %>% 
  dplyr::mutate(sex=factor(
    kid_hbv_kr_dis$hv104, 
    levels = c(1, 2),
    labels = c("Male", "Female")))
#wtd prov overall by sex
output_prov_sex <- wtdage %>% group_by(sex,hv105, prov2015) %>% 
  dplyr::summarize(n=n(),
                   npos = sum(hbvresultlowna==1),
                   prev = mean(hbvresultlowna, na.rm=T)*100,
                   prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100) %>% 
  rename(prov_name=prov2015)

output_prov_sex <- wtdage %>% group_by(sex, prov2015) %>% dplyr::summarize(n=n(),
                                                                 npos = sum(hbvresultlowna==1),
                                                                 prev = mean(hbvresultlowna, na.rm=T)*100,
                                                                 prev_u = mean(hbvresultlowpos, na.rm=T)*100,
                                                                 tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                                                                 tetcovupper = mean(shtetaindasyes, na.rm=T)*100)
view(output_prov_sex)

output_prov_sex <- output_prov_sex %>% mutate(prev_bin = case_when(
  prev == 0 ~ 0, #"0",
  prev > 0 & prev <5 ~ 1, #"<5%",
  prev >= 5 & prev <10 ~ 2, #"5-10%",
  prev >= 10 & prev <20 ~ 3, #"10-20%",
  prev >= 20  ~ 4, #">20%"
) %>%  as.factor())

output_prov_sex <- output_prov_sex %>% rename(prov_name = prov2015)
sf_use_s2(F)

# output_prov_sex_lg <- left_join(drcprov[, c("prov_name", "geometry")],output_prov_sex, by="prov_name")
output_prov_sex_lg = st_join(drcprov, output_prov_sex)
view(output_prov_sex_lg)

ggplot()+
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=output_prov_sex_lg,  mapping=aes(fill=prev_bin))+
  #scale_fill_brewer(palette = "Greens", labels) +
  #scale_fill_distiller(palette = 'Spectral') +
  scale_fill_manual(values = c("#e5f5f9","#99d8c9","#41ae76", "#238b45","#00441b") , labels=c("0","<5%","5-10%","10-20%", ">20%")) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Weighted HBV prevalence in kids ≤5")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))+
  facet_wrap(~sex,nrow=2)
# or facet_grid() ?
ggsave('./Plots/provsex_allwtd.png', width=12, height=6)



kid_hbv_kr_dis$agesex <- paste0(kid_hbv_kr_dis$sex,", ", kid_hbv_kr_dis$hv105)
table(kid_hbv_kr_dis$agesex)
kid_hbv_kr_dis$prov2015
# make survey design object
designf <-svydesign(ids=kid_hbv_kr_dis$hv001, strata=kid_hbv_kr_dis$hv022 , weights=kid_hbv_kr_dis$hh_weight,  data=kid_hbv_kr_dis)
options(survey.lonely.psu="adjust")
designf_dhs2 <-as_survey_design(designf)

agesexprov <- svytable(~ prov2015 + agesex + hbvresultlowna, designf_dhs2) %>% clipr::write_clip()

save <- svytable(~ prov2015 + agesex, designf_dhs2) %>% clipr::write_clip()
save
agesexprov <- as.data.frame(svytable(~ prov2015 + agesex + hbvresultlowna, designf_dhs2) %>% clipr::write_clip())
view(agesexprov)

wtdagesexprov <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/Results discussions/prov counts.xlsx",
                         sheet = "wtdagesexprov")
view(wtdagesexprov)
wtdagesexprov <- wtdagesexprov %>% mutate(
  pd5 = `Male, 5` - `Female, 5`,
  pd4 = `Male, 4` - `Female, 4`,
  pd3 = `Male, 3` - `Female, 3`,
  pd2 = `Male, 2` - `Female, 2`,
  pd1 = `Male, 1` - `Female, 1`,
  pd0 = `Male, 0` - `Female, 0`)

wtdagesexprov_l <- wtdagesexprov %>% gather("prov2015")
view(wtdagesexprov_l)

library(data.table)
wtdagesexprov_l <- melt(wtdagesexprov)
view(wtdagesexprov_l)
wtdagesexprov_l <- wtdagesexprov_l %>% rename(prov_name = prov2015)
wtdagesexprov_l$prov2015nohy <- gsub("-"," ", wtdagesexprov_l$prov2015)


wtdagesexprov_l$ADM1_NAME <- toupper(wtdagesexprov_l$prov2015)
view(wtdagesexprov_l)


wtdagesexprov_lg <- left_join(drcprov[, c("prov_name", "geometry")],wtdagesexprov_l, by="prov_name")
wtdagesexprov_lg <- left_join(wtdagesexprov_l, drcprov[, c("prov_name", "geometry")], by="prov_name")
view(wtdagesexprov_lg)
hist(wtdagesexprov_lg$value)

wtdagesexprov_lg <- wtdagesexprov_lg %>% mutate(prev_bin = case_when(
  value == 0 ~ 0, #"0",
  value > 0 & value <5 ~ 1, #"<5%",
  value >= 5 & value <10 ~ 2, #"5-10%",
  value >= 10 & value <20 ~ 3, #"10-20%",
  value >= 20  ~ 4, #">20%",
) %>%  as.factor())
table(wtdagesexprov_lg$prev_bin, useNA = "always")
class(wtdagesexprov_lg$prev_bin)

# Continuous weighted prevalence
ggplot()+
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=wtdagesexprov_lg[wtdagesexprov_lg$variable != "Total",],  mapping=aes(fill=value))+
  scale_fill_distiller(palette = 'Greens', trans = "reverse") +
  #scale_fill_distiller(palette = 'Spectral') +
  #scale_fill_manual(values = c("#3288BD","#FFFFBF","#FDAE61", "#9E0142") , labels=c("0","≤2%","2-7%",">7%")) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Weighted HBV prevalence in kids ≤5")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))#+
  facet_wrap(~variable,  ncol = 6, nrow=2)
view(wtdagesexprov_lg)
# Group wtd prevalance
ggplot()+
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  #geom_sf(data=wtdagesexprov_lg[wtdagesexprov_lg$variable != "Total",],  mapping=aes(fill=prev_bin))+ # now that we added prev diffs to this dataframe
  geom_sf(data=wtdagesexprov_lg[grepl("ale", wtdagesexprov_lg$variable),],  mapping=aes(fill=prev_bin))+
  #scale_fill_brewer(palette = "Greens", labels) +
  #scale_fill_distiller(palette = 'Spectral') +
  scale_fill_manual(values = c("#e5f5f9","#99d8c9","#41ae76", "#238b45","#00441b") , labels=c("0","<5%","5-10%","10-20%", ">20%")) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Weighted HBV prevalence in kids ≤5")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))+
  facet_wrap(~variable,  ncol = 6, nrow=2)
ggsave('./Plots/provagesex_wtd.png', width=12, height=6)

# Prev diffs age sex mapped------
pds <- wtdagesexprov_lg %>% filter(grepl('pd', variable))
view(pds)

ggplot()+
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=pds, mapping=aes(fill=value))+
  #scale_fill_brewer(palette = "Greens", labels) +
  scale_fill_distiller(palette = 'Spectral') +
  #scale_fill_manual(values = c("#e5f5f9","#99d8c9","#41ae76", "#238b45","#00441b") , labels=c("0","<5%","5-10%","10-20%", ">20%")) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Weighted HBV prevalence in kids ≤5")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))+
  facet_wrap(~variable,  ncol = 6, nrow=1)

# PD overall
# by sex overall
sexprov <- as.data.frame(svytable(~ prov2015 + sex + hbvresultlowna, designf_dhs2) %>% clipr::write_clip())
class(sexprov)

sexprov_w <- reshape(sexprov, idvar = c("prov2015", "sex"), timevar = "hbvresultlowna", direction = "wide")


#Kids with missing outcome----------------



