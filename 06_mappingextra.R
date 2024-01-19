# 06_mapping extra
library(tidyverse)
library(sf)
library(colorspace)

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
view(gpsmiss)

#Impute GPS in prov bounds---------------- 
# set seed
set.seed(10272023)
sf_use_s2(F)
points <- st_sample(gpsmiss$geometry, size = c(1,1), type = "random") #, by_polygon = T ; type=random
p1_sf = st_sf(points)

p1_joined = st_join(p1_sf, drcprov)

gpsmiss_imp <- cbind(p1_joined, gpsmiss)
view(gpsmiss_imp)

gpsmiss_imp <- gpsmiss_imp %>%
  dplyr::mutate(lon_imp = sf::st_coordinates(geometry)[,1],
                lat_imp = sf::st_coordinates(geometry)[,2],
                imputed = 1)

# map imputed points
ggplot() + 
  geom_sf(data=drcprov, aes(fill=as.factor(hasexphh)), color="tan4", size=0.5) + 
  geom_sf(data=points) 

gpsmiss_imp$hv001 <- as.numeric(gpsmiss_imp$hv001)
kidsmap_imp <- left_join(kid_hbv_kr_dis[, c("hv001","cluster_hh","kids_barcode","prov2015","hbvresult5", "hbvresult1","hbvresult2","hbvresult100","shteta","shtetaindasno", "shtetaindasyes","geometry", "latnum", "longnum", "hv105", "hv104")], gpsmiss_imp[, c("hv001","lat_imp","lon_imp","imputed")], by = "hv001" )

kidsmap_imp$lat_rev <- ifelse(kidsmap_imp$latnum=="0" | is.na(kidsmap_imp$latnum), kidsmap_imp$lat_imp, kidsmap_imp$latnum)
kidsmap_imp$long_rev <- ifelse(kidsmap_imp$longnum=="0"| is.na(kidsmap_imp$latnum), kidsmap_imp$lon_imp, kidsmap_imp$longnum)
kidsmap_imp$imputed <- ifelse(is.na(kidsmap_imp$imputed), 0,kidsmap_imp$imputed)

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

# Main maps-------------------------------
library(sf)
library(gstat)
library(stars)
library(tidyverse)
library(patchwork)
library(sp)

dhsmeta <- readRDS("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/DHS_pr_full_merge_backup.rds")

dhsmeta_kids <- dhsmeta %>% filter(kids==1)

# redo sf geometry
kidmapsf = st_as_sf(kid_hbv_kr_dis[!is.na(kid_hbv_kr_dis$latnum) &!is.na(kid_hbv_kr_dis$longnum),], coords = c("longnum", "latnum"), crs = 4326) 
# evaluate the 6997-6973 that didn't map
head(kidmapsf$geometry)
nogpskid <- kid_dhs_int %>% filter(!(cluster_hh %in% kidmapsf$cluster_hh)) %>% select(c("cluster_hh","latnum", "longnum","shnprovin","adm1fips","adm1name","catresult" ))

# from 06_mappingextra.R, use kidsmap_impsf, which has imputed GPS
sf_use_s2(F)

# subset cluster level df to get weights
output_df <- kid_hbv_kr_dis %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   #npos5 = n(hbvresult5),
                   prev = mean(hbvresult5, na.rm=T)*100,
                   prev1 = mean(hbvresult1, na.rm=T)*100,
                   prev2 = mean(hbvresult2, na.rm=T)*100,
                   prev100 = mean(hbvresult100, na.rm=T)*100)

output_df <- merge(output_df, kid_hbv_kr_dis[,c("hh_weight","hv001")],by="hv001", all.x = TRUE)

# GADM boundaries from: https://gadm.org/download_country_v3.html
admin0 <- readRDS('/Users/camillem/Documents/GitHub/animalaria/admin0.rds') %>%          # GADM admin0 boundaries
  st_transform(4326) %>% # set at ESPG 4326
  filter(grepl('Congo|Rwanda|Tanzania|Burundi|African Republic|Angola|Zambia|Uganda|Sudan|Gabon|Cameroon|Equatorial Guinea', Country)) 

st_crs(admin0) # view CRS

DRC <- admin0 %>% filter(Country=='Democratic Republic of the Congo') # DRC

summary(kidmapsf$hh_weight)

table(kidmapsf$shteta)

table(kidsmap_impsf$shteta)
output <- kidsmap_impsf %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   imputed = mean(imputed),
                   prev = mean(hbvresult5, na.rm=T)*100,
                   prev1 = mean(hbvresult1, na.rm=T)*100,
                   prev2 = mean(hbvresult2, na.rm=T)*100,
                   prev100 = mean(hbvresult100, na.rm=T)*100,
                   tetcovlower = mean(shtetaindasno, na.rm=T)*100,
                   tetcovupper = mean(shtetaindasyes, na.rm=T)*100)
table(output$imputed)
output$tetdiff <- output$tetcovupper - output$tetcovlower


# remove points where geometry is outside of DRC outline (geometry=c(0,0))
output_points <- st_join(output, DRC, join = st_intersects) %>% filter(!is.na(Country))

# simple map of dhs cluster locations with kid samples
drcprov = st_read("/Users/camillem/Documents/GitHub/hbv_hover/adm1/GLOBAL_ADM1.shp", stringsAsFactors = FALSE) %>% filter(ADM0_NAME=="DEMOCRATIC REPUBLIC OF THE CONGO") %>%   st_transform(4326)

## DHS clust------------
Z <-
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, alpha=0.8, aes(color=as.factor(imputed))) + 
  #labs(color='') + 
  theme_bw(base_size=14) + 
  scale_color_manual(values = c("black","mediumpurple2"), labels = c("Original", "Imputed") ) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  #ggtitle("Children ≤ 5")+ # for adding with adult figure
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
       # legend.position = c(.25, .1), # to include inside map inset
        legend.background = element_blank(),
        legend.title = element_blank())
Z
ggsave("./Plots/dhslocs_imp.png", width = 9, height = 6)

##Kids sco 5--------------
A5 <- 
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  #geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") + # old coloring - leaving in case want to go back
  #geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=prev), alpha=0.9) + 
  labs(color='') + 
  theme_bw(base_size=14) + 
  #scale_color_distiller(palette = "Greens", direction = 1) +
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
#        legend.position = c(.29, .16), # if want legend inside map outline
#        legend.background = element_blank()

A5
ggsave("./Plots/clusterhbvprev.png", width = 9, height = 6)

### v for supp---------
A5_sup <- 
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=prev), alpha=0.8) + 
  labs(color='') + 
  theme_bw(base_size=14) + 
  #scale_color_distiller(palette = "Greens", direction = -1) +
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("S/CO cutoff of 5.00 (main analysis)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        #        legend.position = c(.29, .16), # if want legend inside map outline
        #        legend.background = element_blank()
  )
A5_sup
##Kids sco 1--------------
A1 <- 
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=prev1), alpha=0.8) + 
  labs(color='') + 
  theme_bw(base_size=14) + 
  #scale_color_distiller(palette = "Greens", direction = -1) +
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("S/CO cutoff of 1.00")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))
A1
ggsave("./Plots/kid_sco1.png", width = 9, height = 6)

##Kids sco 2--------------
A2 <- 
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=prev2), alpha=0.8) + 
  labs(color='') + # HBV prevalence (%) \nin children < 5
  theme_bw(base_size=14) + 
  #scale_color_distiller(palette = "Greens", direction = -1) +
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("S/CO cutoff of 2.00")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))
A2
ggsave("./Plots/kid_sco2.png", width = 9, height = 6)

##Kids sco 100--------------
A100 <- 
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=prev100), alpha=0.8) + 
  labs(color='') + #HBV prevalence (%) \nin children < 5
  theme_bw(base_size=14) + 
  #scale_color_distiller(palette = "Greens", direction = -1) +
  scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("S/CO cutoff of 100.00")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

A100
ggsave("./Plots/kid_sco100.png", width = 9, height = 6)


## Tetanus Ab------------
# tet lower is indeterminants as not reactive (lower bound)
tetlow <- 
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=tetcovlower), alpha=0.8) + 
  labs(color='% in cluster') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral', direction=1) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Children ≤5 with detectable Tetanus antibodies (lower bound)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.position = c(.29, .16),
        legend.background = element_blank())
tetlow

tetupp <- 
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=tetcovupper), alpha=0.8) + 
  labs(color='% in cluster') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = 'Spectral', direction=1) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Children ≤5 with detectable Tetanus antibodies (upper bound)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.position = c(.29, .16),
        legend.background = element_blank())
tetupp

# since these don't appear different, plot of where kids had indeterminant tetanus ab
tetdiff <- 
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=tetdiff), alpha=0.8) + 
  labs(color='% in cluster') + 
  theme_bw(base_size=14) + 
  #scale_color_distiller(palette = 'Grays', direction=1) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Indeterminant tetanus serology")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.position = c(.29, .16),
        legend.background = element_blank())
tetdiff
tetlow + tetupp + tetdiff + plot_layout(nrow=1, ncol = 3) #+ plot_annotation(tag_levels = 'A')
ggsave('./Plots/tet_imp.png', width=15, height=6)

#Kriging-------------
# kriging using gstat: https://rpubs.com/nabilabd/118172 
# https://mgimond.github.io/Spatial/interpolation-in-r.html#generate-the-variance-and-confidence-interval-maps

##sco5----------
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

# conduct kriging: HBsAg sco5 prev
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

B5 <- 
  ggplot() + 
  geom_tile(data=(krige %>% as.data.frame), aes(x=gdf.x,y=gdf.y,fill=as.factor(var1.pred_largegrp2))) + 
  geom_sf(data=admin0 %>% filter(ISO != 'COD'), fill="snow3", color="snow4") +
  geom_sf(data=drcprov, fill=NA, color="gray39", size=0.75) + 
  labs(fill="", x='', y='') + 
  theme_bw(base_size=14) + 
  #scale_fill_brewer(palette ="Greens", labels=c("0","1-2","3-4","5-6","7-8","9-10","11-12","13-14"))+
  scale_fill_brewer(palette ="Spectral", direction=-1, labels=c("0","1-2","3-4","5-6","7-8","9-10","11-12","13-14"))+
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  #ggtitle("Predicted HBsAg prevalence")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))
B5
ggsave("./Plots/krighbvprev.png", width = 9, height = 6)

### v for supp-----------
B5_supp <- 
  ggplot() + 
  geom_tile(data=(krige %>% as.data.frame), aes(x=gdf.x,y=gdf.y,fill=as.factor(var1.pred_largegrp2))) + 
  geom_sf(data=admin0 %>% filter(ISO != 'COD'), fill="snow3", color="snow4") +
  geom_sf(data=drcprov, fill=NA, color="gray39", size=0.75) + 
  labs(fill="", x='', y='') + 
  theme_bw(base_size=14) + 
  scale_fill_brewer(palette ="Spectral", direction=-1, labels=c("0","1-2","3-4","5-6","7-8","9-10","11-12","13-14"))+
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  #ggtitle("S/CO cutoff of 5.00 (main analysis)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))
B5_supp
## sco1----------
# conduct kriging: prev1 for sco1
m.kriged.sco1 <- gstat::krige(prev1~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged.sco1$var1.pred)

# assign points into bins
krige_sco1 <- m.kriged.sco1 %>% cbind(gdf$x, gdf$y) %>% mutate(
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

B1 <- 
ggplot() + 
  geom_tile(data=(krige_sco1 %>% as.data.frame), aes(x=gdf.x,y=gdf.y,fill=as.factor(var1.pred_largegrp2))) + 
  geom_sf(data=admin0 %>% filter(ISO != 'COD'), fill="snow3", color="snow4") +
  geom_sf(data=drcprov, fill=NA, color="gray39", size=0.75) + 
  labs(fill="", x='', y='') + 
  theme_bw(base_size=14) + 
  scale_fill_brewer(palette ="Spectral", direction=-1, labels=c("0","1-2","3-4","5-6","7-8","9-10","11-12","13-14"))+
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  #ggtitle("S/CO cutoff of 1.00")+ # redundant with A columns
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.title = element_blank())
B1

## sco2----------
# conduct kriging: prev2 for sco2
m.kriged.sco2 <- gstat::krige(prev2~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged.sco2$var1.pred)

# assign points into bins
krige_sco2 <- m.kriged.sco2 %>% cbind(gdf$x, gdf$y) %>% mutate(
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

B2 <- 
  ggplot() + 
  geom_tile(data=(krige_sco2 %>% as.data.frame), aes(x=gdf.x,y=gdf.y,fill=as.factor(var1.pred_largegrp2))) + 
  geom_sf(data=admin0 %>% filter(ISO != 'COD'), fill="snow3", color="snow4") +
  geom_sf(data=drcprov, fill=NA, color="gray39", size=0.75) + 
  labs(fill="", x='', y='') + 
  theme_bw(base_size=14) + 
  scale_fill_brewer(palette ="Spectral", direction=-1, labels=c("0","1-2","3-4","5-6","7-8","9-10","11-12","13-14"))+
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  #ggtitle("S/CO cutoff of 2.00")+ # redundant with A columns
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.title = element_blank())
B2

## sco100----------
# conduct kriging: prev1 for sco1
m.kriged.sco100 <- gstat::krige(prev100~1, output_points, st_as_sf(grd_pts_in), model=m.fit)
summary(m.kriged.sco100$var1.pred)

# assign points into bins
krige_sco100 <- m.kriged.sco100 %>% cbind(gdf$x, gdf$y) %>% mutate(
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

B100 <- 
  ggplot() + 
  geom_tile(data=(krige_sco100 %>% as.data.frame), aes(x=gdf.x,y=gdf.y,fill=as.factor(var1.pred_largegrp2))) + 
  geom_sf(data=admin0 %>% filter(ISO != 'COD'), fill="snow3", color="snow4") +
  geom_sf(data=drcprov, fill=NA, color="gray39", size=0.75) + 
  labs(fill="", x='', y='') + 
  theme_bw(base_size=14) + 
  scale_fill_brewer(palette ="Spectral", direction=-1, labels=c("0","1-2","3-4","5-6","7-8","9-10","11-12","13-14"))+
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  # ggtitle("S/CO cutoff of 100.00")+# redundant with A columns
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.title = element_blank())
B100


##Tet------------
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
D
# C and D are for tetanus serology, but prob not showing with main Hbsag findings
# A + B + C + D + plot_layout(nrow=2, ncol = 2) + plot_annotation(tag_levels = 'A')
# ggsave('./Plots/2x2.png', width=15, height=9)

# where left off Nov/Dec 2023----
# consider centroid and way of sampling multiple

# wtd prov level choropleth for 1/2/100
# Choropleth ------------------
# drc prov bound if not read in elsewhere
drcprov = st_read("/Users/camillem/Documents/GitHub/hbv_hover/adm1/GLOBAL_ADM1.shp", stringsAsFactors = FALSE) %>% filter(ADM0_NAME=="DEMOCRATIC REPUBLIC OF THE CONGO") %>%   st_transform(4326)

# weighted by province
library(survey)
library(srvyr)
#df5 <- # figure out how to transpose 
svyby(~prov2015,~hbvresult5, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% clipr::write_clip()
svyby(~prov2015,~hbvresult1, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% clipr::write_clip()
svyby(~prov2015,~hbvresult2, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% clipr::write_clip()
svyby(~prov2015,~hbvresult100, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% clipr::write_clip()

wtdctskids <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/Peyton K DHS/Results discussions/prov counts.xlsx",
                         sheet = "Dec 7form")

wtdctskids$total <- wtdctskids$hbvpos5 + wtdctskids$hbvneg5
wtdctskids$prev5 <- (wtdctskids$hbvpos5/wtdctskids$total)*100
wtdctskids$prev1 <- (wtdctskids$hbvpos1/wtdctskids$total)*100
wtdctskids$prev2 <- (wtdctskids$hbvpos2/wtdctskids$total)*100
wtdctskids$prev100 <- (wtdctskids$hbvpos100/wtdctskids$total)*100
view(wtdctskids)

## WHAT ARE PROV NAMES - MAKE SURE THEY MATCH THE DRCPROV OBJECT
wtdctskids$ADM1_NAME <- toupper(wtdctskids$provnamesimp)
drcprov_hbvkids <- left_join(drcprov,wtdctskids, by="ADM1_NAME")
view(drcprov_hbvkids)

prov5 <-
  ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_hbvkids,  mapping=aes(fill=prev5))+
  scale_fill_distiller(palette = 'BuPu', direction = 1) + # Greens
#  scale_fill_distiller(palette = 'Spectral') +
  theme_bw(base_size=14) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.title = element_blank())
ggsave("./Plots/kidprev_wtdall.png", width = 6, height = 6)

##Main maps together---------
Z + A5 + B5 + prov5 + plot_layout(nrow=1, ncol = 4) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps_imp.png', width=15, height=6)

##all sensitivity analyses together
A1 + B1 + A2 + B2 + A5_sup + B5_supp + A100 + B100 + plot_layout(nrow=4, ncol = 2) #+ plot_annotation(tag_levels = 'A')
ggsave('./Plots/sens_maps.png', width=12, height=20)


prov5_sup <-
  ggplot()+
  geom_sf(data=admin0, fill="snow2", color="snow3") +
  geom_sf(data=drcprov_hbvkids,  mapping=aes(fill=prev5))+
  scale_fill_distiller(palette = 'Greens', direction = 1) +
  #  scale_fill_distiller(palette = 'Spectral') +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("S/CO of 5.00 (main analysis)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

prov1 <-
  ggplot()+
  geom_sf(data=admin0, fill="snow2", color="snow3") +
  geom_sf(data=drcprov_hbvkids,  mapping=aes(fill=prev1))+
  scale_fill_distiller(palette = 'Greens', direction = 1) +
  #  scale_fill_distiller(palette = 'Spectral') +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("S/CO of 1.00")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

prov2 <-
  ggplot()+
  geom_sf(data=admin0, fill="snow2", color="snow3") +
  geom_sf(data=drcprov_hbvkids,  mapping=aes(fill=prev2))+
  scale_fill_distiller(palette = 'Greens', direction = 1) +
  #  scale_fill_distiller(palette = 'Spectral') +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("S/CO of 2.00")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

prov100 <-
  ggplot()+
  geom_sf(data=admin0, fill="snow2", color="snow3") +
  geom_sf(data=drcprov_hbvkids,  mapping=aes(fill=prev100))+
  scale_fill_distiller(palette = 'Greens', direction = 1) +
  #  scale_fill_distiller(palette = 'Spectral') +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("S/CO of 100.00")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

prov1 + prov2 + prov5_sup + prov100 + plot_layout(nrow=4, ncol = 1) # + plot_annotation(tag_levels = 'A')
ggsave('./Plots/sens_prov_wtd.png', width=6, height=9)


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

# Prev diffs, age x sex x province mapped------
pds <- wtdagesexprov_lg %>% filter(grepl('pd', variable))
view(pds)

limit2 <- max(abs(pds$value)) * c(-1, 1)
pdorder <- c("Male vs. female, 0","Male vs. female, 1","Male vs. female, 2","Male vs. female, 3","Male vs. female, 4","Male vs. female, 5")

pds <- pds %>% mutate(pdlabel = case_when(
  variable == "pd0" ~ "Male vs. female, 0",
  variable == "pd1" ~ "Male vs. female, 1",
  variable == "pd2" ~ "Male vs. female, 2",
  variable == "pd3" ~ "Male vs. female, 3",
  variable == "pd4" ~ "Male vs. female, 4",
  variable == "pd5" ~ "Male vs. female, 5"
))
view(pds)

pds %>% group_by(prov_name,pdlabel ) %>% reframe(value)
hist(pds$value)
ggplot()+
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=pds, mapping=aes(fill=value))+
  #scale_fill_brewer(palette = "Greens", labels) +
  #scale_fill_continuous_diverging("Blue-Red 3")+
  scale_fill_distiller(palette = 'RdBu', limit = limit2) +
#  scale_fill_manual(values = c("#e5f5f9","#99d8c9","#41ae76", "#238b45","#00441b") , labels=c("0","<5%","5-10%","10-20%", ">20%")) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Weighted HBV prevalence in kids ≤5")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))+
  facet_wrap(~factor(pdlabel, levels = pdorder),  ncol = 6, nrow=1)
ggsave('./Plots/provsexpdage_wtd.png', width=12, height=6)

# PD overall
# by sex overall
sexprov <- as.data.frame(svytable(~ prov2015 + sex + hbvresultlowna, designf_dhs2) %>% clipr::write_clip())
class(sexprov)



#Overall boys/girls----------------
sexprov <- svytable(~ prov2015 + sex + hbvresult5, designf_dhs2) %>% clipr::write_clip()

save <- svytable(~ prov2015 + agesex, designf_dhs2) %>% clipr::write_clip()
save
sexprov <- as.data.frame(svytable(~ prov2015 + sex + hbvresult5, designf_dhs2) %>% clipr::write_clip())
view(sexprov)

sexprov_w <- reshape(sexprov, idvar = c("prov2015", "sex"), timevar = "hbvresult5", direction = "wide")
view(sexprov_w)

sexprov_w$total <- sexprov_w$Freq.1 + sexprov_w$Freq.0
sexprov_w$prev <- 100*(sexprov_w$Freq.1/sexprov_w$total)
sexprov_w <- sexprov_w %>% rename(count_pos = Freq.1, count_neg = Freq.0)
sexprov_w_w <- reshape(sexprov_w, idvar = "prov2015", timevar = "sex", direction = "wide")
view(sexprov_w_w)
sexprov_w_w$pd <- sexprov_w_w$prev.1 - sexprov_w_w$prev.0


sexprov_w_w$prov2015nohy <- gsub("-"," ", sexprov_w_w$prov2015)
# sexprov_w_w <- sexprov_w_w %>% rename(prov_name = prov2015nohy)

sexprov_w_w$ADM1_NAME <- toupper(sexprov_w_w$prov2015)
view(sexprov_w_w)

table(drcprov$hyphen)
table(sexprov_w_w$prov2015)
sexprov_w_w <- sexprov_w_w %>% rename(hyphen = prov2015)


test <- left_join(drcprov[, c("hyphen", "geometry")],sexprov_w_w, by="hyphen")
#wtdagesexprov_lg <- left_join(wtdagesexprov_l, drcprov[, c("prov_name", "geometry")], by="prov_name")
view(test)


wtdagesexprov_lg <- wtdagesexprov_lg %>% mutate(prev_bin = case_when(
  value == 0 ~ 0, #"0",
  value > 0 & value <5 ~ 1, #"<5%",
  value >= 5 & value <10 ~ 2, #"5-10%",
  value >= 10 & value <20 ~ 3, #"10-20%",
  value >= 20  ~ 4, #">20%",
) %>%  as.factor())
table(wtdagesexprov_lg$prev_bin, useNA = "always")
class(wtdagesexprov_lg$prev_bin)

# Map Male vs female prev diffs, all ages
limit <- max(abs(test$pd)) * c(-1, 1)

ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  #geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=test, mapping=aes(fill=pd))+
  #scale_fill_distiller(palette = 'RdBu', limit = limit) +
  scale_fill_continuous_diverging("Blue-Red 2", limit = limit)+
  #scale_fill_distiller(palette = 'Spectral') +
  #scale_fill_manual(values = c("#3288BD","#FFFFBF","#FDAE61", "#9E0142") , labels=c("0","≤2%","2-7%",">7%")) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme_bw(base_size=14) + 
  ggtitle("Prevalence difference (male vs. female)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.title = element_blank())#
ggsave('./Plots/provsex_wtd.png', width=6, height=6)


#Moran's i-------------
library(ape)
Moran.I(uniquescar$scarprev, test.hhdist.inv)




