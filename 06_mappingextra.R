# 06_mapping extra
library(tidyverse)
library(sf)
library(colorspace)
library(gstat)
library(stars)
library(tidyverse)
library(patchwork)
library(sp)
library(viridis)
# TO USE from 01_datacleaning_revR: elig_kids_whbvres_wt_kr
# should be using cluster number to impute, not cluster/hh!!!


# n=28 clusters with missing when using this - concerning about hhsum_all?
gpsmiss <- elig_kids_whbvres_wt_kr %>% filter(latnum=="0"| is.na(latnum)) %>% select(hv001, prov2015, hv026,latnum, longnum) %>% rename(prov_name = prov2015) %>% distinct(hv001, .keep_all=T)
nrow(gpsmiss)
# n=46 clusters with missing
# count missing by cluster
gpsmiss %>% group_by(prov_name) %>% summarise(n=n())

# view counts by province 
ctsamp <- gpsmiss %>% group_by(prov_name) %>% count()
view(ctsamp)
# hv026 Place of residence, 0  Capital, large city, 1  Small city, 2  Town, 3  Countryside
write.csv(gpsmiss, file = "/Users/camillem/Documents/GitHub/dhs_hbv/Data/gpsmiss_impute.csv")

gpsmiss2 <- left_join(gpsmiss, drcprov[, c("prov_name", "geometry")], by = "prov_name")
view(gpsmiss2)

#Impute GPS in prov bounds---------------- 
# set seed
set.seed(10272023)
sf_use_s2(F)
points <- st_sample(gpsmiss2$geometry, size = c(1,1), type = "random") #, by_polygon = T ; type=random
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
  geom_sf(data=drcprov, color="tan4", size=0.5) + 
  geom_sf(data=points) 

gpsmiss_imp$hv001 <- as.numeric(gpsmiss_imp$hv001)
elig_kids_whbvres_wt_kr$hv001 <- as.numeric(elig_kids_whbvres_wt_kr$hv001)
kidsmap_imp <- left_join(elig_kids_whbvres_wt_kr[, c("hv001","cluster_hh","kids_barcode","prov2015","hbvresult5", "hbvresult1","hbvresult2","hbvresult100","dpt_count","tetab","shteta","shtetaindasno", "shtetaindasyes","geometry", "latnum", "longnum", "hv105_fromhc1_f", "sex")], gpsmiss_imp[, c("hv001","lat_imp","lon_imp","imputed")], by = "hv001" )

kidsmap_imp$lat_rev <- ifelse(kidsmap_imp$latnum=="0" | is.na(kidsmap_imp$latnum), kidsmap_imp$lat_imp, kidsmap_imp$latnum)
kidsmap_imp$long_rev <- ifelse(kidsmap_imp$longnum=="0"| is.na(kidsmap_imp$latnum), kidsmap_imp$lon_imp, kidsmap_imp$longnum)
kidsmap_imp$imputed <- ifelse(is.na(kidsmap_imp$imputed), 0,kidsmap_imp$imputed)

kidsmap_imp <- kidsmap_imp %>% select(-c(geometry.x,geometry.y))
# count how many combined lat missing
kidsmap_imp %>% filter(is.na(lat_rev)) %>% count()

kidsmap_impsf <- st_as_sf(kidsmap_imp[!is.na(kidsmap_imp$lat_rev) &!is.na(kidsmap_imp$long_rev),], coords = c("long_rev", "lat_rev"), crs = 4326) 


# none - all missing gps addressed
test2 <- kidsmap_imp %>% filter(is.na(lat_rev)) 
view(test2)
# none - all missing gps addressed
test3 <- test2 %>% group_by(hv001,prov2015) %>% summarise(n=n())
view(test3)

# Main maps-------------------------------
# number of distinct households per cluster and number of kids per cluster
test <- elig_kids_whbvres_wt_kr %>% group_by(hv001) %>% summarize(nhh = n_distinct(hv002), nkids = n())
view(test)

dhsmeta <- readRDS("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/IDEEL/HepB/DHS_pr_full_merge_backup.rds")

dhsmeta_kids <- dhsmeta %>% filter(kids==1)

# from 06_mappingextra.R, use kidsmap_impsf, which has imputed GPS
sf_use_s2(T)


summary(output_df$n)
# subset cluster level df to get weights
output_df <- kidsmap_impsf %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   imputed = mean(imputed),
                   prev = mean(hbvresult5, na.rm=T)*100,
                   prev1 = mean(hbvresult1, na.rm=T)*100,
                   prev2 = mean(hbvresult2, na.rm=T)*100,
                   prev100 = mean(hbvresult100, na.rm=T)*100,
                   avg_pentdos = mean(dpt_count),
                   tetreact = mean(tetab=="1")) %>% 
  mutate(prev_grp = case_when(
            prev == 0 ~ 0,
            prev > 0 & prev<= 5.5 ~ 1,
            prev > 5.5 & prev<= 10.49 ~ 2,
            prev > 10.49 & prev<= 15.49 ~ 3,
            prev > 15.49  ~ 4),
          prev_grp_f = factor(prev_grp,
            levels = c(0,1,2,3,4),
            labels = c("0%", ">0-5%", ">5-10%", ">10-15%",">15%")),
          prev1_grp = case_when(
            prev1 == 0 ~ 0,
            prev1 > 0 & prev1<= 5.5 ~ 1,
            prev1 > 5.5 & prev1<= 10.49 ~ 2,
            prev1 > 10.49 & prev1<= 15.49 ~ 3,
            prev1 > 15.49  ~ 4),
          prev1_grp_f = factor(prev1_grp,
            levels = c(0,1,2,3,4),
            labels = c("0%", ">0-5%", ">5-10%", ">10-15%",">15%")),
          prev2_grp = case_when(
            prev2 == 0 ~ 0,
            prev2 > 0 & prev2<= 5.5 ~ 1,
            prev2 > 5.5 & prev2<= 10.49 ~ 2,
            prev2 > 10.49 & prev2<= 15.49 ~ 3,
            prev2 > 15.49  ~ 4),
          prev2_grp_f = factor(prev2_grp,
            levels = c(0,1,2,3,4),
            labels = c("0%", ">0-5%", ">5-10%", ">10-15%",">15%")),
          prev100_grp = case_when(
            prev100 == 0 ~ 0,
             prev100 > 0 & prev100<= 5.5 ~ 1,
             prev100 > 5.5 & prev100<= 10.49 ~ 2,
             prev100 > 10.49 & prev100<= 15.49 ~ 3,
             prev100 > 15.49  ~ 4),
          prev100_grp_f = factor(prev100_grp,
             levels = c(0,1,2,3,4),
             labels = c("0%", ">0-5%", ">5-10%", ">10-15%",">15%")))

# for subset with dpt reporting
dpt <- kidsmap_impsf %>% filter(!is.na(dpt_count)) %>% 
  group_by(hv001) %>%
  dplyr::summarize(n=n(),
                   imputed = mean(imputed),
                   avg_pentdos = mean(dpt_count))
# weights from main dataset
# weights are different at the child, household, as well as cluster level
# wts <- elig_kids_whbvres_wt_kr %>% group_by(hv001) %>% select(c("hv001","both_wt_new", "both_wt_old","iptw_s","hv028_div","hh_weight"))

# output_df <- merge(output_df, elig_kids_whbvres_wt_kr[,c("hv001","both_wt_new", "both_wt_old","iptw_s","hv028_div","hh_weight")],by="hv001", all.x = TRUE)


# GADM boundaries from: https://gadm.org/download_country_v3.html
admin0 <- readRDS('/Users/camillem/Documents/GitHub/animalaria/admin0.rds') %>%          # GADM admin0 boundaries
  st_transform(4326) %>% # set at ESPG 4326
  filter(grepl('Congo|Rwanda|Tanzania|Burundi|African Republic|Angola|Zambia|Uganda|Sudan|Gabon|Cameroon|Equatorial Guinea', Country)) 

st_crs(admin0) # view CRS

DRC <- admin0 %>% filter(Country=='Democratic Republic of the Congo') # DRC

# remove points where geometry is outside of DRC outline (geometry=c(0,0))
output_points <- st_join(output_df, DRC, join = st_intersects) %>% filter(!is.na(Country))
dpt_points <- st_join(dpt, DRC, join = st_intersects) %>% filter(!is.na(Country))

# simple map of dhs cluster locations with kid samples
drcprov = st_read("/Users/camillem/Documents/GitHub/hbv_hover/adm1/GLOBAL_ADM1.shp", stringsAsFactors = FALSE) %>% filter(ADM0_NAME=="DEMOCRATIC REPUBLIC OF THE CONGO") %>%  st_transform(4326)
drccities = st_read("/Users/camillem/Documents/GitHub/dhs_hbv/Data/cod_cities_20180906h/COD_CITIES_20180906H.shp", stringsAsFactors = FALSE) %>% filter(estimate20 > 200000 & name != "Kananga") %>%   st_transform(4326) # Mbuji-Mayi too close to Kananga

drcwat = st_read("/Users/camillem/Documents/GitHub/hbv_hover/COD_wat/COD_water_areas_dcw.shp", stringsAsFactors = FALSE)   %>% filter(HYC_DESCRI == "Perennial/Permanent" & NAME != "UNK") %>%  st_transform(4326) #%>% filter(NAME=="CONGO" | NAME == "LUALABA (CONGO)" | NAME == "OUBANGUI(UBANGI)")
view(drcwat)

## DHS clust------------
#Z <-
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, alpha=0.8, aes(shape=as.factor(imputed), color=as.factor(imputed))) + 
  #labs(color='') + 
  geom_sf(data=drcwat, color="steelblue")+
  geom_sf(data=drccities, color = "black", shape = "18")+
  geom_sf_text(data=drccities, aes(label = name))+
  theme_bw(base_size=14) + 
  scale_color_manual(values = c("gray30","mediumpurple2"), labels = c("Original", "Imputed") ) + 
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
library(RColorBrewer)
library(ggrepel)
my_purd = brewer.pal(n = 9, "PuRd")[3:9] #there are 9, I exluded the two lighter hues
my_greens = c("#EAF7E6","#BBE3B4","#41ae76", "#238b45","#00441b") #there are 9, I exluded the two lighter hues
my_gregra = c("gray47","#BBE3B4","#41ae76", "#238b45","#00441b") #there are 9, I exluded the two lighter hues
my_gregra6 = c("#F7F7F7","#EAF7E6",'#afddb3', '#72c18d', '#39a468', '#10632f', '#00441b') #there are 9, I exluded the two lighter hues

A5 <- 
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  #geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") + # old coloring - leaving in case want to go back
  #geom_sf(data=DRC, fill="cornsilk") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=prev_grp_f, shape=as.factor(imputed))) + #, alpha=0.9
    geom_sf(data=drcwat, color="steelblue")+
    geom_sf(data=drccities, color = "black", shape = "18")+
    geom_sf_text(data=drccities, aes(label = name), size = 3, fontface = "bold")+
    labs(color='', shape = '') + 
  scale_shape_discrete(labels = c("Original","Imputed"))+
  theme_bw(base_size=10) + 
  scale_color_manual(values = my_gregra)+
    #scale_color_brewer(palette = "RdGy", direction = -1) + #YlOrRd
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  #ggtitle("Children ≤ 5")+ # for adding with adult figure
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        #legend.text = element_text(size = 8),
        panel.background = element_rect(fill="#daeff8", color=NA))
#        legend.position = c(.29, .16), # if want legend inside map outline
#        legend.background = element_blank()
A5
ggsave("./Plots/prev5clust.png", width = 9, height = 6)

### v for supp---------
A5_sup <- 
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=output_points, aes(color=prev_grp_f, shape=as.factor(imputed)), alpha=0.9, size=2) + 
  labs(color='', shape = '') + 
  scale_shape_discrete(labels = c("Original","Imputed"))+
  theme_bw(base_size=14) + 
  scale_color_brewer(palette = "YlGn", direction = 1) + #YlOrRd
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
  geom_sf(data=output_points, aes(color=prev1_grp_f, shape=as.factor(imputed)), alpha=0.9, size=2) + 
  labs(color='', shape = '') + 
  scale_shape_discrete(labels = c("Original","Imputed"))+
  theme_bw(base_size=14) + 
  scale_color_brewer(palette = "YlGn", direction = 1) + #YlOrRd
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
  geom_sf(data=output_points, aes(color=prev2_grp_f, shape=as.factor(imputed)), alpha=0.9, size=2) + 
  labs(color='', shape = '') + 
  scale_shape_discrete(labels = c("Original","Imputed"))+
  theme_bw(base_size=14) + 
  scale_color_brewer(palette = "YlGn", direction = 1) + #YlOrRd
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
  geom_sf(data=output_points, aes(color=prev100_grp_f, shape=as.factor(imputed)), alpha=0.9, size=2) + 
  labs(color='', shape = '') + 
  scale_shape_discrete(labels = c("Original","Imputed"))+
  theme_bw(base_size=14) + 
  scale_color_brewer(palette = "YlGn", direction = 1) + #YlOrRd
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

#Reported DPT vaccination------
dptmap <-
  ggplot() + 
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=DRC, fill="snow2") +
  geom_sf(data=DRC, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=drcprov, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=dpt_points, aes(color=avg_pentdos), alpha=0.8) + 
  labs(color='') + 
  theme_bw(base_size=14) + 
  scale_color_distiller(palette = "RdYlBu", direction = 1) + #YlOrRd
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  ggtitle("Average DPT doses")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.position = c(.29, .16),
        legend.background = element_blank())
dptmap
ggsave('./Plots/dpt_map.png', width=15, height=6)

## Tetanus Ab------------



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

# weighted by province------------
library(survey)
library(srvyr)
library(readxl)
mainprov5 <- svyby(~prov2015,~hbvresult5, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
mainprov5 <- mainprov5 %>% mutate(level = paste0('hbv5.', level))
mainprov5t <- as.data.frame(t(as.data.frame(mainprov5))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))
mainprov5t <- mainprov5t %>% mutate(across(-c(level), as.numeric))
view(mainprov5t)

mainprov1 <- svyby(~prov2015,~hbvresult1, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
mainprov1 <- mainprov1 %>% mutate(level = paste0('hbv1.', level))
mainprov1t <- as.data.frame(t(as.data.frame(mainprov1))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))
mainprov1t <- mainprov1t %>% mutate(across(-c(level), as.numeric))

mainprov2 <- svyby(~prov2015,~hbvresult2, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
mainprov2 <- mainprov2 %>% mutate(level = paste0('hbv2.', level))
mainprov2t <- as.data.frame(t(as.data.frame(mainprov2))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))
mainprov2t <- mainprov2t %>% mutate(across(-c(level), as.numeric))

mainprov100 <- svyby(~prov2015,~hbvresult100, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
mainprov100 <- mainprov100 %>% mutate(level = paste0('hbv100.', level))
mainprov100t <- as.data.frame(t(as.data.frame(mainprov100))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))
mainprov100t <- mainprov100t %>% mutate(across(-c(level), as.numeric))

mainprovs <- left_join(mainprov5t, mainprov1t, by='level') %>%
  left_join(., mainprov2t, by='level') %>% left_join(., mainprov100t, by = 'level')

view(mainprovs)

mainprovs <- mainprovs %>% mutate(
  prev5 = 100*(hbv5.1/(hbv5.0 + hbv5.1)),
  prev1 = 100*(hbv1.1/(hbv1.0 + hbv1.1)),#,
  prev2 = 100*(hbv2.1/(hbv2.0 + hbv2.1)),#,
  prev100 = 100*(hbv100.1/(hbv100.0 + hbv100.1)))
mainprovs$hyphen <- gsub("prov2015", "", mainprovs$level)

mainprovs_m <- mainprovs %>% select(c("hyphen", starts_with("prev"))) %>% melt()
view(mainprovs_m)
table(mainprovs_m$value)
mainprovs_m <- mainprovs_m %>% mutate(value_g = case_when(
    value == 0 ~ 0,
    value >0 & value <0.5 ~ 1,
    value >0.5 & value <1.5 ~ 2,
    value >1.5 & value <2.5 ~ 3,
    value >2.5 & value <3.5 ~ 4,
    value >3.5 & value <4.5 ~ 5,
    value >4.5 ~ 6),
  value_gf = factor(value_g,
                    levels = c(0,1,2,3,4,5,6),
                    labels = c("0%", "<1%","1%","2%", "3%","4%","5+%")))

drcprov_sens <- left_join( drcprov[,c("hyphen", "geometry")], mainprovs_m, by="hyphen")

#prov_all <-
ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_sens,  mapping=aes(fill=value))+
  scale_fill_distiller(palette = 'Greens', direction = 1) + #  YlGn
  theme_bw(base_size=14) + 
  geom_sf_text(data= drcprov_sens[drcprov_sens$hyphen!="Kasai-Oriental" & drcprov_sens$hyphen!="Kasai-Central" &drcprov_sens$hyphen!="Tshopo" &drcprov_sens$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 2) + #fontface = "bold"
  geom_sf_text(data= drcprov_sens[drcprov_sens$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_sens[drcprov_sens$hyphen=="Tshopo",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = .3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_sens[drcprov_sens$hyphen=="Kasai-Central",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_sens[drcprov_sens$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 2) + #, fontface = "bold"
  #  geom_text_repel(data= colab, mapping=aes(x = X, y = Y, label = hyphen),
  #               size = 3, fontface = "bold"  ) +
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  labs(fill = "HBsAg prevalence (%)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA)#,
        #legend.title = element_blank()
        )+
  facet_wrap(~factor(variable, levels = c("prev1","prev2","prev5", "prev100"), labels = c("S/CO 1", "S/CO 2", "S/CO 5 (main analysis)", "S/CO 100")),
             nrow = 4, ncol = 1)
ggsave("./Plots/prov_all_vert.png", width = 6, height = 12)

# original
prov5 <-
  ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_sens[drcprov_sens$variable=="prev5",],  mapping=aes(fill=value_gf))+
  scale_fill_manual(values = my_gregra6)+
  #scale_fill_distiller(palette = 'Greens', direction = 1) + #  YlGn
#  scale_fill_distiller(palette = 'Spectral') +
  theme_bw(base_size=10) + 
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen!="Kasai-Oriental" & drcprov_agesex$hyphen!="Kasai-Central" &drcprov_agesex$hyphen!="Tshopo" &drcprov_agesex$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 3) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 3) + #,, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Tshopo",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = .3, size = 3) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Kasai-Central",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.3, size = 3) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 3) + #, fontface = "bold"
  labs(fill="HBsAg prevalence")+
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8)
        )
prov5
ggsave("./Plots/kidprev_wtdall_f.png", width = 6, height = 6)

##Main maps together---------
A5 + prov5 + plot_layout(nrow=1, ncol = 2) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps2_imp3.png', width=12, height=6)




##all sensitivity analyses together-----------
A1 + B1 + A2 + B2 + A5_sup + B5_supp + A100 + B100 + plot_layout(nrow=4, ncol = 2) #+ plot_annotation(tag_levels = 'A')
ggsave('./Plots/sens_maps.png', width=12, height=20)


prov5_sup <-
  ggplot()+
  geom_sf(data=admin0, fill="snow2", color="snow3") +
  geom_sf(data=drcprov_hbvkids,  mapping=aes(fill=prev5))+
  scale_fill_distiller(palette = 'Greens', direction = 1) +
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen!="Kasai-Oriental" & drcprov_agesex$hyphen!="Kasai-Central" &drcprov_agesex$hyphen!="Tshopo" &drcprov_agesex$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 2) + #fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Tshopo",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = .3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Kasai-Central",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 2) + #, fontface = "bold"
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
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen!="Kasai-Oriental" & drcprov_agesex$hyphen!="Kasai-Central" &drcprov_agesex$hyphen!="Tshopo" &drcprov_agesex$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 2) + #fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Tshopo",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = .3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Kasai-Central",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 2) + #, fontface = "bold"
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
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen!="Kasai-Oriental" & drcprov_agesex$hyphen!="Kasai-Central" &drcprov_agesex$hyphen!="Tshopo" &drcprov_agesex$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 2) + #fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Tshopo",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = .3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Kasai-Central",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 2) + #, fontface = "bold"
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
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen!="Kasai-Oriental" & drcprov_agesex$hyphen!="Kasai-Central" &drcprov_agesex$hyphen!="Tshopo" &drcprov_agesex$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 2) + #fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Tshopo",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = .3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Kasai-Central",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.3, size = 2) + #, fontface = "bold"
  geom_sf_text(data= drcprov_agesex[drcprov_agesex$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 2) + #, fontface = "bold"
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
wtdage <- kidsmap_impsf %>% select(hv001,cluster_hh, prov2015, hv105_fromhc1_f, sex, hbvresult5, hbvresult1, hbvresult2, hbvresult100, geometry, tetab, dpt_count) 
output <- wtdage %>% group_by(hv105_fromhc1_f, hv001) %>% dplyr::summarize(n=n(),
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




