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



