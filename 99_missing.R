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




