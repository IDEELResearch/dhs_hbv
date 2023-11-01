# 06_mapping extra

# for clusters with missing GPS but province given, impute GPS

gpsmiss <- hhsum_all %>% filter(latnum==0) %>% select(cluster_hh, prov2015, hv026,latnum, longnum) %>% rename(prov_name = prov2015)
view(gpsmiss)
# view counts by province 
ctsamp <- gpsmiss %>% group_by(prov2015 , hv026) %>% count()
# hv026 Place of residence, 0  Capital, large city, 1  Small city, 2  Town, 3  Countryside
write.csv(gpsmiss, file = "/Users/camillem/Documents/GitHub/dhs_hbv/Data/gpsmiss_impute.csv")

gpsmiss <- left_join(gpsmiss, drcprov[, c("prov_name", "geometry")], by = "prov_name")
gpsmiss$lat_imp <- NA_real_
gpsmiss$long_imp <- NA_real_

# set seed
set.seed(88)
points <- st_sample(gpsmiss$geometry, size = c(1,1), type = "random") #, by_polygon = T 
plot(st_geometry(points))

# need to figure out how to append these points onto the metadata (cluster_hh) used to identify them.

