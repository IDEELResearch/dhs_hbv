# Practice with spatial modeling 
## https://www.paulamoraga.com/book-geospatial/sec-geostatisticaldataexamplespatial.html
library(geoR)
# practice data from geoR
data("gambia")
head(gambia)
dim(gambia)
dim(unique(gambia[, c("x", "y")]))

# 
summary(kid_dhs_int_nomissgps$hbvresult)
table(kid_dhs_int_nomissgps$hbvresultlowna, useNA = "always")
test <- kid_dhs_int_nomissgps %>% filter(is.na(hbvresult))
test %>% reframe(shnprovin, hbvresult)

table(kid_dhs_int_nomissgps$sevstunt, useNA = "always")
summary(kid_dhs_int_nomissgps$hc70)
class(kid_dhs_int_nomissgps$sevstunt)

library(dplyr)
kid_dhs_int_nomissgps$sevstunt_nomiss <- as.numeric(ifelse(kid_dhs_int_nomissgps$sevstunt == "1" | kid_dhs_int_nomissgps$sevstunt == "0" , kid_dhs_int_nomissgps$sevstunt, NA_real_))
kid_dhs_int_nomissgps$sevstunt_nomiss <- kid_dhs_int_nomissgps$sevstunt_nomiss - 1
table(kid_dhs_int_nomissgps$sevstunt_nomiss, useNA = "always")
class(kid_dhs_int_nomissgps$sevstunt_nomiss)
table(kid_dhs_int_nomissgps$sevstunt, useNA = "always")

d <- group_by(kid_dhs_int_nomissgps, hv001, longnum, latnum) %>%
  summarize(
    total = n(),
    positive = sum(hbvresultlowna),
    prev = (positive / total)*100,
    tetpos = sum(shtetaindasno),
    tetcov = (tetpos / total)*100,
    sevstunted = sum(!is.na(sevstunt_nomiss)),
    sevstnpct = (sevstunted/total)*100
  )
head(d)
library(tidyverse)
d %>% filter(prev > 0) %>% summary()
d %>% filter(prev > 0) %>% 
  ggplot()+
  geom_histogram(aes(x=prev))

# alternative to dplyr - using practice data
total <- aggregate(
  gambia$pos,
  by = list(gambia$x, gambia$y),
  FUN = length
)
positive <- aggregate(
  gambia$pos,
  by = list(gambia$x, gambia$y),
  FUN = sum
)
prev <- positive$x / total$x

d <- data.frame(
  x = total$Group.1,
  y = total$Group.2,
  total = total$x,
  positive = positive$x,
  prev = prev
)

library(sp)
library(rgdal)
d$longnum <- as.numeric(d$longnum)
d$latnum <- as.numeric(d$latnum)

sps <- SpatialPoints(d[, c("longnum", "latnum")],
                     proj4string = CRS("+proj=utm +zone=28")
)
spst <- spTransform(sps, CRS("+proj=longlat +datum=WGS84"))
d[, c("long", "lat")] <- coordinates(spst)
head(d)

# library(sf)
# sf package not compatible with leaflet
# d = st_as_sf(d[!is.na(d$latnum) &!is.na(d$longnum),], coords = c("longnum", "latnum"), crs = 4326) 
# head(d)
# d = st_set_crs(d, NA)

library(leaflet)
library(viridis)

summary(d$tetcov)
d %>% filter(is.na(prev)) %>% summary()
pal <- colorBin("viridis", bins = c(0, 0.01, 5, 10,30)) # for hbv
pal <- colorBin("viridis", bins = c(0, 25, 50, 75, 100)) # for tetanus coverage

# use for prev (hbv) or tetanus vacc coverage (tetcov)
leaflet(d) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~longnum, lat = ~latnum, color = ~ pal(tetcov)) %>%
  addLegend("bottomright",
            pal = pal, values = ~tetcov,
            title = "Tetanus vaccine (%)",
            #labels = c("0",">0-5",">5-10",">10")
  ) %>%
  addScaleBar(position = c("bottomleft"))

summary(d$sevstnpct)
# use for stunting std
leaflet(d) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~longnum, lat = ~latnum, color = ~ pal(sevstnpct)) %>%
  addLegend("bottomright",
            pal = pal, values = ~sevstnpct,
            title = "Severe stunting (%)",
            #labels = c("0",">0-5",">5-10",">10")
  ) %>%
  addScaleBar(position = c("bottomleft"))

# get altitude data for Gambia from raster package
library(raster)
r <- getData(name = "alt", country = "COD", mask = TRUE) #COD is drcongo

pal <- colorNumeric("viridis", values(r),
                    na.color = "transparent"
)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
            pal = pal, values = values(r),
            title = "Altitude"
  ) %>%
  addScaleBar(position = c("bottomleft"))

# add alt values for the gps locations of our data, and add to our data frame to use as covariate in our model
d$alt <- raster::extract(r, d[, c("long", "lat")])
head(d)

# Model specification
# objective: predict malaria prevalence in The Gambia
# approach: use stochastic partial differential equations to account for spatial autocorrelation

# MODEL:
# conditional on true prevalence P(x_i) at location x_i, i=1,...,n, the number of positive results Y_i out of N_i people sampled follows a binomial distribution
# where the log odds of the true prevalence is given by beta_0 + beta_1*altitude + S(x_i). S(x_i) is a spatial random effect that follows a zero-mean Gaussian process with Matern covariance (long equation - see website)

# more on the Matern covariance: the spatial random effect term has a distribution since it is estimated. We use a Gaussian (normal) distribution, with zero mean. we must have a variance term for this distribution and the Matern covariance is used
# The specification of the Matern covariance (fancy math) is that the covariance between two measurements is a function of the distance between the two points from which they are taken. This is useful in spatial stats, since things closer together in space are more similar than those farther apart.

# we create a mesh to define distances for the matern covariance between points in our data, and use INLA for modeling

library(INLA)
coo <- cbind(d$longnum, d$latnum)
head(d)
# mesh options from past runs
#m1 <- INLA::inla.mesh.2d(coords, max.edge = c(0.45, 1), cutoff = 0.2)
#mesh <- inla.mesh.2d(loc, boundary = list(bnd1, bnd2), max.edge = c(0.05, 0.2), cutoff = 0.005)
#mesha<-inla.mesh.2d(locations,  max.edge = c(10, 20))   #edge values are in units of coordinates, these are decimal degrees
#meshb<-inla.mesh.2d(locations,  max.edge = c(2, 10))
#meshc<-inla.mesh.2d(locations,  max.edge = c(0.5, 2)) # smaller mesh, more precision but more computation


mesh <- inla.mesh.2d(
  loc = coo, max.edge = c(0.5, 2),
  cutoff = 0.01
)
mesh$n
plot(mesh)
points(coo, col = "red")

# now use inla.spde2.matern() to build the stochastic partial diffeq model
spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)
# constr = T to impose a intergrate-to-zero constraint
# alpha is a parameter related to the smoothness parameter of the process (alpha = nu + d/2), where nu is the smoothness parameter and d is distance
# in this case, we set nu=1 and d=2, so alpha = 1 + 2/2 = 2

# make index set for stochastic partial diffeq model
indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)

# now we make a projection matrix, A
# this projects the spatially continuous Gaussian random field from the observations to the nodes of the mesh. 
# inputs are the mesh and the coordinates
A <- inla.spde.make.A(mesh = mesh, loc = coo)

# prediction
# specific locations where we wish to predict
# set prediction locations to the locations of the raster of the covariate altitude
# coordinates from raster can be obtained from rasterToPoints(), which will return a matrix of coordinates and values without NA
dp <- rasterToPoints(r)
dim(dp)

# 12,964 is a lot of points - we can reduce the dimensions so that this runs faster
ra <- aggregate(r, fact = 5, fun = mean) # fact - aggregate in this case 5 cells in each direction and use the mean function for which value to take

dp <- rasterToPoints(ra)
dim(dp)
coop <- dp[, c("x", "y")]
# now we make a new projection matrix using the reduced dimensions to receive the predictions
Ap <- inla.spde.make.A(mesh = mesh, loc = coop)

# now we make the stacks for estimation and prediction
# stack for estimation stk.e
stk.e <- inla.stack(
  tag = "est",
  data = list(y = d$positive, numtrials = d$total),
  A = list(1, A),
  effects = list(b0 = 1, s = indexs)
)

# stack for prediction stk.p
stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA, numtrials = NA),
  A = list(1, A), # with raster data was Ap
  effects = list(data.frame(b0 = 1),
                 s = indexs
  )
)


# stk.full has stk.e and stk.p
stk.full <- inla.stack(stk.e, stk.p)

# specify the formula for the model
formula <- y ~ 0 + b0 + altitude + f(s, model = spde)

# now we call the inla() function
# use default priors 
# control.predictor(compute = T) is to compute the posteriors of the predictions; link=1 to compute fitted values with the same link function as the family specified in the model (in this case binomial)
res <- inla(formula,
            family = "binomial", Ntrials = numtrials,
            control.family = list(link = "logit"),
            data = inla.stack.data(stk.full),
            control.predictor = list(
              compute = TRUE, link = 1,
              A = inla.stack.A(stk.full)
            ),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, return.marginals.predictor=TRUE))
summary(res)

# now we map malaria prev predictions
# mean prev and lower/upper 95% credible intervals are in res$summary.fitted.values
# rows of res$summary.fitted.values that correspond to prediction locations can be obtained by selecting the indices of the stack stk.full that are tagged with tag = "pred". We can obtain these indices by using inla.stack.index() passing stk.full and tag = "pred".


index <- inla.stack.index(stack = stk.full, tag = "pred")$data
prev_mean <- res$summary.fitted.values[index, "mean"]
prev_ll <- res$summary.fitted.values[index, "0.025quant"]
prev_ul <- res$summary.fitted.values[index, "0.975quant"]

pal <- colorNumeric("viridis", c(0, 1), na.color = "transparent")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(
    lng = coop[, 1], lat = coop[, 2],
    color = pal(prev_mean)
  ) %>%
  addLegend("bottomright",
            pal = pal, values = prev_mean,
            title = "Prev."
  ) %>%
  addScaleBar(position = c("bottomleft"))

# this plots points as circles but we can also rasterize
r_prev_mean <- rasterize(
  x = coop, y = ra, field = prev_mean,
  fun = mean
)
pal <- colorNumeric("viridis", c(0, 1), na.color = "transparent")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_prev_mean, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
            pal = pal,
            values = values(r_prev_mean), title = "Prev."
  ) %>%
  addScaleBar(position = c("bottomleft"))

r_prev_ll <- rasterize(
  x = coop, y = ra, field = prev_ll,
  fun = mean
)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_prev_ll, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
            pal = pal,
            values = values(r_prev_ll), title = "LL"
  ) %>%
  addScaleBar(position = c("bottomleft"))

r_prev_ul <- rasterize(
  x = coop, y = ra, field = prev_ul,
  fun = mean
)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_prev_ul, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
            pal = pal,
            values = values(r_prev_ul), title = "UL"
  ) %>%
  addScaleBar(position = c("bottomleft"))

# Exceedence probability
# calculate the prob that malaria prev will be above a certain level, that may be of interest for policy making
# 1 - inla.pmarginal(q = c, marginal = marg)

# obtain posterior marginals of predictions for each location, which are in the list res$marginals.fitted.values[index], where index is the vector of indices of the full stack (stk.full) corresponding to predictions
# we obtained these in the previous section by using inla.stack.index()
# index <- inla.stack.index(stack = stk.full, tag = "pred")$data
# head(index)

res$marginals.fitted.values[index]

marg <- res$marginals.fitted.values[index][[1]]
head(marg)
1 - inla.pmarginal(q = 0.20, marginal = marg)
# res$marginals.fitted.values
excprob <- sapply(res$marginals.fitted.values[index],
                  FUN = function(marg){1-inla.pmarginal(q = 0.20, marginal = marg)})

head(excprob)
# not working 

# rasterize - when it works
r_excprob <- rasterize(
  x = coop, y = ra, field = excprob,
  fun = mean
)
pal <- colorNumeric("viridis", c(0, 1), na.color = "transparent")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_excprob, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
            pal = pal,
            values = values(r_excprob), title = "P(p>0.2)"
  ) %>%
  addScaleBar(position = c("bottomleft"))






