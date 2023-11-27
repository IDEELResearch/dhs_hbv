# INLA practice because figuring out the A matrix and mesh and stack are proving challenging

# starting with the tutorial: https://ourcodingclub.github.io/tutorials/inla/

if(!require(ggregplot)) devtools::install_github("gfalbery/ggregplot") # Installing Greg's package for plotting functions!

library(INLA); library(ggplot2); library(ggregplot)
library(tidyverse)
library(RColorBrewer)

# greg data
Root <-"/Users/camillem/Documents/GitHub/dhs_hbv" # This should be the path to your working directory

# my data
# KR merge kid_hbv_kr
## in prog: dat <- kid_hbv_kr_dis %>% dplyr::select("dbsbarcode","hv001", "cluster_hh","longnum", "latnum", "shtetaindasno", "hv104", "agenum","hv270",  "hv025", 
   ##                                     "shnprovin", "provgrp_kin","hv105",  "hbvresultlowna","totalkidpos_f","pfldh_kids", "stunt", "wasting", "underweight", )
table(kid_hbv_kr_dis$h3, useNA = "always")
table(kid_hbv_kr_dis$h5, useNA = "always")
table(kid_hbv_kr_dis$h7, kid_hbv_kr_dis$hbvresultlowna, useNA = "always")


dat <- kid_dhs_int_nomissgps %>% dplyr::select("dbsbarcode","hv001", "cluster_hh","longnum", "latnum", "shtetaindasno", "hv104", 
                                        "agenum","hv270",  "hv025", "hv026","shnprovin", "provgrp_kin","hv105",  "hbvresultlowna","totalkidpos_f","pfldh_kids")
# with KR vars
dat <- kid_hbv_kr_dis %>% dplyr::select("dbsbarcode","hv001", "cluster_hh","longnum", "latnum", "shtetaindasno", "hv104", 
                                        "agenum","hv270",  "hv025", "hv026","shnprovin", "provgrp_kin","hv105",  "hbvresultlowna", "hbvresultlowpos","totalkidpos_f","pfldh_kids",
                                        "dpt_count", "dpt_doses", "injec", "wast_mod", "stunt_mod", "beat")
dat_all <- kid_hbv_kr_dis %>% dplyr::select("dbsbarcode","hv001", "cluster_hh","longnum", "latnum", "shtetaindasno", "hv104", 
                                               "agenum","hv270",  "hv025", "hv026","shnprovin", "provgrp_kin","hv105",  "hbvresultlowna","hbvresultlowpos","totalkidpos_f","pfldh_kids")

dat <- dat %>% mutate(sex = case_when(
                  hv104==1 ~ "Male",
                  hv104==2 ~ "Female"))
dat_all <- dat_all %>% mutate(sex = case_when(
  hv104==1 ~ "Male",
  hv104==2 ~ "Female"))

# make sure data are factors                      
dat$age_f <- as.factor(dat$hv105)
dat$tetanusab <- as.factor(dat$shtetaindasno)
dat$wealth_f <- as.factor(dat$hv270)
dat$rural_f <- as.factor(dat$hv025)

dat_all$age_f <- as.factor(dat_all$hv105)
dat_all$tetanusab <- as.factor(dat_all$shtetaindasno)
dat_all$wealth_f <- as.factor(dat_all$hv270)
dat_all$rural_f <- as.factor(dat_all$hv025)

#phen <- c("Grid", "ID", "Easting", "Northing") # Base columns with spatial information we'll need
phen <- c("shnprovin", "hv001", "longnum", "latnum") # Base columns with spatial information we'll need

#resp <- "Parasite.count" # Response variable
resp <- "hbvresultlowna" # Response variable - when 46 low vol are neg
resp <- "hbvresultlowpos" # Response variable - when 46 are positive

covar <- c("tetanusab", # tetatnus Ab detected 0/1 conservative in that no result is in not vacc
           "sex", # Sex
           "age_f", # age in years as factor
           "wealth_f", # wealth in 5 groups, as factor
           "shnprovin", # province with numbering so that Kinshasa (=1) is referent
           "rural_f") # rural/urban

TestHBV <- na.omit(dat[, c(phen, resp, covar)]) # Getting rid of NA's, picking adults
TestHBV <- na.omit(dat) # Getting rid of NA's, picking adults
TestHBV2 <- na.omit(dat_all) # Getting rid of NA's, picking adults
# We are using the [] to subset and only extract specific columns
nrow(dat) - nrow(TestHBV)
nrow(dat_all) - nrow(TestHBV2)

# set up custom theme
THEME <- theme(axis.text.x = element_text(size = 12,colour = "black"),
axis.text.y = element_text(size = 12, colour = "black"),
axis.title.x = element_text(vjust = -0.35),
axis.title.y = element_text(vjust = 1.2)) + theme_bw()

# Model fitting
# First without random effects ####

# Specify the formula
f0.1 <- as.formula(paste0(resp, " ~ ", # Response first
                          paste(covar, collapse = " + ") # Collapse the vector of covariates
))

# Run the model
IM0.1  <- inla(hbvresultlowna ~ tetanusab + sex + age_f + wealth_f + shnprovin + rural_f,
               family = "binomial", # Specify the family. Can be a wide range (see r-inla.org).
               data = TestHBV,
               control.compute = list(dic = TRUE)) # Specify the data - the one without NAs
# Run the model without province
IM0.1_noprv  <- inla(hbvresultlowna ~ tetanusab + sex + age_f + wealth_f + rural_f,
               family = "binomial", # Specify the family. Can be a wide range (see r-inla.org).
               data = TestHBV,
               control.compute = list(dic = TRUE)) # Specify the data - the one without NAs

# Run the model # (This is the same thing)
IM0.1  <- inla(f0.1, 
               family = "binomial", # Specify the family. Can be a wide range (see r-inla.org).
               data = TestHBV,
               control.compute = list(dic = TRUE)) # Specify the data

# Then with an ID random effect ####
f0.2 <- as.formula(paste0(resp, " ~ ", 
                          paste(covar, collapse = " + "), 
                          " +  f(hv001, model = 'iid')")) # This is how you include  a typical random effect.

# greg had the below - the random effects ID is the individual bc he had data colllected on the same individuals over time, whereas I have clustering by DHS cluster
# " +  f(dbsbarcode, model = 'iid')")) # This is how you include  a typical random effect.

IM0.2  <- inla(f0.2, 
               family = "binomial",
               data = TestHBV,
               control.compute = list(dic = TRUE)) 
IM0.2_noprv  <- inla(hbvresultlowna ~ tetanusab + sex + age_f + wealth_f + rural_f  +  f(hv001, model = 'iid'),
                     family = "binomial", # Specify the family. Can be a wide range (see r-inla.org).
                     data = TestHBV,
                     control.compute = list(dic = TRUE)) # Specify the data - the one without NAs

summary(IM0.1)
summary(IM0.2)
summary(IM0.1_noprv)
summary(IM0.2_noprv)
# DIC better in model with province, similar between random effects and fixed effects model

# requires MCMCglmm and MASS : Efxplot(list(IM0.1, IM0.2))
plotdat <- bind_rows(
  as_tibble(summary(IM0.1)[["fixed"]], rownames = "var") |> mutate(model = "IM0.1"),
  as_tibble(summary(IM0.2)[["fixed"]], rownames = "var") |> mutate(model = "IM0.2")
)
ggplot(data = plotdat) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(aes(y = var, x = mean, color = model), position = position_dodge(width = 0.5)) +
  
  geom_linerange(aes(y = var, x = mean, xmin = `0.025quant`, xmax = `0.975quant`,
                     color = model), position = position_dodge(width = 0.5)) +
  #coord_cartesian(xlim = c(-5, 5)) +
  theme_bw()

# greg albery wrote a function to perform model selection
HostModelSel <- INLAModelSel(resp, covar, "ID", "iid", "nbinomial", TestHosts)
# conclusion is to drop body condition and food supplementation, and keep: treatment, sex, month
Finalcovar <- HostModelSel$Removed[[length(HostModelSel$Removed)]]
Finalcovar  <- c("tetanusab", # tetanus
           "sex", # Sex
           "age_f",
           "wealth_f",
           "rural_f",
           "shnprovin") # urbal/rural
Finalcovar  <- c("sex")

# let's try final cov without provin  shnprovin
f1 <- as.formula(paste0(resp, " ~ ", 
                        paste(Finalcovar, collapse = " + "), 
                        "+ f(hv001, model = 'iid')")) 

IM1 <- inla(f1,
            family = "binomial",
            data = TestHBV,
            control.compute = list(dic = TRUE)) 

summary(IM1)

# try one covariate at a time---------------------------
kid_hbv_kr_dis$tet
table(TestHBV$hbvresultlowna, useNA = "always") # have dropped the low vol - should add back in
view(TestHBV)
nrow(TestHBV)

# varun says identity link for prev diffs is in family=logistic and this is an inla misnomer https://inla.r-inla-download.org/r-inla.org/doc/likelihood/logistic.pdf
IM0.tet <- inla(hbvresultlowna ~ 1 + shtetaindasyes + f(hv001, model = 'iid'),
            family = "logistic",
            Ntrials = nrow(kid_hbv_kr_dis), 
            data = kid_hbv_kr_dis,
            control.family = list(link="identity"),
            control.compute = list(dic = TRUE) ,  control.predictor = list(compute=TRUE),
            control.fixed = list(prec = 1e-09)) 
# values still not what svyglm() gives
IM0.tet$dic$dic
summary(IM0.tet)
IM0.tet$summary.fixed
plot(IM0.tet)
min(IM0.tet$marginals.fixed$tetanusab1)
plot(IM0.tet$marginals.fixed$tetanusab0, type = "l", xlab = "beta",
     ylab = "density", xlim = c(-15, -12 ))
plot(IM0.tet$marginals.fixed$tetanusab1, type = "l", xlab = "beta",
     ylab = "density", xlim = c(-15, -12 ))
## need to figure out what the coeffs mean (-13.7 and -12.8 for tet+/tet-)

# from Cameletti text p142-3 - exponentiate coefficients for easier interpretation, can exponentiate the beta1 marginals using inla.tmarginal() then inla.zmarginal()
# or if solely interested in posterior mean, can using inla.emarginal()
inla.emarginal(exp, IM0.tet$marginals.fixed$tetanusab1)



#IM0.2_noprv  <- inla(hbvresultlowna ~ tetanusab + sex + age_f + wealth_f + rural_f  +  f(hv001, model = 'iid'),
IM0.sex <- inla(hbvresultlowna ~ sex + f(hv001, model = 'iid'),
                family = "binomial",
                data = TestHBV,
                control.compute = list(dic = TRUE)) 
IM0.sex <- inla(hbvresultlowna ~ -1 + sex + f(hv001, model = 'iid'),
                family = "binomial",
                Ntrials = nrow(TestHBV), 
                data = TestHBV,
                control.compute = list(dic = TRUE) ,  control.predictor = list(compute=TRUE))
                #control.fixed = list(prec = 1e-09)) 
summary(IM0.sex)
IM0.sex$dic$dic
IM0.sex$summary.linear.predictor
IM0.sex$summary.fitted.values

IM0.age <- inla(hbvresultlowna ~ age_f + f(hv001, model = 'iid'),
                family = "binomial",
                data = TestHBV,
                control.compute = list(dic = TRUE)) 
IM0.age <- inla(hbvresultlowna ~  age_f + f(hv001, model = 'iid'),
                family = "binomial",
                Ntrials = nrow(TestHBV), 
                data = TestHBV,
                control.compute = list(dic = TRUE) ,  control.predictor = list(compute=TRUE))
summary(IM0.age)
IM0.age$dic$dic

IM0.wea <- inla(hbvresultlowna ~ wealth_f + f(hv001, model = 'iid'),
                family = "binomial",
                data = TestHBV,
                control.compute = list(dic = TRUE)) 
summary(IM0.wea)
IM0.wea$dic$dic
IM0.wea <- inla(hbvresultlowna ~  wealth_f + f(hv001, model = 'iid'),
                family = "binomial",
                Ntrials = nrow(TestHBV), 
                data = TestHBV,
                control.compute = list(dic = TRUE) ,  control.predictor = list(compute=TRUE))

IM0.rur <- inla(hbvresultlowna ~ rural_f + f(hv001, model = 'iid'),
                family = "binomial",
                data = TestHBV,
                control.compute = list(dic = TRUE)) 
IM0.rur <- inla(hbvresultlowna ~  rural_f + f(hv001, model = 'iid'),
                family = "binomial",
                Ntrials = nrow(TestHBV), 
                data = TestHBV,
                control.compute = list(dic = TRUE) ,  control.predictor = list(compute=TRUE))
summary(IM0.rur)
IM0.rur$dic$dic

IM0.prov <- inla(hbvresultlowna ~ shnprovin + f(hv001, model = 'iid'),
                family = "binomial",
                data = TestHBV,
                control.compute = list(dic = TRUE)) 
IM0.prov <- inla(hbvresultlowna ~  shnprovin + f(hv001, model = 'iid'),
                family = "binomial",
                Ntrials = nrow(TestHBV), 
                data = TestHBV,
                control.compute = list(dic = TRUE) ,  control.predictor = list(compute=TRUE))
summary(IM0.prov)
IM0.prov$dic$dic
# province the strongest predictor (lowest DIC)

# add: pf malaria, 



# Add spatial intercept-------------
# now to the complex part where we add the mesh to account for spatial variation
TestHBV$longnum <- as.numeric(TestHBV$longnum)
TestHBV$latnum <- as.numeric(TestHBV$latnum)
Locations = cbind(TestHBV$longnum, TestHBV$latnum) # using the sampling locations 


MeshA <- inla.mesh.2d(jitter(Locations), max.edge = c(20, 40))
MeshB <- inla.mesh.2d(Locations, max.edge = c(20, 40))
MeshC <- inla.mesh.2d(Locations, max.edge = c(10, 20))

Mesh <- MeshB

plot(MeshA)
plot(MeshB)
plot(MeshC)

points(Locations, col = "red", pch = 2)

# ---from Cedar
meshc<-inla.mesh.2d(Locations,  max.edge = c(50, 200))
Mesh <- meshc

plot(meshc,asp=1)
points(Locations, col = "red", pch = 2)
plot(DRC, add=T)

##2. Spatial effects A matrix, matern covariance, and index files
loc = cbind(locations_km$x,locations_km$y)

mat_a_id<-inla.spde.make.A(mesh=meshc, loc = loc) #A matrix for global spatial effects

mat_spde = inla.spde2.pcmatern(mesh = meshc, prior.range = c(600, 0.5), prior.sigma = c(.5, .5)) # Making SPDE

mat_w_id <- inla.spde.make.index('w', n.spde = mat_spde$n.spde) # making the weights for spatial locations
# ---

# Making the A matrix 

# HostsA <- inla.spde.make.A(Mesh, loc = Locations) # Making A matrix
hbv_A <- inla.spde.make.A(Mesh, loc = Locations) # Making A matrix
# Hosts.spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)) # Making SPDE - greg used prior range of 10 to 0.5, changing to cedar's 600
mat.hbv.spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(600, 0.5), prior.sigma = c(.5, .5)) # Making SPDE
mat.hbv.spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)) # Making SPDE
w.hbv <- inla.spde.make.index('w', n.spde = mat.hbv.spde$n.spde) # making the w

# look into adding spatial slope - test by cov of interest
##a) SPDE and weights for spatial random intercept
mat_a_id<-inla.spde.make.A(mesh=meshc, loc = cbind(locations$longnum,locations$latnum))
mat_s_w <- inla.spde.make.index('w', n.spde = mat_spde$n.spde) # making the weights

##b) SPDE and weights for spatiallly varying slope terms
mat_a_temp<-inla.spde.make.A(mesh=meshc, loc = cbind(locations$longnum,locations$latnum)) # cedar had , weights = gepr_full$temp_1mo_lag_s
mat_s_temp<-inla.spde.make.index('temp_w', n.spde = mat_spde$n.spde) #The 'n.spde' stays the same for all spatial effects that use the same covariance structure
#You can create different covariance structures for different spatial effects (i.e. if you want to change the range prior for a specific covariate)


# Making the model matrix #### 

X0 <- model.matrix(as.formula(paste0(" ~ -1 + ", paste(Finalcovar, collapse = " + "))), data = TestHBV) # make the model matrix using the final model selection formula without a response variable.

X <- as.data.frame(X0[,-which(colnames(X0)%in%c("tetanusab0"))]) # convert to a data frame. Eliminate the base level of the first categorical variable if applicable (you will manually specify an intercept below) 

head(X0)

# Making the stack ####

N <- nrow(TestHBV)

StackHost <- inla.stack(
  data = list(y = TestHBV[,resp]), # specify the response variable
  A = list(1, 1, 1, hbv_A), # Vector of Multiplication factors for random and fixed effects              
  effects = list(
    Intercept = rep(1, N), # specify the manual intercept!
    X = X, # attach the model matrix
    ID = TestHBV$hv001, # insert vectors of any random effects
    w = w.hbv)) # attach the w 

StackHost_nofixcov <- inla.stack(
  data = list(y = TestHBV[,resp]), # specify the response variable
  A = list(1, 1, 1, hbv_A), # Vector of Multiplication factors for random and fixed effects              
  effects = list(
    Intercept = rep(1, N), # specify the manual intercept!
#    X = X, # attach the model matrix
    ID = TestHBV$hv001, # insert vectors of any random effects
    w = w.hbv)) # attach the w 

# now let's run
f1 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + ")))
f2 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid')"))
f3 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid') + f(w, model = mat.hbv.spde)"))
#Spatially varying slope model (if including, need to remove covariate from fixed effects)
f4 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid') + f(w, model = mat.hbv.spde) + f(temp_w, model=mat_spde)"))


IM1 <- inla(f1, # Base model (no random effects)
            family = "binomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost))
)

IM2 <- inla(f2, # f1 + Year and ID random effects
            family = "binomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost))
)

IM3 <- inla(f3, # f2 + SPDE random effect 
            family = "binomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost)),
            control.inla=list(control.vb=list(emergency=31.00))
            )

summary(IM3)

SpatialHostList <- list(IM1, IM2, IM3)
# INLARange(list(IM3), maxrange = Maxrange)
sapply(SpatialHostList, function(f) f$dic$dic)
INLADICFig(SpatialHostList, ModelNames = c("Base", "IID", "SPDE"))

ggField(IM3, Mesh, Groups = 1) +
  scale_fill_brewer(palette = "Blues") 
summary(IM1)$dic$dic
summary(IM2)$dic$dic
summary(IM3)$dic$dic

library(fields)
# cedar's plotting
local.plot.field = function(field, meshc, xlim=c(12, 32), ylim=c(-14, 6), res=500, ...){
  stopifnot(length(field) == meshc$n)
  # - error when using the wrong mesh
  proj = inla.mesh.projector(meshc, xlim = xlim, 
                             ylim = ylim, dims=c(res, res))
  # - Can project from the mesh onto a 300x300 plotting grid 
  field.proj = inla.mesh.project(proj, field)
  # - Do the projection
  image.plot(list(x = proj$x, y=proj$y, z = field.proj), 
             xlim = xlim, ylim = ylim, ...)  
}
m = IM3 #Specify results to plot
m$summary.fixed[]

m$summary
#spatial random intercept
local.plot.field(m$summary.ran$w$mean, meshc ,axes=F)
plot(DRC, add=T)
box()

#Variance of spatial random intercept
local.plot.field(m$summary.ran$w$sd, meshc ,axes=F)
plot(DRC, add=T)
box()




# always use a single-dimension colour palette if you can! It's just easier on the eyes, 
# better for colourblind people, makes sense in black and white, etc.

# ignore the Groups part of the function for now. That'll come later.
# function takes (a list of) models and plots the decay of spatial autocorrelation across a user-defined range

# let's try it on our model ###

# Define the maximum range as something reasonable: the study area is 80 eastings wide, so lets go for:

Maxrange = 40

INLARange(list(IM3), maxrange = Maxrange)
sapply(SpatialHostList, function(f) f$dic$dic)
INLADICFig(SpatialHostList, ModelNames = c("Base", "IID", "SPDE"))

# lets try with province
Finalcovar  <- c("tetanusab", # tetanus
                 "sex", # Sex
                 "age_f",
                 "wealth_f", "hv025"
                 ) # "shnprovin"province with number so kin is referent
# Making the model matrix #### 

X0 <- model.matrix(as.formula(paste0(" ~ -1 + ", paste(Finalcovar, collapse = " + "))), data = TestHBV) # make the model matrix using the final model selection formula without a response variable.
head(X0)
X <- as.data.frame(X0[,-which(colnames(X0)%in%c("tetanusab0"))]) # convert to a data frame. Eliminate the base level of the first categorical variable if applicable (you will manually specify an intercept below) 
head(X)

N <- nrow(TestHBV)

# leave HostsA, Hosts.spde, w.Host as above, but check with hill/cedar's parameters

StackHost <- inla.stack(
  data = list(y = TestHBV[,resp]), # specify the response variable
  
  A = list(1, 1, 1, hbv_A), # Vector of Multiplication factors for random and fixed effects              
  
  effects = list(
    
    Intercept = rep(1, N), # specify the manual intercept!
    
    X = X, # attach the model matrix
    
    ID = TestHBV$hv001, # insert vectors of any random effects
    
    w = w.hbv)) # attach the w 

inla.setOption(num.threads = 8) 
# same functions as above - see example where greg adds random effect for group ()
f1 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + ")))
f2 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid')"))
f3 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid') + f(w, model = mat.hbv.spde)"))
# from greg's example, the following f5 was the best fit - had random effect for the individual (given temporal data) and for group (month)
f5 = as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), 
                       "+ f(ID, model = 'iid') +  f(w, model = mat.hbv.spde, 
                       group = w.group, # This bit is new! 
                       control.group = list(model='exchangeable'))"))
# see rest of his code for the mesh, stack, etc. for this extra random effect

#inla.setOption(num.threads = 8) 

IM1 <- inla(f1, # Base model (no random effects)
            family = "binomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost))
)

IM2 <- inla(f2, # f1 +  cluster random effects
            family = "binomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost))
)

IM3 <- inla(f3, # f2 + SPDE random effect 
            family = "binomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost))
)
SpatialHostList <- list(IM1, IM2, IM3)
ggField(IM3, Mesh, Groups = 1) 
summary(IM3)

 #+ scale_fill_brewer(palette = "Blues") 
summary(IM1)$dic$dic
summary(IM2)$dic$dic
summary(IM3)$dic$dic

# Evaluate models for boys vs girls----------------------------------------
# from above, TestHBV <- na.omit(dat[, c(phen, resp, covar)]) # Getting rid of NA's, picking adults
# need to investigate what is missing 

table(TestHBV$sex)

dat_g <- TestHBV %>% filter(sex=="Female")
dat_b <- TestHBV %>% filter(sex=="Male")


# remove sex from list of final covariates
Finalcovar  <- c("tetanusab", # tetanus
                 "age_f",
                 "wealth_f",
                 "hv025") # urbal/rural
 
# now to the complex part where we add the mesh to account for spatial variation
Locations = cbind(dat_g$longnum, dat_g$latnum) # using the sampling locations 
Locations = cbind(dat_b$longnum, dat_b$latnum) # using the sampling locations 
nrow(Locations)
# options for mesh
meshc<-inla.mesh.2d(Locations,  max.edge = c(50, 200))
MeshB <- inla.mesh.2d(Locations, max.edge = c(20, 40))
Mesh <- MeshB
#Mesh <- meshc
#A_pred <- inla.spde.make.A(mesh = MeshB)

# Making the A matrix - this is where i ran into trouble with hill/cedar's code

hbv_A <- inla.spde.make.A(Mesh, loc = Locations) # Making A matrix
Hosts.spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)) # Making SPDE - greg used prior range of 10 to 0.5, changing to cedar's 600
# Hosts.spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(600, 0.5), prior.sigma = c(.5, .5)) # Making SPDE
w.Host <- inla.spde.make.index('w', n.spde = Hosts.spde$n.spde) # making the w


# Making the model matrix #### 

X0 <- model.matrix(as.formula(paste0(" ~ -1 + ", paste(Finalcovar, collapse = " + "))), data = dat_g) # make the model matrix using the final model selection formula without a response variable.

X <- as.data.frame(X0[,-which(colnames(X0)%in%c("tetanusab0"))]) # convert to a data frame. Eliminate the base level of the first categorical variable if applicable (you will manually specify an intercept below) 

head(X)

# Making the stack ####

N <- nrow(dat_g)

StackHost <- inla.stack(
  data = list(y = dat_g[,resp]), # specify the response variable
  A = list(1, 1, 1, hbv_A), # Vector of Multiplication factors for random and fixed effects              
  
  effects = list(
    Intercept = rep(1, N), # specify the manual intercept!
    X = X, # attach the model matrix
    ID = dat_g$hv001, # insert vectors of any random effects
    w = w.hbv) # attach the w 
)




# now let's run
f1 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + ")))
f2 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid')"))
f3 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid') + f(w, model = mat.hbv.spde)"))


IM1 <- inla(f1, # Base model (no random effects)
            family = "binomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost))
)

IM2 <- inla(f2, # f1 + Year and ID random effects
            family = "binomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost))
)

IM3 <- inla(f3, # f2 + SPDE random effect 
            family = "binomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost)#, #
            #control.inla(list(control.vb=list(emergency)))
            )
)
# for girls to redo axes
IM3_g <- inla(f3, # f2 + SPDE random effect 
            family = "binomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost)#, #
                                     #control.inla(list(control.vb=list(emergency)))
            )
)
summary(IM3)

SpatialHostList <- list(IM1, IM2, IM3)
# INLARange(list(IM3), maxrange = Maxrange)
sapply(SpatialHostList, function(f) f$dic$dic)
INLADICFig(SpatialHostList, ModelNames = c("Base", "IID", "SPDE"))

#spatial random intercept - see function above for local.plot.field()
m = IM3_g #Specify results to plot
# m = IM3 # last run for boys
m$summary.fixed[]

local.plot.field = function(field, MeshB, xlim=c(12, 32), ylim=c(-14, 6), res=500, ...){
  stopifnot(length(field) == MeshB$n)
  # - error when using the wrong mesh
  proj = inla.mesh.projector(MeshB, xlim = xlim, 
                             ylim = ylim, dims=c(res, res))
  # - Can project from the mesh onto a 300x300 plotting grid 
  field.proj = inla.mesh.project(proj, field)
  # - Do the projection
  image.plot(list(x = proj$x, y=proj$y, z = field.proj), 
             xlim = xlim, ylim = ylim, zlim = c(0.6,3.7))  # CHECK ZLIM FOR EACH RUN
}

local.plot.field(m$summary.ran$w$mean, MeshB ,axes=F)
plot(DRC, add=T)
# points(Locations, col = "black", pch = 4) # ADD DHS CLUSTER LOCATIONS
box()

#Variance of spatial random intercept
local.plot.field(m$summary.ran$w$sd, MeshB ,axes=F)
plot(DRC, add=T)
# points(Locations, col = "black", pch = 4) # ADD DHS CLUSTER LOCATIONS
box()

# plain map with DHS clusters-----------
dhsclusters = st_as_sf(kids_clust[!is.na(kids_clust$latnum) &!is.na(kids_clust$longnum),], coords = c("longnum", "latnum"), crs = 4326) 
# GADM boundaries from: https://gadm.org/download_country_v3.html
admin0 <- readRDS('/Users/camillem/Documents/GitHub/animalaria/admin0.rds') %>%          # GADM admin0 boundaries
  st_transform(4326) %>% # set at ESPG 4326
  filter(grepl('Congo|Rwanda|Tanzania|Burundi|African Republic|Angola|Zambia|Uganda|Sudan|Gabon|Cameroon|Equatorial Guinea', Country)) 
st_crs(admin0) # view CRS
DRC_sf <- admin0 %>% filter(Country=='Democratic Republic of the Congo') # DRC

ggplot() + 
  geom_sf(data=admin0, fill="cornsilk2", color="cornsilk3") +
  geom_sf(data=DRC_sf, fill="cornsilk") +
  geom_sf(data=DRC_sf, fill=NA, color="tan4", size=0.75) + 
  geom_sf(data=dhsclusters, alpha=0.8) + 
  #labs(color='HBV prevalence \nin children < 5') + 
  theme_bw(base_size=14) + 
  #scale_color_distiller(palette = 'Spectral') + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  #ggtitle("Children ≤ 5")+ # for adding with adult figure
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))

# go through https://ourcodingclub.github.io/tutorials/spatial-modelling-inla/#modelpredictions for alt way to plot residuals




  