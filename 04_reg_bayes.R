# Using code from models - associations between livestock and malaria, bayesian approach
# https://becarioprecario.bitbucket.io/inla-gitbook/index.html
source("./02_code/data_and_libraries.R")


library(INLA)
library(inlabru)
library(INLAspacetime)

# load data
dhs_clean_spatial <- readRDS(paste0(path, "01_data/dhs_clean_spatial.rds"))


# categorize animal ownership variables into binaries
categorize <- function(x) case_when(x == 0 ~ 0, x > 0 ~ 1, TRUE ~ x)

dhs_model <- dhs_clean_spatial |>
  mutate(across(32:44, categorize, .names = "{col}_f"))



# resource for INLA: https://sites.stat.washington.edu/peter/591/INLA.html
# resource for INLA: https://ourcodingclub.github.io/tutorials/inla/

# start with one country
dat <- dhs_model |> ungroup() |>
  mutate(country = str_sub(SurveyId, start = 1L, end = 2L),
         year = str_sub(SurveyId, start = 3L, end = 6L),
         livestock = dplyr::case_when(livestock == "yes" ~ 1,
                               livestock == 1 ~ 1,
                               livestock == "no" ~ 0,
                               livestock == 0 ~ 0,
                               TRUE ~ NA_real_)) |>
  filter(country == "KE") # |>  st_as_sf(coords = c("LONGNUM", "LATNUM"), crs = 4326)

table(dat$RDT, useNA = "always"); table(dat$livestock, useNA = "always")
table(dat$cluster)

# perform model selection ------------------------------------------------------
# from columbia course
formula_iid <- deaths ~ 1 + f(SIGLA, model = "iid") # SIGLA is province # function for each province that is iid
model <- inla(
  formula_iid,
  data = data,
  E = expected, # expected is variable in dataframe
  family = "poisson",
  #verbose = TRUE, 
  control.predictor = list(link = 1), # log-link
  control.compute = list(config = TRUE) # so that we can simulate draws of posterior
)

# without random effects
kid_inla_mf_2  <- INLA::inla(hbvresult ~  hv104, #+ hv025 + as.numeric(hv270)
                     family = "binomial",
                     data = kid_dhs_int,
                     control.compute = list(dic = TRUE),
                     control.predictor=list(link=1)) # link=1 from columbia course
summary(kid_inla_mf_2)

# with random effects
kid_inla_mf_rand  <- INLA::inla(hbvresult ~ hv104 + f(hv001, model = "iid"),
               family = "binomial",
               data = kid_dhs_int,
               control.compute = list(dic = TRUE),
               control.predictor=list(link=1)) # link=1 from columbia course

summary(kid_inla_mf_rand)

#also try BYM not iid model (requires making the adjacency matrix)

plotdat <- bind_rows(
  as_tibble(summary(kid_inla_mf_rand)[["fixed"]], rownames = "var") |> mutate(model = "kid_inla_mf_rand"),
  as_tibble(summary(kid_inla_mf_2)[["fixed"]], rownames = "var") |> mutate(model = "kid_inla_mf_2")
)
view(plotdat)
ggplot(data = plotdat) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(aes(y = var, x = mean, color = model), position = position_dodge(width = 0.5)) +
  geom_linerange(aes(y = var, x = mean, xmin = `0.025quant`, xmax = `0.975quant`,
                     color = model), position = position_dodge(width = 0.5)) +
  coord_cartesian(xlim = c(-5, 5)) +
  theme_bw()

# compare DIC values from each model -lower is better
summary(kid_inla_mf_2)$dic$dic
summary(kid_inla_mf_rand)$dic$dic 


# set up mesh ------------------------------------------------------------------
# create matrix of survey coordinates
coords <- as.matrix(kid_dhs_int[, c("longnum","latnum")]) # longitude, latitude
coords <- na.omit(coords)

check <- kid_dhs_int[, c("longnum","latnum")]


# create mesh with regular triangles
m1 <- INLA::inla.mesh.2d(coords, max.edge = c(0.45, 1), cutoff = 0.2)
plot(m1, asp = 1, main = "")
points(coords[, 1], coords[, 2], pch = 19, cex = 0.5, col = "red")

# evaluate mesh formation, examine sd, values should be close to 1
inla.mesh.assessment(m1, spatial.range = 3, alpha = 2)

# create mesh with non-convex hull
prdomain <- INLA::inla.nonconvex.hull(coords, -0.03, -0.05, resolution = c(100, 100))
m2 <- INLA::inla.mesh.2d(boundary = prdomain, max.edge = c(0.45, 1), cutoff = 0.2)
plot(m2, asp = 1, main = "")
points(coords[, 1], coords[, 2], pch = 19, cex = 0.5, col = "red")

inla.mesh.assessment(m2, spatial.range = 3, alpha = 2)


# make the A matrix - translates spatial locations on the mesh into vectors in the model. SPDE and weights for spatial random intercept.
Mesh <- m2
A <- INLA::inla.spde.make.A(Mesh, loc = coords) # Making A matrix
spde <-  INLA::inla.spde2.pcmatern(mesh = Mesh, prior.range = c(10, 0.5),
                                   prior.sigma = c(.5, .5)) # Making SPDE
w <- INLA::inla.spde.make.index('w', n.spde = spde$n.spde) # making the w


# combine the A matrix with the model matrix and random effects = stack
# make the model matrix using the model formula without a response variable
X0 <- model.matrix(as.formula("~ -1 + livestock + gender + residence + wealth + slept_ITN"), data = dat)

X <- as.data.frame(X0[,]) # convert to a data frame. Eliminate the base level of the first categorical variable if applicable (you will manually specify an intercept below)

head(X)

# make the stack
N <- nrow(dat)

Stack <- INLA::inla.stack(
  data = list(y = dat[, "RDT"]), # specify the response variable
  tag = "est",

  A = list(1, 1, 1, A), # vector of multiplication factors for random and fixed effects
  # this is generally a series of 1’s (for the intercept, random effects, and fixed effects), followed by the spatial A matrix which you specified earlier.
  effects = list(
    Intercept = rep(1, N), # specify the manual intercept!
    X = X, # attach the model matrix
    cluster = dat$cluster, # insert vectors of any random effects
    w = w)) # attach the weights for random effects


# run INLA models --------------------------------------------------------------
f1 <- as.formula(paste0("RDT ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + ")))
# fixed effects + random intercept
f2 <- as.formula(paste0("RDT ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(cluster, model = 'iid')"))
# fixed effects + random intercept + spatial intercept
f3 <- as.formula(paste0("RDT ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(cluster, model = 'iid') + f(w, model = spde)"))


IM1 <- inla(f1, # Base model (no random effects)
            family = "binomial",
            data = inla.stack.data(Stack),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(Stack))
)

IM2 <- inla(f2, # f1 + cluster random effects
            family = "binomial",
            data = inla.stack.data(Stack),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(Stack))
)

IM3 <- inla(f3, # f2 + SPDE random effect
            family = "binomial",
            data = inla.stack.data(Stack),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(Stack))
)

SpatialList <- list(IM1, IM2, IM3)

# plot correlation by distance
ModelList <- list(IM3)
MaxRange <- 40
Priors <- MaxRange/2
PriorProbabilities <- 0.5
Resolution <- 100
WNames <- rep("w", length(ModelList))
MeshList <- list(Mesh)

SpFi.w = 1:length(ModelList) %>% lapply(function(j){
  inla.spde2.result(inla = ModelList[[j]],
                    name = WNames[[j]],
                    spde = inla.spde2.pcmatern(mesh = MeshList[[j]],
                                               prior.range = c(Priors[[j]], PriorProbabilities[[j]]),
                                               prior.sigma = c(.5, .5)),
                    do.transfer = TRUE)
})

Kappa <- lapply(SpFi.w,  function(j)
  inla.emarginal(function(x) x,
                 j$marginals.kappa[[1]] ))

d.vec <- seq(0, MaxRange, length = Resolution)

Cor <- lapply(Kappa,function(f){
  Cor.M <- as.numeric((f * d.vec) * besselK(f * d.vec, 1))
  Cor.M[1] <- 1
  return(data.frame(d.vec,Cor.M))
})

Cor <- dplyr::bind_rows(Cor)
Cor$Model <- as.factor(rep(1:length(Kappa), each = Resolution))

ggplot(Cor, aes(d.vec, Cor.M, color = Model, lty = Model)) +
  geom_line(size = 1, alpha = 0.8) +
  coord_fixed(ratio = MaxRange) +
  labs(colour ="Model",x = "Distance", y = "Correlation") + theme_bw()

# compare DIC values
sapply(SpatialList, function(f) f$dic$dic)


plotdat <- bind_rows(
  as_tibble(summary(IM1)[["fixed"]], rownames = "var") |> mutate(model = "IM1"),
  as_tibble(summary(IM2)[["fixed"]], rownames = "var") |> mutate(model = "IM2"),
  as_tibble(summary(IM3)[["fixed"]], rownames = "var") |> mutate(model = "IM3")
)

ggplot(data = plotdat) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(aes(y = var, x = mean, color = model)) +
  geom_linerange(aes(y = var, x = mean, xmin = `0.025quant`, xmax = `0.975quant`,
                     color = model)) +
  coord_cartesian(xlim = c(-5, 5)) +
  theme_bw()


# function to plot output mean and sd
inlaplot <- function(m, colo, nam){
  ip<-ggplot()+
    gg(Mesh, color=m)+ # , mask=DRC
    # gg(DRC)+
    coord_equal()+
    scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, colo)),
                         name=nam)+
    xlab("Easting")+
    ylab("Northing")+
    theme_bw()
  return(ip)
}

inlaplot(m = IM3$summary.random$w$mean, colo="RdYlBu", nam="Spatial intercept\nmean")
inlaplot(m = IM3$summary.random$w$sd, colo="Blues", nam="Spatial intercept\nSD")


# Notes from Varun:
# INLA is serving as a regression model but with credible intervals instead of confidence intervals and with priors.
# If you set uninformative priors you will see similar results between INLA and a regular regression model.
# The reason to use INLA here instead of glm is because it makes model comparison easier (the bayesian framework takes into account uncertainty)
# INLA allows us to adjust for spatial effects (which are omitted variables). It is possible there is a variable we don't know about or can't measure which has a spatial nature (e.g. temperature).
# INLA maps allow us to adjust for variables vs. kriging which is a straw man model


