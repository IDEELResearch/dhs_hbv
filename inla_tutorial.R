# INLA practice because figuring out the A matrix and mesh and stack are proving challenging

# starting with the tutorial: https://ourcodingclub.github.io/tutorials/inla/

if(!require(ggregplot)) devtools::install_github("gfalbery/ggregplot") # Installing Greg's package for plotting functions!

library(INLA); library(ggplot2); library(ggregplot)
library(tidyverse)
library(RColorBrewer)

Root <-"/Users/camillem/Documents/GitHub/dhs_hbv" # This should be the path to your working directory

Hosts <- read.csv(paste0(Root, "/Data/HostCaptures.csv"), header = T)

head(Hosts)

substr(names(Hosts), 1, 1) <- toupper(substr(names(Hosts), 1, 1)) # Giving the host names capital letters

phen <- c("Grid", "ID", "Easting", "Northing") # Base columns with spatial information we'll need

resp <- "Parasite.count" # Response variable

covar <- c("Month", # Julian month of sampling
           "Sex", # Sex
           "Smi", # Body condition
           "Supp.corrected", # Nutrition supplementation
           "Treated") # Treatment

TestHosts <- na.omit(Hosts[, c(phen, resp, covar)]) # Getting rid of NA's, picking adults
# We are using the [] to subset and only extract specific columns

# Turning variables into factors
TestHosts$Month <- as.factor(TestHosts$Month)
TestHosts$Grid <- as.factor(TestHosts$Grid)

TestHosts$Parasite.count <- round(TestHosts$Parasite.count) # Parasite counts should be integers

table(table(TestHosts$ID)) # Enough repeat samples for a mixed model?

# set up custom theme
THEME <- theme(axis.text.x = element_text(size = 12,colour = "black"),
axis.text.y = element_text(size = 12, colour = "black"),
axis.title.x = element_text(vjust = -0.35),
axis.title.y = element_text(vjust = 1.2)) + theme_bw()

(samp_locations <- ggplot(TestHosts, aes(Easting, Northing)) + 
    geom_jitter(aes(colour = factor(Grid))) + coord_fixed() + 
    THEME + 
    labs(colour = "Grid"))
# Recall that putting your entire ggplot code in brackets () creates the graph and then shows it in the plot viewer. If you don’t have the brackets, you’ve only created the object, but haven’t visualized it. 
# You would then have to call the object such that it will be displayed by just typing samp_locations after you’ve created the “samp_locations” object.

length(unique(TestHosts$ID))

table(with(TestHosts, tapply(Grid, ID, function(x) length(unique(x)))))

# Model fitting
# First without random effects ####

# Specify the formula
f0.1 <- as.formula(paste0(resp, " ~ ", # Response first
                          paste(covar, collapse = " + ") # Collapse the vector of covariates
))

# Run the model
IM0.1  <- inla(Parasite.count ~ Month + Sex + Smi + Supp.corrected + Treated, 
               family = "nbinomial", # Specify the family. Can be a wide range (see r-inla.org).
               data = TestHosts) # Specify the data

# Run the model # (This is the same thing)
IM0.1  <- inla(f0.1, 
               family = "nbinomial", # Specify the family. Can be a wide range (see r-inla.org).
               data = TestHosts) # Specify the data

# Then with an ID random effect ####

f0.2 <- as.formula(paste0(resp, " ~ ", 
                          paste(covar, collapse = " + "), 
                          " +  f(ID, model = 'iid')")) # This is how you include  a typical random effect.

IM0.2  <- inla(f0.2, 
               family = "nbinomial",
               data = TestHosts) 

summary(IM0.1)
summary(IM0.2)
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
  coord_cartesian(xlim = c(-5, 5)) +
  theme_bw()

# greg albery wrote a function to perform model selection
HostModelSel <- INLAModelSel(resp, covar, "ID", "iid", "nbinomial", TestHosts)
# conclusion is to drop body condition and food supplementation, and keep: treatment, sex, month
Finalcovar <- HostModelSel$Removed[[length(HostModelSel$Removed)]]
Finalcovar  <- c("Month", # Julian month of sampling
           "Sex", # Sex
           "Treated") # Treatment


f1 <- as.formula(paste0(resp, " ~ ", 
                        paste(Finalcovar, collapse = " + "), 
                        "+ f(ID, model = 'iid')")) 

IM1 <- inla(f1,
            family = "nbinomial",
            data = TestHosts,
            control.compute = list(dic = TRUE)) 

summary(IM1)

# now to the complex part where we add the mesh to account for spatial variation
Locations = cbind(TestHosts$Easting, TestHosts$Northing) # using the sampling locations 

MeshA <- inla.mesh.2d(jitter(Locations), max.edge = c(20, 40))
MeshB <- inla.mesh.2d(Locations, max.edge = c(20, 40))
MeshC <- inla.mesh.2d(Locations, max.edge = c(10, 20))

Mesh <- MeshB

plot(MeshA)

plot(MeshB)

plot(MeshC)

points(Locations, col = "red", pch = 2)

# Making the A matrix - this is where i ran into trouble with hill/cedar's code

HostsA <- inla.spde.make.A(Mesh, loc = Locations) # Making A matrix
Hosts.spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)) # Making SPDE
w.Host <- inla.spde.make.index('w', n.spde = Hosts.spde$n.spde) # making the w


# Making the model matrix #### 

X0 <- model.matrix(as.formula(paste0(" ~ -1 + ", paste(Finalcovar, collapse = " + "))), data = TestHosts) # make the model matrix using the final model selection formula without a response variable.

X <- as.data.frame(X0[,-which(colnames(X0)%in%c("Month7"))]) # convert to a data frame. Eliminate the base level of the first categorical variable if applicable (you will manually specify an intercept below) 

head(X)

# Making the stack ####

N <- nrow(TestHosts)

StackHost <- inla.stack(
  data = list(y = TestHosts[,resp]), # specify the response variable
  
  A = list(1, 1, 1, HostsA), # Vector of Multiplication factors for random and fixed effects              
  
  effects = list(
    
    Intercept = rep(1, N), # specify the manual intercept!
    
    X = X, # attach the model matrix
    
    ID = TestHosts$ID, # insert vectors of any random effects
    
    w = w.Host)) # attach the w 

# now let's run
f1 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + ")))
f2 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid')"))
f3 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid') + f(w, model = Hosts.spde)"))


IM1 <- inla(f1, # Base model (no random effects)
            family = "nbinomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost))
)

IM2 <- inla(f2, # f1 + Year and ID random effects
            family = "nbinomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost))
)

IM3 <- inla(f3, # f2 + SPDE random effect 
            family = "nbinomial",
            data = inla.stack.data(StackHost),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost))
)

SpatialHostList <- list(IM1, IM2, IM3)
ggField(IM3, Mesh, Groups = 1) +
  scale_fill_brewer(palette = "Blues") 

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

# Specifying a new set of SPDE components ####

Groups = "Month"

NGroups <- length(unique(TestHosts[,Groups])) 

HostA2 <- inla.spde.make.A(Mesh, # Leave
                           loc = Locations, # Leave
                           group = as.numeric(as.factor(TestHosts[,Groups])),# this must be a numeric value counting from 1. If the groups variable is a factor, this will happen by default.
                           n.group = NGroups) 

w.Host2 <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = Hosts.spde$n.spde,
  n.group = NGroups)  

StackHost2 <- inla.stack( 
  data = list(y = TestHosts[,resp]), # Leave
  
  A = list(1, 1, 1, HostA2), # Change the A matrix to the new one
  
  effects = list(
    Intercept = rep(1, N), # Leave
    X = X, # Leave
    ID = TestHosts$ID, # Leave
    
    w = w.Host2)) # CHANGE
f4 = as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), 
                       " +  f(ID, model = 'iid') +  f(w, model = Hosts.spde, 
group = w.group,                           # This bit is new! 
control.group = list(model = 'iid'))"))

inla.setOption(num.threads = 8) 

IM4 <- inla(f4,
            family = "nbinomial",
            data = inla.stack.data(StackHost2), # Don't forget to change the stack!
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost2)) # Twice!
)

SpatialHostList[[4]] <- IM4
Labels = c("July", "August", "September", "October", "November")
names(Labels) <- c(1:NGroups)

ggField(IM4, Mesh, Groups = NGroups) + # Notice the groups argument, using the number of unique months.
  scale_fill_brewer(palette = "Reds") + 
  facet_wrap( ~ Group, labeller = labeller(Group = Labels), ncol = 3) # Doing this manually changes the facet labels
INLARange(SpatialHostList[3:4], MaxRange = Maxrange, MeshList = Mesh, ModelNames = c("Full", "Monthly"))
# this isn't working

f5 = as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), 
                       "+ f(ID, model = 'iid') +  f(w, model = Hosts.spde, 
                       group = w.group, # This bit is new! 
                       control.group = list(model='exchangeable'))"))

#inla.setOption(num.threads = 8) 

IM5 <- inla(f5,
            family="nbinomial",
            data = inla.stack.data(StackHost2),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost2))
)

SpatialHostList[[5]] <- IM5
INLADICFig(SpatialHostList, ModelNames = c("Base", "IID", "SPDE", "SPDE2", "SPDE3"))
ggField(IM5, Mesh, Groups = NGroups) + # Notice the groups argument, using the number of unique months.
  scale_fill_brewer(palette = "Greens") 

Group2 = "Grid"

TestHosts$Easting2 <- TestHosts$Easting - with(TestHosts, tapply(Easting, Grid, min))[TestHosts$Grid]
TestHosts$Northing2 <- TestHosts$Northing - with(TestHosts, tapply(Northing, Grid, min))[TestHosts$Grid]

Locations2 = cbind(TestHosts$Easting2, TestHosts$Northing2)

Mesh2 <- inla.mesh.2d(Locations2, max.edge = c(20, 40))#, cutoff = 0.8)

NGroup2 <- length(unique(TestHosts[,Group2]))

Hosts.spde2 = inla.spde2.pcmatern(mesh = Mesh2, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)) # Making SPDE

HostA3 <- inla.spde.make.A(Mesh2, loc = Locations2,
                           repl = as.numeric(TestHosts[,Group2]),
                           n.repl = NGroup2)

w.Host3 <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = Hosts.spde2$n.spde,
  n.repl = NGroup2)  

StackHost3 <- inla.stack(
  data = list(y = TestHosts[,resp]),  
  A = list(1, 1, 1, HostA3), # Change A matrix
  effects = list(
    
    Intercept = rep(1, N), # Leave
    
    X = X, # Leave
    
    ID = TestHosts$ID, # Leave
    
    w = w.Host3)) # Change 

f6 = as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), 
                       " +  f(ID, model = 'iid') +   
                       f(w, model = Hosts.spde2, replicate = w.repl)")) # Not necessary to specify a linking model

IM6 <- inla(f6,
            family = "nbinomial",
            data = inla.stack.data(StackHost3),
            control.compute = list(dic = TRUE),
            control.predictor = list(A = inla.stack.A(StackHost3))
)

SpatialHostList[[6]] <- IM6







  