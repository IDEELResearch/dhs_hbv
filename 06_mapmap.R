# Mapping extra_rev
library(janitor)
library(tidyverse)
library(sf)
library(srvyr)
library(survey)
library(patchwork)
library(reshape)
options(survey.lonely.psu="adjust")

designf <-svydesign(ids=elig_kids_whbvres_wt_kr$hv001, strata=elig_kids_whbvres_wt_kr$hv022 , weights=elig_kids_whbvres_wt_kr$both_wt_new,  data=elig_kids_whbvres_wt_kr)
designf_dhs2 <-as_survey_design(designf)


# use kidsmap_impsf

# by age and sex-------
agesexprov5 <- svyby(~prov2015,~hbvresult5+sex+hv105_fromhc1_f, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
agesexprov5 <- agesexprov5 %>% mutate(level = paste0('hbv', level))
agesexprov5t <- as.data.frame(t(as.data.frame(agesexprov5))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))

agesexprov5t <- agesexprov5t %>% mutate(across(-c(level), as.numeric))
view(agesexprov5t)

agesexprov5t_p <- agesexprov5t %>% mutate(
  boys0p = 100*(hbv1.Male.0/(hbv0.Male.0 + hbv1.Male.0)),
  boys1p = 100*(hbv1.Male.1/(hbv0.Male.1 + hbv1.Male.1)),
  boys2p = 100*(hbv1.Male.2/(hbv0.Male.2 + hbv1.Male.2)),
  boys3p = 100*(hbv1.Male.3/(hbv0.Male.3 + hbv1.Male.3)),
  boys4p = 100*(hbv1.Male.4/(hbv0.Male.4 + hbv1.Male.4)),
  girls0p = 100*(hbv1.Female.0/(hbv0.Female.0 + hbv1.Female.0)),
  girls1p = 100*(hbv1.Female.1/(hbv0.Female.1 + hbv1.Female.1)),
  girls2p = 100*(hbv1.Female.2/(hbv0.Female.2 + hbv1.Female.2)),
  girls3p = 100*(hbv1.Female.3/(hbv0.Female.3 + hbv1.Female.3)),
  girls4p = 100*(hbv1.Female.4/(hbv0.Female.4 + hbv1.Female.4)))

agesexprov5t_p$hyphen <- gsub("prov2015", "", agesexprov5t_p$level)
#agesexprov5t_p$ADM1_NAME <- toupper(agesexprov5t_p$prov)
agesexprov5t_p2 <- agesexprov5t_p %>% select(c(hyphen, starts_with("boys"), starts_with("girls")))

library(data.table)
library(reshape2)
agesexprov5t_p2m <- melt(agesexprov5t_p2, id = "hyphen")
agesexprov5t_p2m <- agesexprov5t_p2m %>% mutate(
  value_g = case_when(
    value == 0 ~ 0,
    value > 0 & value<= 5.5 ~ 1,
    value > 5.5 & value<= 10.49 ~ 2,
    value > 10.49 & value<= 15.49 ~ 3,
    value > 15.49  ~ 4),
  value_gf = factor(value_g,
                      levels = c(0,1,2,3,4),
                      labels = c("0%", ">0-5%", ">5-10%", ">10-15%",">15%")))
view(agesexprov5t_p2m)
drcprov_agesex <- left_join( drcprov[,c("hyphen", "geometry")], agesexprov5t_p2m, by="hyphen")
#drcprov_agesex <- left_join(drcprov,agesexprov5t_p2, by="hyphen")

my_greens4 = c("#F7F7F7","#BBE3B4","#41ae76", "#238b45","#00441b") #there are 9, I exluded the two lighter hues

drcprov_agesex <- drcprov_agesex %>% mutate(label = case_when(
  variable == "boys0p" ~ "Boys, 6-11mo",
  variable == "girls0p" ~ "Girls, 6-11mo",
  variable == "boys1p" ~ "Boys, 12-23mo",
  variable == "girls1p" ~ "Girls, 12-23mo",
  variable == "boys2p" ~ "Boys, 24-35mo",
  variable == "girls2p" ~ "Girls, 24-35mo",
  variable == "boys3p" ~ "Boys, 36-47mo",
  variable == "girls3p" ~ "Girls, 36-47mo",
  variable == "boys4p" ~ "Boys, 48-59mo",
  variable == "girls4p" ~ "Girls, 48-59mo"))

asorder <-  c( "Boys, 6-11mo", "Boys, 12-23mo",  "Boys, 24-35mo",  "Boys, 36-47mo",  "Boys, 48-59mo", "Girls, 6-11mo",  "Girls, 12-23mo", "Girls, 24-35mo", "Girls, 36-47mo", "Girls, 48-59mo")

ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_agesex,  mapping=aes(fill=value_gf))+
  #scale_fill_distiller(palette = 'Greens', direction = 1, limits = c(0, 25)) + #  YlGn
  scale_fill_manual(values = my_greens)+
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
        legend.title = element_blank())+
  #facet_grid(~label)
  facet_wrap(~factor(label, levels=asorder), ncol = 5, nrow=2)
ggsave('./Plots/provsexage_feball.png', width=12, height=6)

# prev diff------
view(agesexprov5t_p2)
agesexprov5t_p2 <- agesexprov5t_p2 %>% mutate(
  pd0 = boys0p  - girls0p,
  pd1 = boys1p  - girls1p,
  pd2 = boys2p  - girls2p,
  pd3 = boys3p  - girls3p,
  pd4 = boys4p  - girls4p)
pdm <- agesexprov5t_p2 %>% select(c("hyphen", starts_with("pd")))

pdm2 <- melt(pdm, id = "hyphen")
table(pdm2$variable)

table(pdm2$value)

pdm2 <- pdm2 %>% mutate(labelmo = case_when(
    variable == "pd0" ~ "6-11mo",
    variable == "pd1" ~ "12-23mo",
    variable == "pd2" ~ "24-35mo",
    variable == "pd3" ~ "36-47mo",
    variable == "pd4" ~ "48-59mo"),
  value_g = case_when(
    value < -15 ~ 0,
    value >= -15  & value < -10 ~ 1,
    value >= -10 & value < -5 ~ 2,
    value >= -5  & value < 0 ~ 3,
    value == 0 ~ 4,
    value > 0  & value <= 5 ~ 5,
    value > 5  & value <= 10 ~ 6,
    value > 10  & value <= 15 ~ 7,
    value >15  ~ 8),
  value_gf = factor(value_g,
     levels = c(0,1,2,3,4,5,6,7,8),
     labels = c("Girls >15", "Girls >10-15",  "Girls >5-10", "Girls >0-5",  "No difference", "Boys >0-5", "Boys >5-10", "Boys >10-15","Boys >15")))

pdm2_sf <- left_join( drcprov[,c("hyphen", "geometry")], pdm2, by="hyphen")

table(pdm2$value_g)
library(RColorBrewer)
my_spec = brewer.pal(n = 9, "Spectral")[2:9] #there are 9, I exluded the two lighter hues
my_piyg = brewer.pal(n = 9, "PiYG")[2:9] #there are 9, I exluded the two lighter hues
my_puor = brewer.pal(n = 9, "PuOr")[2:9] #there are 9, I exluded the two lighter hues

pdorder <-  c( "6-11mo", "12-23mo",  "24-35mo",  "36-47mo",  "48-59mo")

ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=pdm2_sf,  mapping=aes(fill=value_gf))+
  #scale_fill_distiller(palette = 'Greens', direction = 1, limits = c(0, 25)) + #  YlGn
  scale_fill_manual(values = my_puor)+
  #scale_fill_brewer(palette = 'Spectral') +
  theme_bw(base_size=14) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) +
  labs(fill = "HBsAg infections per 100")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        )+ #legend.title = element_blank()
  #facet_grid(~label)
  facet_wrap(~factor(labelmo, levels = pdorder), ncol = 5, nrow=1)
ggsave('./Plots/provsexage_feb.png', width=12, height=3)

# boys vs girls pds (all ages together)-------
# by age and sex
sexprov5 <- svyby(~prov2015,~hbvresult5+sex, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
sexprov5 <- sexprov5 %>% mutate(level = paste0('hbv', level))
sexprov5t <- as.data.frame(t(as.data.frame(sexprov5))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))
sexprov5t <- sexprov5t %>% mutate(across(-c(level), as.numeric))
# calc prevs
sexprov5tp <- sexprov5t %>% mutate(
  boysp = 100*(hbv1.Male/(hbv0.Male + hbv1.Male)),
  girlsp = 100*(hbv1.Female/(hbv0.Female + hbv1.Female)),
  pd = boysp - girlsp)
view(sexprov5tp)
sexprov5tp$hyphen <- gsub("prov2015", "", sexprov5tp$level)

sexprov5tpm <- sexprov5tp %>%  select(c(hyphen, boysp,girlsp, pd)) #%>% melt()
view(sexprov5tpm)
sexprov5tpm %>% group_by(pd) %>% count() %>% print(n=Inf)

sexprov5tpm <- sexprov5tpm %>% mutate(
  pdsex_g = case_when(
    pd < -2 ~ 0, #Girls 2+ more infections
    pd >= -2  & pd < -1 ~ 1, # Girls 1+ more infections
    boysp == 0 & girlsp == 0 ~ 2, # DISTINGUISH no diff and prev was zero
    pd >= -1 & pd < 1  & (boysp != 0 | girlsp != 0) ~ 3, # DISTINGUISH no diff and prev was zero vs no diff but prev >0
    pd >= 1  & pd < 2 ~ 4, # Boys 1 more
    pd >= 2  & pd < 3 ~ 5, #Boys 2 more
    pd >= 3  & pd < 4 ~ 6, # none in this group
    pd >= 4 & pd < 5 ~ 7, #Boys 4 more (none with 3)
    pd >= 5 & pd < 6 ~ 8, #Boys 5 more (none with 3)
    pd >= 6 & pd < 7 ~ 9, # none here
    pd >= 7 & pd < 8 ~ 10, # none here
    pd > 8  ~ 11), # boys with 8 more
  pdsex_gf = factor(pdsex_g,
                    levels = c(0,1,2,3,4,5,6,7,8,9,10,11),
                    labels = c("Girls 2+ more", "Girls 1 more",  "No difference (0% prev in both)", "No difference (prev >0%)", "Boys 1 more", "Boys 2 more"  ,"Boys 3 more", "Boys 4 more", "Boys 5 more", "Boys 6 more", "Boys 7 more", "Boys 8+ more")))
table(sexprov5tpm$pdsex_gf) # need 2 orange for girls, 1 white, 5 purple for boys
drcprovpdsex_sf <- left_join( drcprov[,c("hyphen", "geometry")], sexprov5tpm, by="hyphen")

library(RColorBrewer)
brewer.pal(n = 9, "PuOr")[1:9] #there are 9, I exluded the two lighter hues
 # using chrome.hs, get 16 of this palette for 8 on either side, then choose nth for each
 c('#e08214', '#e68d29', '#ec993b', '#f2a44d', '#f7b05f', '#fabb72', '#fdc787', '#ffd49e', "#F7F7F7",'#a49bc8', '#988bbe', '#8c7bb4', '#806aab', '#755aa2', '#6a4a99', '#5f3990', '#542788' )
sexpuor <- c('#f7b05f', '#ffd49e', "lightgray","#F7F7F7",'#a49bc8', '#8c7bb4', '#755aa2', '#6a4a99','#542788' )
drcprovpdsex_sf$pdsex_gf
  
sexpds <-
  ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprovpdsex_sf,  mapping=aes(fill=pdsex_gf))+
   geom_sf_text(data=drcprovpdsex_sf[drcprovpdsex_sf$pdsex_g != 2 & drcprovpdsex_sf$pdsex_g != 3 & drcprovpdsex_sf$hyphen!="Kasai-Oriental" & drcprovpdsex_sf$hyphen!="Kasai-Central" & drcprovpdsex_sf$hyphen!="Lomami" ,], 
                mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 3, fontface = "bold") + 
  geom_sf_text(data= drcprovpdsex_sf[drcprovpdsex_sf$pdsex_g != 2 & drcprovpdsex_sf$pdsex_g != 3 & drcprovpdsex_sf$hyphen=="Kasai-Central",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .5, size = 3, fontface = "bold") + 
  geom_sf_text(data= drcprovpdsex_sf[drcprovpdsex_sf$pdsex_g != 2 & drcprovpdsex_sf$pdsex_g != 3 & drcprovpdsex_sf$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = 0, size = 3, fontface = "bold") + 
  geom_sf_text(data= drcprovpdsex_sf[drcprovpdsex_sf$pdsex_g != 2 & drcprovpdsex_sf$pdsex_g != 3  & drcprovpdsex_sf$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 3, fontface = "bold") + 
  scale_fill_manual(values = sexpuor)+
  theme_bw(base_size=10) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) +
  labs(fill = "HBsAg infections per 100", x='', y='')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))
ggsave('./Plots/provsexpd.png', width=9, height=6)

## prevalence for boys and girls-----
sexprov5tpm2 <- sexprov5tpm %>% select(c(hyphen, girlsp, boysp)) %>% melt()

sexprov5tpm2 <- sexprov5tpm2 %>% mutate(value_g = case_when(
  value == 0 ~ 0, # 0%
  value >0 & value <= 0.5 ~ 1,
  value >0.500 & value <= 1.49 ~ 2,
  value >1.49 & value <= 2.49 ~ 3,
  value >2.49 & value <= 3.49 ~ 4,
  value >3.49 & value <= 4.49 ~ 5,
  value >4.49 & value <= 5.49 ~ 6,
  value >5.49 & value <= 6.49 ~ 7,
  value >6.49 & value <= 7.49 ~ 8,
  value >7.49 & value <= 8.49 ~ 9,
  value >8.49 & value <= 9.49 ~ 10,
  value >9.49  ~ 11 # >10%
) %>% as.factor(),
value_gf = factor(value_g,
                  levels = c(0,1,2,3,4,5,6,7,8,9,10,11),
                  labels = c("0%", "<1%","1%","2%", "3%","4%","5%", "6%", "7%", "8%", "9%", ">10%")))

# add geometry for provs
drcprovsexprev_sf <- left_join( drcprov[,c("hyphen", "geometry")], sexprov5tpm2, by="hyphen")
mygreens8of12
# "#eaf7e6" "#cae9c8" "#aadaaf" "#89cb9a" "#66bc86" "#40ac74" "#339b5e" "#116531",'#00441b' 9 and >10 '#075525', '#00441b'; 6-7 '#27894d', '#1c773e',
mygreens9of12 <- c('#F7F7F7','#eaf7e6', '#cae9c8', '#aadaaf', '#89cb9a', '#66bc86', '#40ac74', '#339b5e',  '#00441b') #9 and >10 '#075525', '#00441b'; 6-7 '#27894d', '#1c773e',

ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprovsexprev_sf,  mapping=aes(fill=value_gf))+
  #scale_fill_distiller(palette = 'Greens', direction = 1)+
  scale_fill_manual(values = mygreens9of12)+
  theme_bw(base_size=14) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) +
  labs(fill = "HBsAg infections per 100", x='', y='')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA))+
  facet_wrap(~factor(variable, levels = c("girlsp", "boysp"), labels = c("Girls <5", "Boys <5")))
ggsave('./Plots/provsexprevs.png', width=12, height=6)


#chart of prevs over time-----
view(agesexprov5t_p2m)
test <- agesexprov5t_p2m %>% mutate(age = str_sub(variable,-2,-2) %>% as.numeric(),
                                    sex = str_sub(variable, 1,4),
                                    biryr = 2013-age)
view(test)

provprev <- test %>% group_by(hyphen) %>% summarise(avgprev = mean(value)) %>% mutate(belowavg = case_when(avgprev>1.2 ~ "Prev >1.2%",
                                                                                                          avgprev <=1.2 ~ "Avg/below 1.2%"))
test2 <- left_join(test, provprev, by = "hyphen")
view(test2$avgprev)
test2 %>% filter(belowavg == "Prev >1.2%") %>% 
  ggplot(aes(x=biryr, y = value, color = fct_reorder(hyphen, desc(avgprev))))+
  geom_line()+
  labs(y="HBsAg prevalence", x="Birth cohort")+
#  scale_color_manual(values = c('#00429d', '#325da9', '#4e78b5', '#6694c1', '#80b1cc', '#9dced6', '#c0eade', '#ffdac4', '#ffb3a7', '#fb8a8c', '#eb6574', '#d5405e', '#b81b4a', '#93003a'))+
  scale_color_manual(values = c('#93003a', '#b81b4a', '#d5405e', '#eb6574', '#fb8a8c', '#ffb3a7', '#ffdac4', '#c0eade', '#9dced6', '#80b1cc', '#6694c1', '#4e78b5', '#325da9', '#00429d'))+
  theme(panel.background = element_blank(),
        legend.title = element_blank())+
  facet_wrap(~sex, nrow=2)

# use age in months to assess over time----
agemoprov5 <- svyby(~prov2015,~hbvresult5+hc1, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
agemoprov5 <- agemoprov5 %>% mutate(level = paste0('hbv', level))
agemo_prov5t <- as.data.frame(t(as.data.frame(agemoprov5))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))

agemo_prov5t <- agemo_prov5t %>% mutate(across(-c(level), as.numeric))
colnames(agemo_prov5t)

agesexprov5t_p <- agesexprov5t %>% mutate(
  boys0p = 100*(hbv1.Male.0/(hbv0.Male.0 + hbv1.Male.0)),
  boys1p = 100*(hbv1.Male.1/(hbv0.Male.1 + hbv1.Male.1)),
  boys2p = 100*(hbv1.Male.2/(hbv0.Male.2 + hbv1.Male.2)),
  boys3p = 100*(hbv1.Male.3/(hbv0.Male.3 + hbv1.Male.3)),
  boys4p = 100*(hbv1.Male.4/(hbv0.Male.4 + hbv1.Male.4)),
  girls0p = 100*(hbv1.Female.0/(hbv0.Female.0 + hbv1.Female.0)),
  girls1p = 100*(hbv1.Female.1/(hbv0.Female.1 + hbv1.Female.1)),
  girls2p = 100*(hbv1.Female.2/(hbv0.Female.2 + hbv1.Female.2)),
  girls3p = 100*(hbv1.Female.3/(hbv0.Female.3 + hbv1.Female.3)),
  girls4p = 100*(hbv1.Female.4/(hbv0.Female.4 + hbv1.Female.4)))

# map by age only------
ageprov5 <- svyby(~prov2015,~hbvresult5+hv105_fromhc1_f, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
ageprov5 <- ageprov5 %>% mutate(level = paste0('hbv', level))
ageprov5t <- as.data.frame(t(as.data.frame(ageprov5))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))

ageprov5t <- ageprov5t %>% mutate(across(-c(level), as.numeric))
view(ageprov5t)

ageprov5t_p <- ageprov5t %>% mutate(
  p0 = 100*(hbv1.0/(hbv0.0 + hbv1.0)),
  p1 = 100*(hbv1.1/(hbv0.1 + hbv1.1)),
  p2 = 100*(hbv1.2/(hbv0.2 + hbv1.2)),
  p3 = 100*(hbv1.3/(hbv0.3 + hbv1.3)),
  p4 = 100*(hbv1.4/(hbv0.4 + hbv1.4)))
ageprov5t_p$hyphen <- gsub("prov2015", "", ageprov5t_p$level)
view(ageprov5t_p)

ageprov5t_p2 <- ageprov5t_p %>% select(-level) %>% relocate(hyphen)
ageprov5t_p2 <- ageprov5t_p2 %>% select(c(hyphen, starts_with("p"))) 
view(ageprov5t_p2)
ageprov5t_p2m <- melt(ageprov5t_p2, id = "hyphen")
view(ageprov5t_p2m)

ageprov5t_p2m <- ageprov5t_p2m %>% mutate(
  value_g = case_when(
    value == 0 ~ 0,
    value > 0 & value<= 5.5 ~ 1,
    value > 5.5 & value<= 10.49 ~ 2,
    value > 10.49 & value<= 15.49 ~ 3,
    value > 15.49  ~ 4),
  value_gf = factor(value_g,
                    levels = c(0,1,2,3,4),
                    labels = c("0%", ">0-5%", ">5-10%", ">10-15%",">15%")),
  labelmo = case_when(
    variable == "p0" ~ "6-11mo",
    variable == "p1" ~ "12-23mo",
    variable == "p2" ~ "24-35mo",
    variable == "p3" ~ "36-47mo",
    variable == "p4" ~ "48-59mo"),
  age = str_sub(variable,2,2) %>% as.numeric(),
  biryr = 2013 - age)

drcprov_age <- left_join( drcprov[,c("hyphen", "geometry")], ageprov5t_p2m, by="hyphen")
my_greens4 = c("#F7F7F7","#BBE3B4",'#53955e',   "#00441b") #there are 9, I exluded the two lighter hues
#'#66bc86', '#40ac74',   "#41ae76",'#116531',
mapagerev <- 
  ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_age,  mapping=aes(fill=value_gf))+
  #scale_fill_distiller(palette = 'Greens', direction = 1, limits = c(0, 25)) + #  YlGn
  scale_fill_manual(values = my_greens4)+
  geom_sf_text(data= drcprov_age[drcprov_age$value_g >0 & drcprov_age$hyphen!="Kasai-Oriental" & drcprov_age$hyphen!="Kasai-Central" & drcprov_age$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 2, fontface = "bold") + 
  #geom_sf_text(data= drcprov_age[drcprov_age$value_g >0 ,], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 3) + #, fontface = "bold"
  geom_sf_text(data= drcprov_age[drcprov_age$value_g >0 & drcprov_age$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 2, fontface = "bold") + 
  geom_sf_text(data= drcprov_age[drcprov_age$value_g >0 & drcprov_age$hyphen=="Kasai-Central",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.3, size = 2, fontface = "bold") + 
  geom_sf_text(data= drcprov_age[drcprov_age$value_g >0 &drcprov_age$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 2, fontface = "bold") + 
  theme_bw(base_size=14) + 
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) +
  labs(fill = "HBsAg infections per 100", x='',y='')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="#daeff8", color=NA),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)
  )+ #legend.title = element_blank()
  #facet_wrap(~age, ncol = 5, nrow=1)
  ggtitle("Province-level HBsAg prevalence by age cohort, oldest to youngest")+
  facet_wrap(~reorder(factor(age, levels = c(4,3,2,1,0), labels = c("48-59 months", "36-47 months", "24-35 months", "12-23 months", "6-12 months")), biryr), ncol = 5, nrow=1)
mapagerev
ggsave('./Plots/provageyrprevs.png', width=12, height=4)

threemorolling
ggsave('./Plots/tang/3moavgprev_byprov.png', width=12, height=6)

# combine plot for supplement for age: mapagerev and threemorolling (from 06c_moremaps.R)
mapagerev + threemorolling + plot_layout(nrow=2, ncol = 1) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/provageyrprevs.png', width=12, height=6)
# problem with this is the two are the same height, but really the lower one needs to be taller (2/3 ish of the height and the maps ~1/3)


# prev over time, overall-------
# use 'prov2015_by' from 02_tablesandfigures.R
prov2015_by$prev100 <- prov2015_by$hbvresult5*100

prov5_by <- prov2015_by %>% mutate(compavg = case_when(
  prev100 > 1.2 ~ "Above average",
  prev100 <= 1.2 ~ "At/below average"
))
prov5_by <- prov5_by %>% rename(hyphen = covname)

ageprov5t_p2m2 <- left_join(ageprov5t_p2m, prov5_by, by = "hyphen")
view(prov2015_by)

timetrendall <- 
  ageprov5t_p2m2 %>% 
  ggplot(aes(x=biryr, y = value, color = fct_reorder(hyphen, desc(prev100))))+
  geom_line()+
  labs(y="HBsAg prevalence", x="Birth cohort")+
  #'#93003a', '#a80a42', '#bb1e4c', '#cb3256', '#d94561', '#e5596d', '#ef6d79', '#f88186', '#fe9694', '#ffaca3', '#ffc2b2', '#ffd7c2', '#ffebd1', '#ffffe0', '#d6f5e0', '#bde7dd', '#a9d9d9', '#98c9d4', '#88bacf', '#7aaac9', '#6c9bc3', '#5f8bbd', '#517cb7', '#446db1', '#345faa', '#2250a4', '#00429d'
  scale_color_manual(values = c('#93003a', '#a70842', '#b81b4a', '#c82d54', '#d5405e', '#e15268', '#eb6574', '#f4777f', '#fb8a8c', '#ff9e99', '#ffb3a7', '#ffc6b6',  '#d8f6e1', '#c0eade', '#addcda', '#9dced6', '#8ebfd1', '#80b1cc', '#73a2c6', '#6694c1', '#5a86bb', '#4e78b5', '#406aaf', '#325da9', '#204fa3', '#00429d'))+
  #scale_color_manual(values = c('#67001f', '#700226', '#78042d', '#810834', '#890d3c', '#921243', '#9a174b', '#a21c53', '#aa225b', '#b12963', '#b92f6b', '#bf3673', '#c63d7c', '#cb4584', '#d04d8d', '#d45695', '#d85f9d', '#da69a5', '#dc72ad', '#dd7cb4', '#dd87bb', '#dc91c2', '#db9bc8', '#d9a5cf', '#d7afd4', '#d4b9da'))+
  #  scale_color_manual(values = c('#00441b', '#024b1f', '#065224', '#0a5a29', '#0f612e', '#146933', '#187139', '#1d783f', '#228045', '#27884c', '#2c9053', '#31985b', '#36a064', '#3ca76d', '#45af77', '#57b67f', '#68bd87', '#77c38f', '#86ca98', '#95d1a1', '#a3d7ab', '#b2deb5', '#c0e4c0', '#ceebcb', '#dcf1d8', '#eaf7e6'))+
  #scale_color_manual(values = c('#93003a', '#b81b4a', '#d5405e', '#eb6574', '#fb8a8c', '#ffb3a7', '#ffdac4', '#c0eade', '#9dced6', '#80b1cc', '#6694c1', '#4e78b5', '#325da9', '#00429d'))+
  theme(panel.background = element_blank(),
        legend.title = element_blank())+
  facet_wrap(~compavg, nrow=2)
mapagerev + timetrendall + plot_layout(nrow=2, ncol = 1) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/provsexage_feb.png', width=12, height=6)

## tetanus by province-------
provtet5 <- svyby(~prov2015,~hbvresult5+tetab, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
provtet5 <- provtet5 %>% mutate(level = paste0('hbv', level))
provtet5t <- as.data.frame(t(as.data.frame(provtet5))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))

provtet5t <- provtet5t %>% mutate(across(-c(level), as.numeric))

provtet5t <- provtet5t %>% mutate(
  Nonreactive_p = 100*(hbv1.Nonreactive/(hbv1.Nonreactive + hbv0.Nonreactive)),
  Reactive_p = 100*(hbv1.Reactive/(hbv1.Reactive + hbv0.Reactive)),
  pdtet = Reactive_p  - Nonreactive_p,
  pdtet_g = case_when(
    pdtet < -5 ~ 0, #Reactive Ab 5+ fewer infections
    pdtet >= -5  & pdtet < -4 ~ 1, # Reactive Ab >4-5 fewer
    pdtet >= -4 & pdtet < -3 ~ 2, # Reactive Ab >3-4 fewer
    pdtet >= -3  & pdtet < -2 ~ 3, # Reactive Ab >2-3 fewer
    pdtet >= -2  & pdtet < -1 ~ 4, #Reactive Ab >1-2 fewer
    pdtet >= -1  & pdtet <= 1 & (Reactive_p == 0 & Nonreactive_p == 0) ~ 5, # no diff (and prev = 0 in both)
    pdtet >= -1  & pdtet <= 1 & (Reactive_p != 0 | Nonreactive_p != 0) ~ 6, # no diff (and prev = 0 in both)
    pdtet > 1  & pdtet <= 2 ~ 7,
    pdtet > 2  ~ 8),
  pdtet_gf = factor(pdtet_g,
                    levels = c(0,1,2,3,4,5,6,7,8),
                    labels = c("Tetanus sero-neg 5+ more", "Tetanus sero-neg 4 more",  "Tetanus sero-neg 3 fewer", "Tetanus sero-neg 2 more", "Tetanus sero-neg 1 more"  ,"No difference (prev 0% in both)", "No difference (prev >0%)","Tetanus sero-pos 1 more", "Tetanus sero-pos 2+ more")))

view(provtet5t)

n11colorpigr <- c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#f7f7f7", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419")
n11colorrdbl <- c('#93003a', '#c52a52', '#e75d6f', '#fd9291', '#ffcab9', '#ffffe0', '#b1dfdb', '#85b7ce', '#618fbf', '#3e67ae', '#00429d')
n11colorblrd <- c('#00429d', '#3e67ae', '#618fbf', '#85b7ce', '#b1dfdb',"#f7f7f7", '#ffffe0', '#ffcab9', '#fd9291','#e75d6f',  '#c52a52', '#93003a')

provtet5t$hyphen <- gsub("prov2015", "", provtet5t$level)
drcprov_tet <- left_join( drcprov[,c("hyphen", "geometry")], provtet5t, by="hyphen")

provtet<-
  ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_tet,  mapping=aes(fill=pdtet_gf))+
  geom_sf_text(data= drcprov_tet[drcprov_tet$pdtet_g != 5 & drcprov_tet$pdtet_g != 6 & drcprov_tet$hyphen!="Kasai-Oriental" & drcprov_tet$hyphen!="Kasai-Central" & drcprov_tet$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 3, fontface = "bold") + 
  geom_sf_text(data= drcprov_tet[drcprov_tet$pdtet_g != 5 & drcprov_tet$pdtet_g != 6 & drcprov_tet$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 3, fontface = "bold") + 
  geom_sf_text(data= drcprov_tet[drcprov_tet$pdtet_g != 5 & drcprov_tet$pdtet_g != 6 & drcprov_tet$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 3, fontface = "bold") + 
  scale_fill_manual(values = n11colorblrd)+
  theme_bw(base_size=10) + 
  labs(fill = "HBsAg infections per 100 children", x='',y='')+ # legend title if wanted HBsAg infections per 100 children comparing\nReactive vs nonreactive tetanus antibodies
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        panel.background = element_rect(fill="#daeff8", color=NA))#+
provtet
ggsave('./Plots/provtet_feb.png', width=12, height=6)

# combine from 06_mapping.R, see below for 4 including DPT
A5 + prov5 + provtet + plot_layout(nrow=1, ncol = 3) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps3.png', width=12, height=4)

A5 + prov5 + provtet + plot_layout(nrow=3, ncol = 1) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps3v.png', width=6, height=12)
#combine with sex pds - 2x2 box
A5 + prov5 + provtet +sexpds + plot_layout(nrow=2, ncol = 2) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps4box.png', width=12, height=9)

# maps for reactive and non-reactive by hbsag prev
provtet5tr <- provtet5t %>% select(c('hyphen', "Nonreactive_p", "Reactive_p"))
provtet5tr_m <- melt(provtet5tr)
view(provtet5tr_m)
provtet5tr_m <- provtet5tr_m %>% mutate(value_g = case_when(
  value == 0 ~ 0, # 0%
  value >0 & value <= 0.5 ~ 1,
  value >0.500 & value <= 1.49 ~ 2,
  value >1.49 & value <= 2.49 ~ 3,
  value >2.49 & value <= 3.49 ~ 4,
  value >3.49 & value <= 4.49 ~ 5,
  value >4.49 & value <= 5.49 ~ 6,
  value >5.49 & value <= 6.49 ~ 7,
  value >6.49 & value <= 7.49 ~ 8,
  value >7.49 & value <= 8.49 ~ 9,
  value >8.49 & value <= 9.49 ~ 10,
  value >9.49  ~ 11 # >10%
) %>% as.factor(),
value_gf = factor(value_g,
                  levels = c(0,1,2,3,4,5,6,7,8,9,10,11),
                  labels = c("0%", "<1%","1%","2%", "3%","4%","5%", "6%", "7%", "8%", "9%", ">10%")))

drcprov_teto <- left_join( drcprov[,c("hyphen", "geometry")], provtet5tr_m, by="hyphen")

ntetblrd <- c('#618fbf',  '#ffffe0', '#ffe5cc', '#ffcab9', '#ffaea5', '#fd9291',  '#f4777f',  '#c52a52') # '#e75d6f','#d84360', '#ae1045','#93003a'
my_greens4 = c("#F7F7F7","#BBE3B4","#41ae76", "#238b45","#00441b") #there are 9, I exluded the two lighter hues
mygreens8of12 <- c('#F7F7F7','#eaf7e6', '#cae9c8', '#aadaaf', '#89cb9a', '#66bc86', '#40ac74',   '#116531') #9 and >10 '#075525', '#00441b'; 6-7 '#27894d', '#1c773e', '#339b5e',

ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_teto,  mapping=aes(fill=value_gf))+
  #scale_color_continuous(palette = "PuRd")+ # 9 of 12 colors
  scale_fill_manual(values = mygreens8of12)+
  theme_bw(base_size=14) + 
  labs(fill = "HBsAg prevalence", x='',y='')+ # legend title if wanted HBsAg infections per 100 children comparing\nReactive vs nonreactive tetanus antibodies
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.background = element_rect(fill="#daeff8", color=NA))+
  facet_wrap(~factor(variable, levels = c("Nonreactive_p", "Reactive_p"), labels =c("Nonreactive tetanus Ab (n=2,436)", "Reactive tetanus Ab (n=4,698)")), nrow = 1, ncol = 2)
ggsave('./Plots/provtet_prevs.png', width=12, height=6)


## DPT vacc by province---------
# switch between dpt_any_f and dpt_count
# provdpt5 <- svyby(~prov2015,~hbvresult5+dpt_any_f, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")

provdptbin5 <- svyby(~prov2015,~hbvresult5+as.factor(dpt_any), designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
provdptbin5 <- provdptbin5 %>% mutate(level = paste0('hbv', level))
provdptbin5t <- as.data.frame(t(as.data.frame(provdptbin5))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))
provdptbin5t <- provdptbin5t %>% mutate(across(-c(level), as.numeric))
table(provdptbin5t$hbv1.1)

provdptbin5t <- provdptbin5t %>% mutate(
  Nodose_p = 100*(hbv1.0/(hbv1.0 + hbv0.0)),
  anydose_p = 100*(hbv1.1/(hbv1.1 + hbv0.1)),
  pd_dpt = anydose_p - Nodose_p)
view(provdptbin5t)

n11colorpigr <- c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#f7f7f7", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419")
n11colorrdbl <- c('#93003a', '#c52a52', '#e75d6f', '#fd9291', '#ffcab9', '#ffffe0', '#b1dfdb', '#85b7ce', '#618fbf', '#3e67ae', '#00429d')
n11colorblrd <- c('#00429d', '#3e67ae', '#618fbf', '#85b7ce', '#b1dfdb', '#ffffe0', '#ffcab9', '#fd9291','#e75d6f',  '#c52a52', '#93003a')

n9blrd <- c('#618fbf',  '#ffffe0', '#ffe5cc', '#ffcab9', '#ffaea5', '#fd9291',  '#f4777f',  '#ae1045','#93003a') #'#e75d6f','#d84360',  '#c52a52', 
mygreens9of12 <- c('#eaf7e6', '#cae9c8', '#aadaaf', '#89cb9a', '#66bc86',  '#339b5e', '#27894d', '#075525', '#00441b') #8  '#116531', ; 7  '#1c773e', 4 '#40ac74',

provdptbin5t$hyphen <- gsub("prov2015", "", provdptbin5t$level)
provdptbin5t2 <- provdptbin5t %>% select(c("hyphen", ends_with("_p")))
view(provdptbin5t2)
provdptbin5t2m <- melt(provdptbin5t2)
table(provdptbin5t2m$value)
view(provdptbin5t2m)

provdptbin5t2m <- provdptbin5t2m %>% mutate(value_g = case_when(
  value == 0 ~ 0, # 0%
  value >0 & value <= 0.5 ~ 1,
  value >0.500 & value <= 1.49 ~ 2,
  value >1.49 & value <= 2.49 ~ 3,
  value >2.49 & value <= 3.49 ~ 4,
  value >3.49 & value <= 4.49 ~ 5,
  value >4.49 & value <= 5.49 ~ 6,
  value >5.49 & value <= 6.49 ~ 7,
  value >6.49 & value <= 7.49 ~ 8,
  value >7.49 & value <= 8.49 ~ 9,
  value >8.49 & value <= 9.49 ~ 10,
  value >9.49  ~ 11 # >10%
) %>% as.factor(),
    value_gf = factor(value_g,
      levels = c(0,1,2,3,4,5,6,7,8,9,10,11),
      labels = c("0%", "<1%","1%","2%", "3%","4%","5%", "6%", "7%", "8%", "9%", ">10%")))

drcprov_dpt <- left_join( drcprov[,c("hyphen", "geometry")], provdptbin5t2m, by="hyphen")

doses = c("Nodose_p", "onedose_p",'twodose_p', "threedose_p")
names(doses) = c("0 doses (n=1,014)", "1 dose (n=333)", "2 doses (n=590)", "Completed series (n=2,960)")
333 + 590 + 2960
#provdpt<-
ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_dpt,  mapping=aes(fill=value_gf))+
  #scale_color_continuous(palette = "PuRd")+ # 9 of 12 colors
  scale_fill_manual(values = mygreens9of12)+
  theme_bw(base_size=14) + 
  labs(fill = "HBsAg prevalence", x='',y='')+ # legend title if wanted HBsAg infections per 100 children comparing\nReactive vs nonreactive tetanus antibodies
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.background = element_rect(fill="#daeff8", color=NA))+
    facet_wrap(~factor(variable, levels =c("Nodose_p", "anydose_p") , labels = c("0 doses (n=1,014)", "At least one dose (n=3,883)")), nrow = 1, ncol = 2)
#provdpt
ggsave('./Plots/provdpt_bin.png', width=12, height=6)

# pds for any vs no dpt doses----
table(provdptbin5tpd$pd_dpt)
view(provdptbin5tpd)
provdptbin5tpd <- provdptbin5t %>% select(c(hyphen,Nodose_p, anydose_p,pd_dpt)) %>% mutate(pd_g = case_when(
    pd_dpt < -10 ~ 0, #any doses 10+ fewer infections
    pd_dpt >= -10  & pd_dpt < -5 ~ 1, # any doses 5 fewer
    pd_dpt >= -5 & pd_dpt < -3 ~ 2, # any doses >3 fewer
    pd_dpt >= -3  & pd_dpt < -2 ~ 3, # any doses >2 fewer
    pd_dpt >= -2  & pd_dpt < -0.5 ~ 4, #any doses >1 fewer
    pd_dpt >= -0.5  & pd_dpt <= 0.5 & (Nodose_p == 0 & anydose_p == 0) ~ 5, # no diff AND PREV IS 0
    pd_dpt >= -0.5  & pd_dpt <= 0.5 & (Nodose_p != 0 | anydose_p != 0) ~ 6, # no diff AND PREV > 0
    pd_dpt > 0.5  & pd_dpt <= 2 ~ 7, # using 0.5 as rounding to 1
    pd_dpt > 2  & pd_dpt <= 3 ~8,
    pd_dpt > 3 ~ 9),
  pd_gf = factor(pd_g,
                 levels = c(0,1,2,3,4,5,6,7,8,9),
                 labels = c("No DPT doses >10 more", "No DPT doses 5 more", "No DPT doses 3 more", "No DPT doses 2 more", "No DPT doses 1 more", "No difference (prev 0% in both)", "No difference (prev >0%)", "Any DPT doses 1 more", "Any DPT doses 2 more", "Any DPT doses 3 more")))

drcprov_dptpd <- left_join( drcprov[,c("hyphen", "geometry")], provdptbin5tpd, by="hyphen")

n11colorpigr <- c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#f7f7f7", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419")
n11colorrdbl <- c('#93003a', '#c52a52', '#e75d6f', '#fd9291', '#ffcab9', '#ffffe0', '#b1dfdb', '#85b7ce', '#618fbf', '#3e67ae', '#00429d')
n10colorblrd <- c('#00429d', '#3e67ae', '#618fbf', '#85b7ce', '#b1dfdb', '#f7f7f7','#ffffe0', '#ffcab9', '#fd9291','#e75d6f')


dpt <-
  ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_dptpd,  mapping=aes(fill=pd_gf))+
  geom_sf_text(data= drcprov_dptpd[drcprov_dptpd$pd_g != 5 & drcprov_dptpd$pd_g != 6 & drcprov_dptpd$hyphen!="Kasai-Oriental" & drcprov_dptpd$hyphen!="Kasai-Central" & drcprov_dptpd$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 2, fontface = "bold") + 
  geom_sf_text(data= drcprov_dptpd[drcprov_dptpd$pd_g != 5 & drcprov_dptpd$pd_g != 6 & drcprov_dptpd$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = 0, size = 2, fontface = "bold") + 
  geom_sf_text(data= drcprov_dptpd[drcprov_dptpd$pd_g != 5 & drcprov_dptpd$pd_g != 6 & drcprov_dptpd$hyphen=="Kasai-Central",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .5, size = 2, fontface = "bold") + 
  geom_sf_text(data= drcprov_dptpd[drcprov_dptpd$pd_g != 5 & drcprov_dptpd$pd_g != 6 & drcprov_dptpd$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 2, fontface = "bold") + 
  scale_fill_manual(values = n10colorblrd)+
  theme_bw(base_size=14) + 
  labs(fill = "HBsAg infections per 100 children", x='',y='')+ # legend title if wanted HBsAg infections per 100 children comparing\nReactive vs nonreactive tetanus antibodies
  scale_x_continuous(limits=c(12,31)) + 
  scale_y_continuous(limits=c(-13.5,5.4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.background = element_rect(fill="#daeff8", color=NA))#+

ggsave('./Plots/provdptany_pd.png', width=12, height=6)


# combine from 06_mapping.R
A5 + prov5 + provtet + plot_layout(nrow=3, ncol = 1) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps3.png', width=6, height=10)

A5 + provtet + prov5 +  dpt + plot_layout(nrow=2, ncol = 2) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps4box.png', width=16, height=12)

# tet ab vs dpt
dptvsab <- svyby(~dpt_doses_f,~tetab, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
view(dptvsab)
elig_kids_whbvres_wt_kr %>% filter(tetab != "Indeterminate") %>% group_by(tetab, dpt_doses_f ) %>% summarise(n=n(), count = n() / nrow(.) )
table(elig_kids_whbvres_wt_kr$dpt_count, elig_kids_whbvres_wt_kr$tetab, useNA = "always")
table(elig_kids_whbvres_wt_kr$dpt_count, elig_kids_whbvres_wt_kr$hbvresult5, useNA = "always")

# maps for other cutoffs--------
svyby(~prov2015,~hbvresult1, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% clipr::write_clip()
svyby(~prov2015,~hbvresult2, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% clipr::write_clip()
svyby(~prov2015,~hbvresult100, designf_dhs2, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% clipr::write_clip()

