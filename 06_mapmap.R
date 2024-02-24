# Mapping extra_rev
library(janitor)
library(tidyverse)
library(sf)
library(srvyr)
library(survey)
library(patchwork)
options(survey.lonely.psu="adjust")

designf <-svydesign(ids=elig_kids_whbvres_wt_kr$hv001, strata=elig_kids_whbvres_wt_kr$hv022 , weights=elig_kids_whbvres_wt_kr$both_wt_new,  data=elig_kids_whbvres_wt_kr)
designf_dhs2 <-as_survey_design(designf)


# use kidsmap_impsf

# by age and sex
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

my_greens4 = c("#EAF7E6","#BBE3B4","#41ae76", "#238b45","#00441b") #there are 9, I exluded the two lighter hues

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

mapagerev <- 
  ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_age,  mapping=aes(fill=value_gf))+
  #scale_fill_distiller(palette = 'Greens', direction = 1, limits = c(0, 25)) + #  YlGn
  scale_fill_manual(values = my_greens)+
  geom_sf_text(data= drcprov_age[drcprov_age$value_g >0 & drcprov_age$hyphen!="Kasai-Oriental" & drcprov_age$hyphen!="Kasai-Central" & drcprov_age$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 1.5, fontface = "bold") + 
  #geom_sf_text(data= drcprov_age[drcprov_age$value_g >0 ,], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 3) + #, fontface = "bold"
  geom_sf_text(data= drcprov_age[drcprov_age$value_g >0 & drcprov_age$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 1.5, fontface = "bold") + 
  geom_sf_text(data= drcprov_age[drcprov_age$value_g >0 & drcprov_age$hyphen=="Kasai-Central",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.3, size = 1.5, fontface = "bold") + 
  geom_sf_text(data= drcprov_age[drcprov_age$value_g >0 &drcprov_age$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 1.5, fontface = "bold") + 
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
  )+ #legend.title = element_blank()
  #facet_grid(~label)
  facet_wrap(~reorder(age, biryr), ncol = 5, nrow=1)
mapagerev


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
colnames(provtet5t)

summary(provtet5t$pdtet)
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
    pdtet >= -1  & pdtet <= 1 ~ 5,
    pdtet > 1  & pdtet <= 2 ~ 6,
    pdtet > 2  ~ 7),
  pdtet_gf = factor(pdtet_g,
                    levels = c(0,1,2,3,4,5,6,7),
                    labels = c("Reactive Ab 5+ fewer infections", "Reactive Ab >4-5 fewer",  "Reactive Ab >3-4 fewer", "Reactive Ab >2-3 fewer", "Reactive Ab >1-2 fewer"  ,"No difference", "Reactive Ab >1-2 more", "Reactive Ab >2 more")))

view(provtet5t)

n11colorpigr <- c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#f7f7f7", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419")
n11colorrdbl <- c('#93003a', '#c52a52', '#e75d6f', '#fd9291', '#ffcab9', '#ffffe0', '#b1dfdb', '#85b7ce', '#618fbf', '#3e67ae', '#00429d')
n11colorblrd <- c('#00429d', '#3e67ae', '#618fbf', '#85b7ce', '#b1dfdb', '#ffffe0', '#ffcab9', '#fd9291','#e75d6f',  '#c52a52', '#93003a')


provtet5t$hyphen <- gsub("prov2015", "", provtet5t$level)
drcprov_tet <- left_join( drcprov[,c("hyphen", "geometry")], provtet5t, by="hyphen")
library(patchwork)
provtet<-
  ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_tet,  mapping=aes(fill=pdtet_gf))+
  geom_sf_text(data= drcprov_tet[drcprov_tet$pdtet_g != 5 & drcprov_tet$hyphen!="Kasai-Oriental" & drcprov_tet$hyphen!="Kasai-Central" & drcprov_tet$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 2, fontface = "bold") + 
  geom_sf_text(data= drcprov_tet[drcprov_tet$pdtet_g != 5 & drcprov_tet$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 2, fontface = "bold") + 
  geom_sf_text(data= drcprov_tet[drcprov_tet$pdtet_g != 5 & drcprov_tet$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 2, fontface = "bold") + 
  scale_fill_manual(values = n11colorblrd)+
  theme_bw(base_size=14) + 
  labs(fill = "", x='',y='')+ # legend title if wanted HBsAg infections per 100 children comparing\nReactive vs nonreactive tetanus antibodies
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
provtet
ggsave('./Plots/provtet_feb.png', width=12, height=6)

# combine from 06_mapping.R
A5 + prov5 + provtet + plot_layout(nrow=1, ncol = 3) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps3.png', width=12, height=4)

A5 + prov5 + provtet + plot_layout(nrow=3, ncol = 1) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps3v.png', width=6, height=12)

## DPT vacc by province---------
provdpt5 <- svyby(~prov2015,~hbvresult5+dpt_count, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
provdpt5 <- provdpt5 %>% mutate(level = paste0('hbv', level))
provdpt5t <- as.data.frame(t(as.data.frame(provdpt5))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))

provdpt5t <- provdpt5t %>% mutate(across(-c(level), as.numeric))
view(provdpt5t)

provdpt5t <- provdpt5t %>% mutate(
  Nonreactive_p = 100*(hbv1.Nonreactive/(hbv1.Nonreactive + hbv0.Nonreactive)),
  Reactive_p = 100*(hbv1.Reactive/(hbv1.Reactive + hbv0.Reactive)),
  pdtet = Reactive_p  - Nonreactive_p,
  pdtet_g = case_when(
    pdtet < -5 ~ 0, #Reactive Ab 5+ fewer infections
    pdtet >= -5  & pdtet < -4 ~ 1, # Reactive Ab >4-5 fewer
    pdtet >= -4 & pdtet < -3 ~ 2, # Reactive Ab >3-4 fewer
    pdtet >= -3  & pdtet < -2 ~ 3, # Reactive Ab >2-3 fewer
    pdtet >= -2  & pdtet < -1 ~ 4, #Reactive Ab >1-2 fewer
    pdtet >= -1  & pdtet <= 1 ~ 5,
    pdtet > 1  & pdtet <= 2 ~ 6,
    pdtet > 2  ~ 7),
  pdtet_gf = factor(pdtet_g,
                    levels = c(0,1,2,3,4,5,6,7),
                    labels = c("Reactive Ab 5+ fewer infections", "Reactive Ab >4-5 fewer",  "Reactive Ab >3-4 fewer", "Reactive Ab >2-3 fewer", "Reactive Ab >1-2 fewer"  ,"No difference", "Reactive Ab >1-2 more", "Reactive Ab >2 more")))

view(provtet5t)

n11colorpigr <- c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#f7f7f7", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419")
n11colorrdbl <- c('#93003a', '#c52a52', '#e75d6f', '#fd9291', '#ffcab9', '#ffffe0', '#b1dfdb', '#85b7ce', '#618fbf', '#3e67ae', '#00429d')
n11colorblrd <- c('#00429d', '#3e67ae', '#618fbf', '#85b7ce', '#b1dfdb', '#ffffe0', '#ffcab9', '#fd9291','#e75d6f',  '#c52a52', '#93003a')


provtet5t$hyphen <- gsub("prov2015", "", provtet5t$level)
drcprov_tet <- left_join( drcprov[,c("hyphen", "geometry")], provtet5t, by="hyphen")
library(patchwork)
provtet<-
  ggplot()+
  geom_sf(data=admin0, fill="snow3", color="snow4") +
  geom_sf(data=drcprov_tet,  mapping=aes(fill=pdtet_gf))+
  geom_sf_text(data= drcprov_tet[drcprov_tet$pdtet_g != 5 & drcprov_tet$hyphen!="Kasai-Oriental" & drcprov_tet$hyphen!="Kasai-Central" & drcprov_tet$hyphen!="Lomami" ,], 
               mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.1, nudge_y = -.1, size = 2, fontface = "bold") + 
  geom_sf_text(data= drcprov_tet[drcprov_tet$pdtet_g != 5 & drcprov_tet$hyphen=="Kasai-Oriental",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = -0.025, nudge_y = .3, size = 2, fontface = "bold") + 
  geom_sf_text(data= drcprov_tet[drcprov_tet$pdtet_g != 5 & drcprov_tet$hyphen=="Lomami",], mapping=aes(label = hyphen, geometry = geometry), nudge_x = 0.5, nudge_y = 1, size = 2, fontface = "bold") + 
  scale_fill_manual(values = n11colorblrd)+
  theme_bw(base_size=14) + 
  labs(fill = "", x='',y='')+ # legend title if wanted HBsAg infections per 100 children comparing\nReactive vs nonreactive tetanus antibodies
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
provtet
ggsave('./Plots/provtet_feb.png', width=12, height=6)

# combine from 06_mapping.R
A5 + prov5 + provtet + plot_layout(nrow=1, ncol = 3) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps3.png', width=12, height=4)

A5 + prov5 + provtet + plot_layout(nrow=3, ncol = 1) + plot_annotation(tag_levels = 'A')
ggsave('./Plots/main_maps3v.png', width=6, height=12)

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

