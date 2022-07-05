
#==========  MASTER ANALYSIS  ==========



#============= LOAD PACKS ==============  


library(pacman)
library(Ostats)

p_load(formattable,knitr,kableExtra, # ncie tables
       tidyverse,vegan,lubridate,gridExtra,ggrepel,reshape2,ggpmisc,BBmisc,stringr,
       circlize, # interaction networks
       Distance, # transect analysis, relative abundance, density
       readxl,writexl)


#============ IMPORT DATA ==============  


## TRANSECT----
Transect <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Composition")
Transect<-Transect %>% filter(Study.Area!='Palawan Beach')

## COMPOSITION----
Composition <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                          sheet = "Composition")
Composition<-Composition %>% filter(Study.Area!='Palawan Beach')

## INTERACTIONS
Interact <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Interactions")
Interact <- Interact %>% filter(Study.Area!='Palawan Beach')

## NICHE OVERLAP----
NO <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                 sheet = "Composition")
NO<-NO %>% filter(Study.Area!='Palawan Beach')



#========== TRANSECT ANALYSIS ========== 


# CALCULATIONS FOR ~~RELATIVE ABUNDANCE~~
# EVERY SITE AND SPECIES
# HALF NORMAL (HN) DETECTION MODEL WORKS BEST OVERALL

# check effort per site
Transect %>% group_by(Study.Area) %>% summarise(max(Surveyno))
# Changi Village                 10
# Pasir Ris Town Park            12
# Sengkang Riverside Park        7
# Springleaf                     10
# Stirling Road                  8

convunit <- convert_units("meter", "kilometer", "hectare")

## Changi----
Changi<-Transect %>% filter(Study.Area=='Changi Village')
Changi$Effort<-Changi$Effort*10
Changi<-Changi %>% select(Region.Label,Area,Sample.Label,Effort,distance,Species,Surveyno) %>% 
  rename(species=Species) %>% rename(visit=Surveyno)
Changi.birds <- ds(data = Changi,
                   key="hn", convert_units = convunit,
                   formula=~species, truncation = 70)
Changi.ests <- dht2(ddf=Changi.birds, flatfile=Changi,
                    strat_formula = ~species, convert_units = convunit,
                    stratification = "object") 
changiDAB<-Changi.ests %>% 
  select(Area,df_var,species,n,p_var,p_average,ER,Abundance,Abundance_se,Abundance_CV,bigC,LCI,UCI) %>% 
  arrange(desc(Abundance)) %>% 
  mutate(Study.Area='Changi Village',.before=1)
# generate density results https://rdrr.io/cran/Distance/src/R/dht2.R
changiDAB<-changiDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)

## Pasir Ris----
Pasir<-Transect %>% filter(Study.Area=='Pasir Ris Town Park')
Pasir$Effort<-Pasir$Effort*12
Pasir<-Pasir %>% select(Region.Label,Area,Sample.Label,Effort,distance,Species,Surveyno) %>% 
  rename(species=Species) %>% rename(visit=Surveyno)
Pasir.birds <- ds(data = Pasir,
                  key="hn", convert_units = convunit,
                  formula=~species, truncation = 70)
Pasir.ests <- dht2(ddf=Pasir.birds, flatfile=Pasir,
                   strat_formula = ~species, convert_units = convunit,
                   stratification = "object") 
PasirDAB<-Pasir.ests %>% 
  select(Area,df_var,species,n,p_var,p_average,ER,Abundance,Abundance_se,Abundance_CV,bigC,LCI,UCI) %>% 
  arrange(desc(Abundance)) %>% 
  mutate(Study.Area='Pasir Ris Town Park',.before=1)
# generate density results 
PasirDAB<-PasirDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)

## Springleaf----
Springleaf<-Transect %>% filter(Study.Area=='Springleaf')
Springleaf$Effort<-Springleaf$Effort*10
Springleaf<-Springleaf %>% select(Region.Label,Area,Sample.Label,Effort,distance,Species,Surveyno) %>% 
  rename(species=Species) %>% rename(visit=Surveyno)
Springleaf.birds <- ds(data = Springleaf,
                       key="hn", convert_units = convunit,
                       formula=~species, truncation = 70)
Springleaf.ests <- dht2(ddf=Springleaf.birds, flatfile=Springleaf,
                        strat_formula = ~species, convert_units = convunit,
                        stratification = "object") 
SpringleafDAB<-Springleaf.ests %>% 
  select(Area,df_var,species,n,p_var,p_average,ER,Abundance,Abundance_se,Abundance_CV,bigC,LCI,UCI) %>% 
  arrange(desc(Abundance)) %>% 
  mutate(Study.Area='Springleaf',.before=1)
# generate density results 
SpringleafDAB<-SpringleafDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)

## Sengkang----
Sengkang<-Transect %>% filter(Study.Area=='Sengkang Riverside Park')
Sengkang$Effort<-Sengkang$Effort*7
Sengkang<-Sengkang %>% select(Region.Label,Area,Sample.Label,Effort,distance,Species,Surveyno) %>% 
  rename(species=Species) %>% rename(visit=Surveyno)
Sengkang.birds <- ds(data = Sengkang,
                     key="hn", convert_units = convunit,
                     formula=~species, truncation = 70)
Sengkang.ests <- dht2(ddf=Sengkang.birds, flatfile=Sengkang,
                      strat_formula = ~species, convert_units = convunit,
                      stratification = "object") 
SengkangDAB<-Sengkang.ests %>% 
  select(Area,df_var,species,n,p_var,p_average,ER,Abundance,Abundance_se,Abundance_CV,bigC,LCI,UCI) %>% 
  arrange(desc(Abundance)) %>% 
  mutate(Study.Area='Sengkang Riverside Park',.before=1)
# generate density results 
SengkangDAB<-SengkangDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)

## Stirling/Queenstown----
Stirling<-Transect %>% filter(Study.Area=='Stirling Road')
Stirling$Effort<-Stirling$Effort*8
Stirling<-Stirling %>% select(Region.Label,Area,Sample.Label,Effort,distance,Species,Surveyno) %>% 
  rename(species=Species) %>% rename(visit=Surveyno)
Stirling.birds <- ds(data = Stirling,
                     key="hn", convert_units = convunit,
                     formula=~species, truncation = 70)
Stirling.ests <- dht2(ddf=Stirling.birds, flatfile=Stirling,
                      strat_formula = ~species, convert_units = convunit,
                      stratification = "object") 
StirlingDAB<-Stirling.ests %>% 
  select(Area,df_var,species,n,p_var,p_average,ER,Abundance,Abundance_se,Abundance_CV,bigC,LCI,UCI) %>% 
  arrange(desc(Abundance)) %>% 
  mutate(Study.Area='Stirling Road',.before=1)
# generate density results 
StirlingDAB<-StirlingDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)

## GOF plots----
par(mfrow=c(3,2))
gof_ds(Changi.birds)
title(main = "GOF CHANGI")
gof_ds(Pasir.birds)
title(main = "GOF PASIR RIS")
gof_ds(Springleaf.birds)
title(main = "GOF SPRINGLEAF")
gof_ds(Sengkang.birds)
title(main = "GOF SENGKANG")
gof_ds(Stirling.birds)
title(main = "GOF STIRLING RD")
mtext("Goodness of fit - Distance models", side = 3, line = -2, outer = TRUE)

## Merge----
Transect_2 <- rbind(changiDAB,PasirDAB,SpringleafDAB,StirlingDAB,SengkangDAB)
Transect_2 <- Transect_2 %>% filter(species!='Total') %>% rename(Species=species)
view(Transect_2)

### clean up----
# leave .birds objects in the environment because they take a while to re-run
rm(list = c('Changi','Changi.ests','Pasir','Pasir.ests','Springleaf','Springleaf.ests',
            'Stirling','Stirling.ests','Sengkang','Sengkang.ests'))


#==== MERGE TRANSECT_2~COMPOSITION ====


Composition_2<-merge(Composition,Transect_2,by=c('Study.Area','Species'),all=T)
View(Composition_2)

# Transect & Transect_2 are not needed in analysis from this point on



#=========== BASIC DATA PREP ===========


## Factorise Composition_2----
str(Composition_2)
cols_comp <-c('Study.Area','Species','Region.Label','Sci_name','SG_status',
              'IUCN_status','NestType','sp_lab','TrophicLevel',
              'TrophicNiche','ForagingNiche')
Composition_2<-Composition_2 %>% mutate_at(cols_comp, factor)
str(Composition_2)

## Factorise Interact----
str(Interact)
cols_int <-c('Region.Label','Study.Area','ampm','initsp','recipsp','interaction',
             'isout','rsout','treeid','treesci','treecom','trim','at_cav')
Interact_2<-Interact %>% mutate_at(cols_int,factor)  
str(Interact_2)  

## Interaction ratings----
Interact_2<-Interact_2 %>% 
  mutate(rating=case_when(
    interaction=="Neutral"~"0", # no aggression
    interaction=="Displace"~"1", # remove from perch by landing
    interaction=="Threat"~"2", # vocal or flaring threats
    interaction=="Swoop"~"3", # fly within 60cm at species but not landing
    interaction=="Chase"~"4", # in-flight pursuit may or may not displace
    interaction=="Contact"~"5", # physical contact
    interaction=="Fight"~"6")) # multiple physical contact
Interact_2$rating<-as.numeric(Interact_2$rating)
Interact_2$interaction<-factor(Interact_2$interaction,
                             levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact","Fight"))
levels(Interact_2$interaction)



#====== ALPHA BD INDICES ======


# Testing 3 possibilities 
## Count max----
Comp.max<-Composition_2 %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% 
  summarise(max_obs = max(n)) %>% 
  arrange(Study.Area,desc(max_obs))

### spread/gather----
Comp.alpha<-Comp.max %>% spread(key=Species,value = max_obs) %>% 
  replace(is.na(.), 0) %>% remove_rownames %>% 
  column_to_rownames(var="Study.Area")
#view(Comp.alpha)

### Richness----
fun.1<-function(x){sum(x>0)}
richness<-apply(Comp.alpha, 1, FUN=fun.1)
richness<-data.frame(richness)
colnames(richness)<-"Richness.max"
#view(richness)

### Shannon index----
for (Comp.alpha.row in 1:5)
{shannon<- matrix(diversity(Comp.alpha[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(Comp.alpha)
colnames(shannon)<-"Shannon.max"
#view(shannon)

### Simpson index----
for (Comp.alpha.row in 1:5)
{simpson<- matrix(diversity(Comp.alpha[,], index = "simpson"))}
simpson<-round(simpson,3)
#Adjusting the names of rows and columns
row.names(simpson)<-row.names(Comp.alpha)
colnames(simpson)<-"Simpson.max"
#view(simpson)

# Putting together all indices
Indices.max<-cbind(richness, shannon, simpson)
Indices.max<-data.frame(Indices.max)

## Count all----
Comp.all<-Composition_2 %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% 
  summarise(all_obs=sum(n))
## Count mean----
Comp.mean<-Composition_2 %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% 
  summarise(mean_obs=mean(n))



# Plot indices----
plot.indices.mean<-Indices %>% 
  ggplot(aes(x=Simpson,y=Shannon,
             label=row.names(Indices))) +
  geom_point(aes(color=Richness), size=4)+
  geom_text(hjust=0.7,vjust=-1.2)+
  labs(title = 'Alpha biodiversity of each survey site (mean obs)')

grid.arrange(plot.indices.all,plot.indices.max,plot.indices.mean,ncol=3)
plot.indices.max
plot.indices.all
plot.indices.mean


# table
formattable(Indices) 




