
#/////////////////////////////////////////////////////////////////////////////#
#============================  MASTER ANALYSIS   =============================
#/////////////////////////////////////////////////////////////////////////////#


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================= 1. LOAD PACKS ==================================  
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#


library(pacman)
library(Ostats)

p_load(formattable,knitr,kableExtra, # ncie tables
       tidyverse,vegan,lubridate,gridExtra,ggrepel,reshape2,ggpmisc,BBmisc,stringr,
       ggpubr,AICcmodavg, #anova
       circlize, # interaction networks
       Distance, # transect analysis, relative abundance, density
       readxl,writexl)



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================== 2. IMPORT DATA ================================  
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

# Transect
Transect <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Composition")
Transect<-Transect %>% filter(Study.Area!='Palawan Beach')

# Composition
Composition <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                          sheet = "Composition")
Composition<-Composition %>% filter(Study.Area!='Palawan Beach')

# Interactions
Interact <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Interactions")
Interact <- Interact %>% filter(Study.Area!='Palawan Beach')

# Niche overlap
NO <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                 sheet = "Composition")
NO<-NO %>% filter(Study.Area!='Palawan Beach')

# Environment

Enviro <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                     sheet = "Enviro")
Enviro<-Enviro %>% filter(Study.Area!='Palawan Beach')



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================ 3. TRANSECT ANALYSIS ============================ 
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
  
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


#========#
# Changi
#========#

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


#===========#
# Pasir Ris
#===========#

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
PasirDAB<-PasirDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)


#============#
# Springleaf
#============#

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
SpringleafDAB<-SpringleafDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)


#==========#
# Sengkang
#==========#

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
SengkangDAB<-SengkangDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)


#=====================#
# Stirling/Queenstown
#=====================#

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
StirlingDAB<-StirlingDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)


#===================#
# 3.a. GOF plots----
#===================#

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

#===============#
# 3.b. Merge----
#===============#

Transect_2 <- rbind(changiDAB,PasirDAB,SpringleafDAB,StirlingDAB,SengkangDAB)
Transect_2 <- Transect_2 %>% filter(species!='Total') %>% rename(Species=species)
view(Transect_2)

#==================#
# 3.c. Clean up----
#==================#
# leave .birds objects in the environment because they take a while to re-run
rm(list = c('Changi','Changi.ests','Pasir','Pasir.ests','Springleaf','Springleaf.ests',
            'Stirling','Stirling.ests','Sengkang','Sengkang.ests'))



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#===================== 4. MERGE TRANSECT_2 ~ COMPOSITION ======================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

Composition_2<-merge(Composition,Transect_2,by=c('Study.Area','Species'),all=T)
Composition_2<-Composition_2 %>% select(-Effort,-Area.x,-Area.y,-Weather,-Comments,
                                        -Pref_hab,-TrophicLevel,-Sci_name,-Sample.Label,
                                        -Start_time,-End_time,-p_var,-p_average,
                                        -df_var,-df_var2)

# Transect & Transect_2 are not needed in analysis from this point on



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#=========================== 5. BASIC DATA PREP ==============================
#/////////////////////////////////////////////////////////////////////////////#

# Factorise Composition_2
str(Composition_2)
cols_comp <-c('Study.Area','Species','Region.Label','SG_status',
              'IUCN_status','NestType','sp_lab',
              'TrophicNiche','ForagingNiche')
Composition_2<-Composition_2 %>% mutate_at(cols_comp, factor)
str(Composition_2)
rm(cols_comp)

# Factorise Interact
str(Interact)
cols_int <-c('Region.Label','Study.Area','ampm','initsp','recipsp','interaction',
             'isout','rsout','treeid','treesci','treecom','trim','at_cav')
Interact_2<-Interact %>% mutate_at(cols_int,factor)  
str(Interact_2)  
rm(cols_int)

# Interaction ratings
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



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================ 6. ALPHA BD INDICES ============================= 
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

# Testing 3 possibilities~

#===================#
# 6.a. Count max----
#===================#

Comp.max<-Composition_2 %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% 
  summarise(max_obs = max(n)) %>% 
  arrange(Study.Area,desc(max_obs))

# spread/gather
Comp.alpha<-Comp.max %>% spread(key=Species,value = max_obs) %>% 
  replace(is.na(.), 0) %>% remove_rownames %>% 
  column_to_rownames(var="Study.Area")

# Richness.max
fun.1<-function(x){sum(x>0)}
richness<-apply(Comp.alpha, 1, FUN=fun.1)
richness<-data.frame(richness)
colnames(richness)<-"Richness.max"

# Shanno.max
for (Comp.alpha.row in 1:5)
{shannon<- matrix(diversity(Comp.alpha[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(Comp.alpha)
colnames(shannon)<-"Shannon.max"

# Simpson.max
for (Comp.alpha.row in 1:5)
{simpson<- matrix(diversity(Comp.alpha[,], index = "simpson"))}
simpson<-round(simpson,3)
#Adjusting the names of rows and columns
row.names(simpson)<-row.names(Comp.alpha)
colnames(simpson)<-"Simpson.max"

# Putting together all indices.max
Indices.max<-cbind(richness, shannon, simpson)
Indices.max<-data.frame(Indices.max)

# plot.all
plot.indices.max<-Indices.max %>% 
  ggplot(aes(x=Simpson.max,y=Shannon.max,
             label=row.names(Indices.max))) +
  geom_point(aes(color=Richness.max), size=4)+
  geom_text(hjust=0.7,vjust=-1.2)+
  labs(title = 'Alpha biodiversity of each survey site (max obs)')


#===================#
# 6.b. Count all----
#===================#

Comp.all<-Composition_2 %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% 
  summarise(all_obs=sum(n))

# spread/gather
Comp.alpha<-Comp.all %>% spread(key=Species,value = all_obs) %>% 
  replace(is.na(.), 0) %>% remove_rownames %>% 
  column_to_rownames(var="Study.Area")

# Richness.all
fun.1<-function(x){sum(x>0)}
richness<-apply(Comp.alpha, 1, FUN=fun.1)
richness<-data.frame(richness)
colnames(richness)<-"Richness.all"

# Shannon.all
for (Comp.alpha.row in 1:5)
{shannon<- matrix(diversity(Comp.alpha[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(Comp.alpha)
colnames(shannon)<-"Shannon.all"

# Simpson.all
for (Comp.alpha.row in 1:5)
{simpson<- matrix(diversity(Comp.alpha[,], index = "simpson"))}
simpson<-round(simpson,3)
#Adjusting the names of rows and columns
row.names(simpson)<-row.names(Comp.alpha)
colnames(simpson)<-"Simpson.all"

# Putting together all indices.all
Indices.all<-cbind(richness, shannon, simpson)
Indices.all<-data.frame(Indices.all)

# plot.all
plot.indices.all<-Indices.all %>% 
  ggplot(aes(x=Simpson.all,y=Shannon.all,
             label=row.names(Indices.all))) +
  geom_point(aes(color=Richness.all), size=4)+
  geom_text(hjust=0.7,vjust=-1.2)+
  labs(title = 'Alpha biodiversity of each survey site (all obs)')


#====================#
# 6.c. Count mean----
#====================#

Comp.mean<-Composition_2 %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% 
  summarise(mean_obs=mean(n))

# spread/gather
Comp.alpha<-Comp.mean %>% spread(key=Species,value = mean_obs) %>% 
  replace(is.na(.), 0) %>% remove_rownames %>% 
  column_to_rownames(var="Study.Area")

# Richness.mean
fun.1<-function(x){sum(x>0)}
richness<-apply(Comp.alpha, 1, FUN=fun.1)
richness<-data.frame(richness)
colnames(richness)<-"Richness.mean"

# Shannon.mean
for (Comp.alpha.row in 1:5)
{shannon<- matrix(diversity(Comp.alpha[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(Comp.alpha)
colnames(shannon)<-"Shannon.mean"

# Simpson.mean
for (Comp.alpha.row in 1:5)
{simpson<- matrix(diversity(Comp.alpha[,], index = "simpson"))}
simpson<-round(simpson,3)
#Adjusting the names of rows and columns
row.names(simpson)<-row.names(Comp.alpha)
colnames(simpson)<-"Simpson.mean"

# Putting together all indices.mean
Indices.mean<-cbind(richness, shannon, simpson)
Indices.mean<-data.frame(Indices.mean)

#plot.mean
plot.indices.mean<-Indices.mean %>% 
  ggplot(aes(x=Simpson.mean,y=Shannon.mean,
             label=row.names(Indices.mean))) +
  geom_point(aes(color=Richness.mean), size=4)+
  geom_text(hjust=0.7,vjust=-1.2)+
  labs(title = 'Alpha biodiversity of each survey site (mean obs)')


#==============================#
# 6.d. Merge all variations----
#==============================#

Indices<-cbind(Indices.all, Indices.mean, Indices.max)
Indices <- rownames_to_column(Indices, "Study.Area")
view(Indices)


#=================#
# 6.e. Plot indices----
#=================#

grid.arrange(plot.indices.all,plot.indices.max,plot.indices.mean,ncol=3)
# table
formattable(Indices) 


#=============#
# Clean up----
#=============#

rm(list = c('Indices.max','Indices.mean','Indices.all',
            'Comp.max','Comp.mean','Comp.all','Comp.alpha','simpson','shannon',
            'Comp.alpha.row'))



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#===================== 7. MERGE: INDICES ~ COMPOSITION =======================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

Composition_2<-merge(Composition_2,Indices,by='Study.Area')
View(Composition_2)



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#========================= 8. DATA TRANSFORMATION ============================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

#=====================================#
# 8.a. Transformations of: COMPOSITION_2----
#=====================================#

# add max daily counts and maximum proportion
x<-Composition_2 %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% 
  summarise(max_obs = max(n)) %>% 
  arrange(Study.Area,desc(max_obs)) %>% 
  mutate(max.proportion=max_obs/sum(max_obs)*100) %>% 
  arrange(Study.Area,desc(max.proportion))

Composition_2<-merge(Composition_2,x,by=c('Study.Area','Species'),all=T)

# add total counts and maximum proportion
y<-Composition_2 %>% filter(Species=='Asian glossy starling') %>% 
  group_by(Study.Area,Species) %>% 
  tally() %>% 
  mutate(total.proportion=n/sum(n)*100) %>% 
  arrange(Study.Area,desc(total.proportion))
y<-y %>% rename(total_obs=n)

Composition_2<-merge(Composition_2,y,by=c('Study.Area','Species'),all=T)


#========================#
# 8.b. ISRS Long Transform----
#========================#

# IS
IS<-Interact_2 %>% 
  filter(recipsp!="NA") %>% 
  group_by(Study.Area,initsp,interaction,rating,isout,) %>% 
  tally()
IS<-rename(IS,species=initsp)  
IS<-rename(IS,outcome=isout)
IS$role<-'IS' # identify IS/RS

# RS
RS<-Interact_2 %>% 
  filter(recipsp!="NA") %>% 
  group_by(Study.Area,recipsp,interaction,rating,rsout) %>% 
  tally()
RS<-rename(RS,species=recipsp)  
RS<-rename(RS,outcome=rsout)
RS$role<-'RS' # identify IS/RS

# Combine
ISRS<-rbind(IS,RS)
ISRS<-ISRS %>% relocate(1,2,3,4,5,7,6) 
ISRS<-ISRS %>% rename(n_ints=n)
ISRS<-ISRS %>% rename(Species=species)
view(ISRS)




#=================#
# 8.c. Tibbles----
#=================#
rm(Ints.Abundance)
x<-ISRS %>% 
  group_by(Study.Area,Species) %>% summarise(n_ints=sum(n_ints))

y<-Composition_2 %>% 
  group_by(Study.Area,Species) %>%summarise(Abundance=mean(Abundance)) %>%
  arrange(Study.Area,desc(Abundance))

Ints.Abundance <- merge(x,y,by=c("Study.Area","Species")) %>% 
  arrange(Study.Area,desc(Abundance))

z<-Composition_2 %>% 
  group_by(Study.Area,Species) %>%summarise(max_obs=max(max_obs)) %>%
  arrange(Study.Area,desc(max_obs)) %>% 
  mutate(proportion=max_obs/sum(max_obs)*100)

Ints.Abundance <- merge(Ints.Abundance,z,by=c("Study.Area","Species")) 

# Trans. ~INDICES

# Environment
envpc<-Enviro %>% 
  select(Study.Area,canopypc,Vegpc,buildpc,surfacepc,waterpc)
Indices_2<-merge(Indices,envpc,by='Study.Area')

# Parrot proportion
x<-Composition_2 %>% 
  filter(Species=="Monk parakeet"|Species=="Red-breasted parakeet"|Species=="Tanimbar corella"|
           Species=="Long-tailed parakeet"|Species=="Rose-ringed parakeet") %>% 
  group_by(Study.Area,Species) %>% 
  summarise(n=mean(max.proportion)) %>% 
  group_by(Study.Area) %>% 
  summarise(parrotprop=sum(n))
Indices_2<-merge(Indices_2,x,by='Study.Area')

# Cavity nester proportion
x<-Composition_2 %>% 
  filter(NestType=='Cavity') %>% 
  group_by(Study.Area,Species) %>% 
  summarise(n=mean(max.proportion)) %>% 
  group_by(Study.Area) %>% 
  summarise(cavnesterprop=sum(n))
Indices_2<-merge(Indices_2,x,by='Study.Area')

#To ISRS

# Initiations
# Use ~IS~ ONLY otherwise 2x count
x<-ISRS %>% 
  filter(role=='IS') %>% group_by(Study.Area) %>% 
  summarise(allIS=sum(n_ints))
Indices_2 <- merge(Indices_2,x,by='Study.Area')

# average aggression rating by site W/O NE
x<-ISRS %>% filter(interaction!='Neutral') %>% 
  group_by(Study.Area) %>% 
  summarise(avgrating=mean(rating))
Indices_2 <- merge(Indices_2,x,by='Study.Area')

# same but WITH NE
x<-ISRS %>% 
  group_by(Study.Area) %>% 
  summarise(avgratingNE=mean(rating))
Indices_2 <- merge(Indices_2,x,by='Study.Area')

# Master
# + abundance, max count, max prop, total count, total prop
x<-Composition_2 %>% select(Study.Area,Species,max_obs,max.proportion,
                            total_obs,total.proportion,sp_lab,Avg_size,NestType,
                            CavityYN,Abundance,Richness.all,Richness.max,Shannon.all,
                            Shannon.max,Simpson.all,Simpson.max) 

Master<-merge(ISRS,x,by=c('Study.Area','Species'),all=T)
dim(Master)

Master<-Master %>% distinct(Study.Area,Species,interaction,outcome,
                            role,n_ints, .keep_all = TRUE)



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#=========================== 9.0 DATA EXPLORATION ============================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

Composition_2 %>% select(Species) %>% n_distinct()
# 81 species observed in total [comp surveys]

Interact_2 %>% count(initsp) %>% summarise(sum(n))
# 752 total interactions - Sentosa excluded

Interact_2 %>% summarise(n_distinct(initsp)) 
# 30 species observed initiating interactions

Interact_2 %>% summarise(n_distinct(recipsp)) 
# 44 species observed receiving interactions

# Interaction pairs
int_pairs <- Interact_2 %>%
  count(initsp, recipsp) %>%
  complete(initsp, nesting(recipsp), fill = list(n = 0)) %>% 
  filter(n!='0') %>% 
  arrange(desc(n))
int_pairs %>% print(n=20) # top 10 interaciton pairs

## major interactions are limited, primaryy targets of NNP's are
  # Javan myna, rock dove, LTP, hornbill, starling

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*****
#!!! Species density / overcrowding could be a driver of interactions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*****

#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#=============================== 10.0 CHARTS =================================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#


# clean wrapped labels!!!!
ISRS$Species2 = str_wrap(ISRS$Species, width = 10)



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================== 10.1 TOP-LINE CORRELATIONS =========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

# i Richness x parrot prop
richxprop <-Indices_2 %>% 
  ggplot(aes(parrotprop,Richness.max))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Parrot proportion in the community positively correlates with overall species Richness',
       y='Species richness',x='Proportion of parrots in the community')
#R2 = 0.85
## 85% variation in Richness is attributed to parrot abundance
y<-lm(Richness.mean~parrotprop,Indices_2)
#summary(y)

shannxprop <- Indices_2 %>% 
  ggplot(aes(parrotprop,Shannon.mean))+ # mean, max, avg all diff
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Parrot proportion in the community positively correlates with Alpha Biodoversity',
       y='Shannon biodiversity',x='Proportion of parrots in the community')
#R2 = 0.75
## 75% variation in biodoversity (shann) is attributed to parrot abundance
y1<-lm(Shannon.mean~parrotprop,Indices_2)
#summary(y1)
# plot
grid.arrange(richxprop,shannxprop,ncol=2)

## Tanimbar corella may have a negative effect on Richness and BD within an area
## Stirling RD & CV are lowest on Rich&BD

# ii initiated interactions
# parrot prop in community != more interactions
isxprop<-Indices_2 %>% 
  ggplot(aes(parrotprop,allIS))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Greater proportion of parrots does not predict greater interaction frequency',
       y='n interactions',x='Proportion of parrots in the community')
# R2 = 0.06
## only 6% variation of interaction frequency is explained by parrot abundance
y<-lm(allIS~parrotprop,Indices_2)
#summary(y)

isxrich<-Indices_2 %>% 
  ggplot(aes(Richness.max,allIS))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Richness of site is also not a driver of interaction frequency',
       y='n interactions',x='Species richness')
# R2 = <0.1%
# plot
grid.arrange(isxprop,isxrich,ncol=2)

# iii avg aggression
### Aggregated aggression score increases as site diversity decreases
shanxagg<-Indices_2 %>% 
  ggplot(aes(avgratingNE,Shannon.max))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'On average, more aggressive interactions at less diverse sites',
       x='Average aggression rating',y='Shannon biodiversity')
# R2 = 0.58
richxagg<-Indices_2 %>% 
  ggplot(aes(avgratingNE,Richness.max))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'On average, more aggressive interactions at less species rich sites',
       x='Average aggression rating',y='Species richness')
# R2 = 0.3
propxagg<-Indices_2 %>% 
  ggplot(aes(avgratingNE,parrotprop))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Sites with greatest % of parrots have less aggressive interactions',
       x='Average aggression rating',y='Proportion of parrots in the community(%)')
# R2 = 0.43
## details of the species at each site may go a way to explain why this is

# plot
grid.arrange(shanxagg,richxagg,propxagg,ncol=3)


# iv Land type
isxcanopy<-Indices_2 %>% 
  ggplot(aes(canopypc,allIS))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Correlation between Canopy cover and n interactions')
# R2 = 0.46
## 46% variation of interaction frequency is explained by % canopy cover
## canopy cover associated with cavity-suitable trees
### No relationship between urban area,water body area, 
### total vegetation area, road area
isxveg<-Indices_2 %>% 
  ggplot(aes(Vegpc,allIS))+stat_poly_line(se=F)+stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Correlation between vegetation cover and n interactions')
isxbuild<-Indices_2 %>% 
  ggplot(aes(buildpc,allIS))+stat_poly_line(se=F)+stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Correlation between building cover and n interactions')
isxsurface<-Indices_2 %>% 
  ggplot(aes(surfacepc,allIS))+stat_poly_line(se=F)+stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Correlation between artificial surface cover and n interactions')

# plot
grid.arrange(isxcanopy,isxveg,isxbuild,isxsurface,ncol=2,nrow=2)

# v Cavity
Indices_2 %>% 
  ggplot(aes(cavnesterprop,avgrating))+
           stat_poly_line(se=F)+
           stat_poly_eq()+
           geom_point(size=4)+labs(title = 'Cavity nester proportion = higher aggression')
         

#===============================#
# Species level correlations----
#===============================#

Ints.Abundance %>% 
  ggplot(aes(Abundance,n_ints,color=Species))+
  geom_point(size=3,alpha=0.8)+
  labs(x='Relative abundance',y='total interaction involvement',
       title = 'Relative abundance / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))

Ints.Abundance %>% 
  ggplot(aes(max_obs,n_ints,color=Species))+
  geom_point(size=3,alpha=0.8)+
  labs(x='Abundance',y='total interaction involvement',
       title = 'Abundance / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))

Ints.Abundance %>% 
  ggplot(aes(proportion,n_ints,color=Species))+
  geom_point(size=3,alpha=0.8)+
  labs(x='Proportion of species in the community',y='total interaction involvement',
       title = 'Proportion of community / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================= 10.2 TOP-LINE REGRESSIONS ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

summary(Indices_2)

y<-aov(allIS~canopypc*cavnesterprop,data=Indices_2)
summary(y)



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================== 10.3 CHARTS: COMPOSITION ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

Composition_2 %>% 
  group_by(Study.Area,Species) %>% 
  select(Study.Area,Species,max_obs) %>% 
  summarise(max_obs=max(max_obs)) %>% 
  ggplot(aes(Study.Area,max_obs))+
  geom_jitter(aes(color=Species),width=0.12,size=5,alpha=0.6,shape=20)+coord_trans(y='log10')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311',
                              'Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377',
                              'Tanimbar corella'='#33BBEE',
                              'Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))+
  theme_light()+
  labs(title = 'Max daily counts per site, species',color="Species")#change legend title!!



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================== 10.4 CHARTS: INTERACTIONS ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

# n interactions parrots
ISRS %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  group_by(Species) %>% 
  summarise(n=sum(n_ints)) %>% 
  ggplot(aes(reorder(Species,-n),n))+
  geom_col()+
  geom_text(aes(label = n), vjust = -0.5)+
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  labs(x='Species',y='n',title='Total interactions')

# n interactions all
ISRS %>% 
  group_by(Species) %>% 
  summarise(n=sum(n_ints)) %>% 
  ggplot(aes(reorder(Species,n),n,))+ # this order high to low
  geom_col(position='dodge',alpha=0.8)+coord_flip()+
  labs(x='Species',y='total interactions',title='Total interactions')+
  theme(legend.position = 'none')

#2. ROLES
# n IS RS
ISRS %>%
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  group_by(Species,role) %>% 
  summarise(n=sum(n_ints)) %>% 
  ggplot(aes(reorder(Species,-n),n,fill=role))+
  geom_col(position = 'stack')+ 
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  labs(x='Species',y='n',title='n interactions intiated and recieved')+
  scale_fill_manual(values=c('IS'='#4a7b77','RS'='#f67e4b'))

# % IS RS
ISRS %>%  
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  group_by(Species,role) %>% 
  summarise(n=sum(n_ints)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(Species,-n),freq,fill=role))+
  geom_col(position = 'fill')+theme_minimal()+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  labs(x='Species',y='%',title='Proportion interactions intiated and recieved')+
  scale_fill_manual(values=c('IS'='#4a7b77','RS'='#f67e4b'))

# 3. W/L/NE summary
# n W/L/NE all ints
ISRS %>% 
  group_by(Species,outcome) %>%
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
    summarise(n=sum(n_ints)) %>% 
  ggplot(aes(reorder(Species,-n),n,fill=outcome))+
  geom_col(position = 'stack')+ 
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  labs(x='Species',y='n',title='n wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#4393c3','NE'='#f7f7f7','L'='#d6604d'))

ISRS %>% 
  group_by(Species,outcome) %>% filter(outcome!='NE') %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  summarise(n=sum(n_ints)) %>% 
  ggplot(aes(reorder(Species,-n),n,fill=outcome))+
  geom_col(position = 'stack')+ 
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  labs(x='Species',y='n',title='n wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#4393c3','L'='#d6604d'))

# % W/L/NE all ints
ISRS %>% 
  group_by(Species,outcome) %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  summarise(n=sum(n_ints)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(Species,-n),freq,fill=outcome))+
  geom_col(position = 'fill')+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#4393c3','NE'='#f7f7f7','L'='#d6604d'))

# relative freq (%)
ISRS %>% 
  filter(role=='IS') %>% group_by(Species) %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  mutate(freq=n_ints/sum(n_ints)*100) %>% 
  ggplot(aes(interaction,freq))+geom_col(width=0.95)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)+
  theme(legend.position = 'none')+labs(y='relative frequency',x='interaction',title='Interaction distribution: positively skewed')+
  facet_wrap(~Species)

# distance from nest
Interact %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  ggplot(aes(nxt_cav,initsp))+
  geom_jitter(aes(color=initsp),width=2,height=0.1,alpha=0.4,size=3,shape=20)+
  labs(y='Species observed',x='distance from cavity',title='Distance of interaction from the nearest cavity')+
  theme(legend.position = 'none')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311',
                              'Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377',
                              'Tanimbar corella'='#33BBEE',
                              'Long-tailed parakeet'='#009988'))

