
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

p_load(formattable,knitr,kableExtra, # nice tables
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

# Composition
Composition <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                          sheet = "Composition")

# Interactions
Interact <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Interactions")

# Niche overlap
NO <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                 sheet = "Composition")

# Environment

Enviro <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                     sheet = "Enviro")

# Trees

Tree <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                     sheet = "TC")


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
# Stirling Road                  9

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
Stirling$Effort<-Stirling$Effort*9
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
rm(Transect_2)
Transect_2 <- rbind(changiDAB,PasirDAB,SpringleafDAB,StirlingDAB,SengkangDAB)
Transect_2 <- Transect_2 %>% filter(species!='Total') %>% rename(Species=species)
#view(Transect_2)

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

rm(Composition_2)
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
rm(Interact_2)
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

# Factorise Tree data
str(Tree)
cols_tree <- c('Study.Area','tree_sp','id','cav','food')
Tree<-Tree %>% mutate_at(cols_tree,factor)  
str(Tree)  
rm(cols_tree)

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
#View(Composition_2)



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#========================= 8. DATA TRANSFORMATION ============================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

#===========================================#
# 8.a. Transformations of: COMPOSITION_2----
#===========================================#

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


#=============================#
# 8.b. ISRS Long Transform----
#=============================#

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
#view(ISRS)

x<- Composition_2 %>% select(Study.Area,Species,Avg_size,CavityYN,NestType)

ISRS <- merge(ISRS,x,by=c("Study.Area","Species"),all=T) %>% 
  distinct(Study.Area, Species,interaction,outcome,role,n_ints,Avg_size,
           CavityYN, .keep_all = TRUE) %>% filter(interaction!='NA')

#=================#
# 8.c. Tibbles----
#=================#

# Abundance, pop, ints----
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

z<-Composition %>% 
  select(Study.Area,Species,Avg_size,CavityYN,Foraginglab)

Ints.Abundance <- merge(Ints.Abundance,z,by=c("Study.Area","Species"))

Ints.Abundance_reduced <- Ints.Abundance %>% 
  distinct(Study.Area,Species,n_ints,Abundance,max_obs, .keep_all = TRUE)

# Unique species per parrot sp----
rm((parrot.targets))
parrot.targets <- Interact_2 %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  select(initsp,recipsp,rating) %>% 
  group_by(initsp,recipsp) %>% 
  count(recipsp) %>% 
  arrange(initsp,desc(n)) %>% rename(n_ints=n)
# Neutral only
x<-Interact_2 %>% 
  filter(rating=='0') %>% filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  select(initsp,recipsp,rating) %>% 
  group_by(initsp,recipsp) %>% 
  count(recipsp) %>% arrange(initsp,desc(n)) %>% rename(n_NE=n)
# Merge 1
parrot.targets<-merge(parrot.targets,x,by=c('initsp','recipsp'),all = TRUE) %>% replace(is.na(.), 0) 
# Aggressions, no NE
y<-Interact_2 %>% 
  filter(rating>=1) %>% filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  select(initsp,recipsp,rating) %>%
  group_by(initsp,recipsp) %>% count(recipsp) %>% 
  arrange(initsp,desc(n)) %>% rename(n_Agg=n)
# Merge 2
parrot.targets<-merge(parrot.targets,y,by=c('initsp','recipsp'),all = TRUE) %>% replace(is.na(.), 0)
parrot.targets<- parrot.targets%>% arrange(initsp,desc(n_Agg))

#view(parrot.targets)


#============================#
# Additions to: INDICES_2----
#============================#

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

### FROM ISRS

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


#============================#
# 8.d. Tree & Cavity data----
#============================#

###


#===============================#
# 8.e. Enviro Long Transform----
#===============================#

# add properly summed Vegetation cover (less canopy cover)
Enviro <- Enviro %>% 
  mutate(Vegpc_act=Vegpc-canopypc)
  
Enviro_2 <- Enviro %>% 
  select(Study.Area,canopypc,Vegpc,buildpc,surfacepc,waterpc,Vegpc_act) %>% 
  gather(key='land_prop',value='proportion',-Study.Area)

Enviro_2$land_prop <- factor(Enviro_2$land_prop
                             ,levels = c('buildpc','surfacepc','Vegpc_act',
                                         'canopypc','waterpc'))
levels(Enviro_2$land_prop)

#===========================#
# OTHER / UNUSED SCRIPT
#===========================#

# Master
# + abundance, max count, max prop, total count, total prop
#x<-Composition_2 %>% select(Study.Area,Species,max_obs,max.proportion,
#                            total_obs,total.proportion,sp_lab,Avg_size,NestType,
 #                           CavityYN,Abundance,Richness.all,Richness.max,Shannon.all,
 #                           Shannon.max,Simpson.all,Simpson.max) 
#Master<-merge(ISRS,x,by=c('Study.Area','Species'),all=T)
#dim(Master)
#Master<-Master %>% distinct(Study.Area,Species,interaction,outcome,
  #                         role,n_ints, .keep_all = TRUE)



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================ 9. DATA EXPLORATION ============================
##/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

# 5 sites were surveyed. 
# Each site was surveyed for a total of 15 hours.
# Equal effort per site and species.
# Sites were selected based on NSS Parrot Count data to provide 
#   equal effort to each focal species.

#=========================================#
# 9.a. Composition / Overall summaries---- 
#=========================================#

nrow(Composition_2)
# 3,458 individuals observed in the composition surveys

Composition_2 %>% select(Species) %>% n_distinct()
# 81 distinct species observed observed in the composition surveys
  # = 19.9% of all known species in Singapore
    # https://www.nparks.gov.sg/biodiversity/wildlife-in-singapore/species-list/bird
      # 407 spp


#================================#
# 9.b. Interaction summaries---- 
#================================#

Interact_2 %>% count(initsp) %>% summarise(sum(n))
# 758 total interactions were observed 

Interact_2 %>% filter(rsout=='NE') %>% count(initsp) %>% summarise(sum(n))
# of which 598 (78.89% were aggressive)
  # 160 (21.1% NEUTRAL)

n_distinct(ISRS$Species)
# 49 distinct species were observed in interactions
  # 60.49% of all species observed
    # 12.03% of all species

Interact_2 %>% summarise(n_distinct(initsp))
Interact_2 %>% summarise(n_distinct(recipsp))
# 30 initiating species
# 45 recipient species

#////////////////////
# Interaction pairs
#///////////////////

int_pairs <- Interact_2 %>%
  count(initsp, recipsp) %>%
  complete(initsp, nesting(recipsp), fill = list(n = 0)) %>% 
  filter(n!='0') %>% 
  arrange(desc(n))
int_pairs %>% print(n=20) # top 10 interaction pairs
nrow(int_pairs)
## 160 unique species pair interactions were observed

int_pairs %>% 
  group_by(recipsp) %>% summarise(n=sum(n)) %>% 
  mutate(freq=n/sum(n)*100) %>% arrange(desc(freq)) %>% 
  filter(recipsp!='Red-breasted parakeet',recipsp!='Rose-ringed parakeet',
         recipsp!='Monk parakeet',recipsp!='Tanimbar corella') %>% 
  print(n=30)
# Parrots excluded~ Top recipients
  # Javan myna, LTP, house crow, OPH, YCC, AGS, YVBB, Oriole,
  # Flameback, dollarbird 


#=================================#
# 9.c. Cavity Nesters in focus----
#================================#

#  filter(Species!='Red-breasted parakeet',Species!='Rose-ringed parakeet',Species!='Monk parakeet',Species!='Tanimbar corella') %>% 
  
ISRS %>% 
  filter(role=='RS') %>%  filter(NestType=='Cavity') %>% # modify IS/RS
  summarise(n_distinct(Species))
# 15 cavity nesting sp. were interaction initiators
  # == 30% of all interacting sp.
  # == 50% of all initiators

ISRS %>% 
    filter(role=='RS') %>% filter(NestType=='Cavity') %>%  
  summarise(n=sum(n_ints))
# Cavity nesters (inc. focal sp.) initiated 597 [78.75%] of all interactions
  # Focal parrots initiated 397 [52.37%] of all interactions
    # Non-focal cavity nesters initiated 200 [26.38%] of all interactions
  
# Cavity nesters (inc. focal parrots) received 559 [73.74%] of all interactions
#   Non-focal cavity nesters received of 338 [44.59%] of all interactions
 

#=============================#
# 9.c.i. Aggressions only ----
#=============================#

# 598 aggressions

#  filter(Species!='Red-breasted parakeet',Species!='Rose-ringed parakeet',Species!='Monk parakeet',Species!='Tanimbar corella') %>% 

ISRS %>% 
  filter(Species!='Red-breasted parakeet',Species!='Rose-ringed parakeet',Species!='Monk parakeet',Species!='Tanimbar corella') %>% 
    filter(role=='RS') %>% filter(NestType=='Cavity') %>% filter(outcome!='NE') %>%  
  summarise(n=sum(n_ints))
# Cavity nesters (inc. focal parrots) initiated 467 (78.09%) of all aggressions
# Focal parrots initiated 323 (54.01%) of all aggressions
# Non-focal cavity nesters initiated 144 (24.08%) of all aggressions

# Cavity nesters (inc. focal parrots) received 439 (73.41%) of all aggressions
# Focal parrots initiated 167 (27.92%) of all aggressions
# Non-focal cavity nesters received of 272 (45.48%) of all aggressions 



#================================#
# 9.d. Tree / Cavity profiles----
#================================#

Tree %>% group_by(Study.Area) %>% summarise(n_distinct(id)) 
# Changi Village = 208 trees identified
# Stirling Road  = 144 trees

Tree %>% group_by(Study.Area) %>% summarise(n_distinct(tree_sp)) 
# Changi Village = 15 tree species
# Stirling Road  = 24 tree species

Tree %>% filter(cav=='y') %>% 
  group_by(Study.Area,cav) %>% tally()

# Changi Village = 236 trees with cavities 
# Stirling Road  = 77 trees with cavities

Tree %>% 
  select(Study.Area,id,cav) %>% 
  distinct(Study.Area,id,cav) %>% 
  select(-id) %>% group_by(Study.Area,cav) %>% tally() %>% 
  mutate(freq=n/sum(n)*100)

# Changi Village = only 62 (29.8%) cavity bearing
# Stirling Road  = only 31 (21.5%) cavity bearing


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


#=========================#
# i Richness x parrot prop
#=========================#

richxprop <-Indices_2 %>% 
  ggplot(aes(parrotprop,Richness.max))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Parrot proportion in the community positively correlates with overall species Richness',
       y='Species richness',x='Proportion of parrots in the community')
#R2 = 0.83
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
#R2 = 0.78
## 75% variation in biodoversity (shann) is attributed to parrot abundance
y1<-lm(Shannon.mean~parrotprop,Indices_2)
#summary(y1)
# plot
grid.arrange(richxprop,shannxprop,ncol=2)

## Tanimbar corella may have a negative effect on Richness and BD within an area
## Stirling RD & CV are lowest on Rich&BD



#==========================#
# ii initiated interactions
#==========================#

# parrot prop in community != more interactions
isxprop<-Indices_2 %>% 
  ggplot(aes(parrotprop,allIS))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Greater proportion of parrots does not predict greater interaction frequency',
       y='n interactions',x='Proportion of parrots in the community')
# R2 = 0.05
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


#===================#
# iii avg aggression
#===================#

### Aggregated aggression score increases as site diversity decreases
shanxagg<-Indices_2 %>% 
  ggplot(aes(avgratingNE,Shannon.max))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'On average, more aggressive interactions at less diverse sites',
       x='Average aggression rating',y='Shannon biodiversity')
# R2 = 0.72
richxagg<-Indices_2 %>% 
  ggplot(aes(avgratingNE,Richness.max))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'On average, more aggressive interactions at less species rich sites',
       x='Average aggression rating',y='Species richness')
# R2 = 0.45
propxagg<-Indices_2 %>% 
  ggplot(aes(avgratingNE,parrotprop))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Sites with greatest % of parrots have less aggressive interactions',
       x='Average aggression rating',y='Proportion of parrots in the community(%)')
# R2 = 0.54
## details of the species at each site may go a way to explain why this is

# plot
grid.arrange(shanxagg,richxagg,propxagg,ncol=3)


#=============#
# iv Land type
#=============#

isxcanopy<-Indices_2 %>% 
  ggplot(aes(canopypc,allIS))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Correlation between Canopy cover and n interactions')
# R2 = 0.55
## 55% variation of interaction frequency is explained by % canopy cover
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


#===========================#
# v Cavity nester proportion
#===========================#

Indices_2 %>% 
  ggplot(aes(cavnesterprop,avgrating))+
           stat_poly_line(se=F)+
           stat_poly_eq()+
           geom_point(aes(color=Study.Area),size=4)+labs(title = 'Cavity nester proportion = higher aggression')
# R2 = 0.66
## greater proportion of cavity nesters correlates to higher aggression

#===============================#
# Species level correlations----
#===============================#

# Relative Abundance
Ints.Abundance %>% 
  ggplot(aes(Abundance,n_ints,color=Species))+
  geom_point(size=3,alpha=0.8)+
  labs(x='Relative abundance',y='total interaction involvement',
       title = 'Relative abundance / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))

# Abundance
Ints.Abundance %>% 
  ggplot(aes(max_obs,n_ints,color=Species))+
  geom_point(size=3,alpha=0.8)+
  labs(x='Abundance',y='total interaction involvement',
       title = 'Abundance / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))

# Proportion of community
Ints.Abundance %>% 
  ggplot(aes(proportion,n_ints,color=Species))+
  geom_point(size=3,alpha=0.8)+
  labs(x='Proportion of species in the community',y='total interaction involvement',
       title = 'Proportion of community / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))

# Proportion by site
Ints.Abundance %>% 
  ggplot(aes(proportion,n_ints,color=Species))+
  geom_point(size=3,alpha=0.8)+
  labs(x='Proportion of species in the community',y='total interaction involvement',
       title = 'Proportion of community / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))+
  facet_wrap(~Study.Area)

# Average size
Ints.Abundance_reduced %>%
  select(-Study.Area) %>% 
  group_by(Species) %>% 
  summarise(n_ints=sum(n_ints),
            Avg_size=mean(Avg_size),
            CavityYN=mean(CavityYN)) %>% 
  ggplot(aes(Avg_size,n_ints,color=Species))+
  geom_jitter()

#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================= 10.2 TOP-LINE REGRESSIONS ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

summary(Indices_2)




#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================== 10.3 CHARTS: COMPOSITION ===========================
##/////////////////////////////////////////////////////////////////////////////#
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

Composition_2 %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  group_by(Study.Area,Species) %>% 
  select(Study.Area,Species,max.proportion) %>% 
  summarise(max.proportion=max(max.proportion)) %>% arrange(Study.Area,desc(max.proportion))
  
# Changi Vilage = RBP + TC
# Pasir Ris     = RBP + RRP + MP
# Sengkang      = RBP + RRP + LTP
# Springleaf    = RBP + RRP + LTP + BRP
# Stirling Rd   = RBP + RRP + TC 

## Red-breasted parakeets were seen at 5/5 sites
## Rose-ringed parakeets were seen in 4/5 sites
## Tanimbar corella seen at 2/5 
## MP seen and observed at 1/5 sites
### Native LTP observed at 2/5 sites

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
# 

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

# Size
ISRS %>% 
  group_by(Species) %>% 
  summarise(avg_agg=mean(rating),
            Avg_size=mean(Avg_size)) %>% 
  filter(avg_agg>0) %>% 
  ggplot(aes(Avg_size,avg_agg))+
  geom_point()


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================= 10.5 CHARTS: ENVIRONMENTAL ==========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

Enviro_2 %>% 
  filter(land_prop!='Vegpc') %>% 
  ggplot(aes(Study.Area,proportion,fill=land_prop))+
  geom_col(position='fill')+
  labs(y='proportion of land cover type',x='Study Area',title = 'Land type per study site')+
  scale_fill_manual(values = c('buildpc'='#f6c141',
                               'surfacepc'='#f1932d',
                               'Vegpc_act'='#90c987',
                               'canopypc'='#4eb265',
                               'waterpc'='#7bafde'))
