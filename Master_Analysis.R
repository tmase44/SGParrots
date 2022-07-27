
#/////////////////////////////////////////////////////////////////////////////#
#============================  MASTER ANALYSIS   =============================
#/////////////////////////////////////////////////////////////////////////////#


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================= LOAD PACKS ==================================  
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
library(pacman)
p_load(formattable,knitr,kableExtra, # nice tables
       tidyverse,vegan,lubridate,gridExtra,grid,ggrepel,reshape2,ggpmisc,
       BBmisc,stringr,Hmisc,moments,
       ggpubr,AICcmodavg, #anova
       circlize, # interaction networks
       Distance, # transect analysis, relative abundance, density
       readxl,writexl)
library(Ostats)
library(gam)
library(GGally)
library(psych)#
#library(MASS)
#detach("package:MASS", unload=TRUE)




#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================== IMPORT DATA ================================  
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
# Composition
Composition <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                          sheet = "Composition")
# Interactions
Interact <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Interactions")
# Profile / Niche
Prof <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Profile2")
# Enviro
Enviro <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                     sheet = "Enviro")
# NSS data
NSS <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/NSS/data/NSS_parrot_data_long.xlsx", 
                  sheet = "data")
# pilot time data
 Pilot <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                    sheet = "pilottime")


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================ ALPHA BD INDICES ============================= 
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

#========================================#
# Simpson, Shannon, Richness----
#========================================#
Comp.max<-Composition %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% 
  summarise(max_obs = max(n)) %>% 
  arrange(Study.Area,desc(max_obs))

# spread/gather
Comp.alpha<-Comp.max %>% 
  spread(key=Species,value = max_obs) %>% 
  replace(is.na(.), 0) %>% 
  remove_rownames %>% 
  column_to_rownames(var="Study.Area")

# Richness.max
fun.1<-function(x){sum(x>0)}
richness<-apply(Comp.alpha, 1, FUN=fun.1)
richness<-data.frame(richness)
colnames(richness)<-"Richness"

# Shannon.max
for (Comp.alpha.row in 1:6)
{shannon<- matrix(diversity(Comp.alpha[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(Comp.alpha)
colnames(shannon)<-"Shannon"

# Simpson.max
for (Comp.alpha.row in 1:6)
{simpson<- matrix(diversity(Comp.alpha[,], index = "simpson"))}
simpson<-round(simpson,3)
#Adjusting the names of rows and columns
row.names(simpson)<-row.names(Comp.alpha)
colnames(simpson)<-"Simpson"

# Putting together all indices.max
Indices<-cbind(richness, shannon, simpson)
Indices<-data.frame(Indices)
Indices <- rownames_to_column(Indices, "Study.Area")


#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#============================= BETA BD INDICES ============================= 
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

#===================#
# Sorensen index----
#===================#
#Calculating Sorensen index
sorensen<-designdist(Comp.alpha, method="(2∗J)/(A+B)", terms=c("binary"))
sorensen<-round(sorensen,3)
sorensen

# Higher values of the Sorensen index indicate greater overlap in species
# composition and vice versa

#Plotting a dendrogram
plot(hclust(sorensen, method="complete"),
     main="Sorensen index", col = "#1965B0",
     col.main = "black", col.lab = "black",
     col.axis = "black", sub = "")

# Or as a matrix
#Transforming results into a matrix
sorensen.complete<-stats:::as.matrix.dist(sorensen)
sorensen.complete

#Melt function
sorensen.melted<-melt(sorensen.complete)
sorensen.melted


#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#======================== DATA TRANSFORMATION ===========================
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

# Merge Indices (BD, max counts) + profiles
rm(Composition_2) 
Composition_2<-merge(Comp.max,Prof, by='Species',all=T)

  # add percent abundance 
x<-Composition %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% 
  summarise(max_obs = max(n)) %>% 
  mutate(max.freq=max_obs/sum(max_obs)*100) %>% select(-max_obs)
  # merge
Composition_2<-merge(Composition_2,x, by=c('Study.Area','Species'),all = T)
  # rearrange cols
Composition_2<-Composition_2 %>% relocate(1,2,5,3,17,10,11,8,9,4,6,7,12,13,14,15,16)
  # add all_obs
x<-Composition %>% 
  group_by(Study.Area,Species) %>% 
  summarise(all_obs=n())
  #merge
Composition_2<-merge(Composition_2,x, by=c('Study.Area','Species'),all = T)
Composition_2<-Composition_2 %>% relocate(all_obs,.after = SG_status)

# Factorise Composition_2
str(Composition_2)
cols_comp <-c('Study.Area','Species','SG_status',
              'IUCN_status','NestType','sp_lab',
              'TrophicNiche','ForagingNiche')
Composition_2<-Composition_2 %>% mutate_at(cols_comp, factor)
str(Composition_2)
rm(cols_comp)

# Factorise Interactions
str(Interact)
cols_int <-c('Region.Label','Study.Area','ampm','initsp','recipsp','interaction',
             'isout','rsout','treeid','treesci','treecom','trim','at_cav')
Interact_2<-Interact %>% mutate_at(cols_int,factor)  
str(Interact_2)  
rm(cols_int)

  # Add interaction ratings
Interact_2<-Interact_2 %>% 
  mutate(interaction=case_when(
    interaction=="Neutral"~"Neutral", # no aggression
    interaction=="Displace"~"Displace", # remove from perch by landing
    interaction=="Threat"~"Threat", # vocal or flaring threats
    interaction=="Swoop"~"Swoop", # fly within 60cm at species but not landing
    interaction=="Chase"~"Chase", # in-flight pursuit may or may not displace
    interaction=="Contact"~"Contact", # physical contact
    interaction=="Fight"~"Contact")) # multiple physical contact
Interact_2$interaction<-factor(Interact_2$interaction,
                                  levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact"))

# order sites by Shannon score
Interact_2$Study.Area<-factor(Interact_2$Study.Area,
                                 levels = c(
                                   'Changi Airport','Changi Village','Stirling Road','Sengkang Riverside Park',
                                   'Pasir Ris Town Park','Palawan Beach','Springleaf'
                                 ))
levels(Interact_2$Study.Area)

Interact_2$tree_h<-as.numeric(Interact_2$tree_h)

# Factorise NSS data
NSS$Site<-as.factor(NSS$Site)
NSS$Study.Area<-as.factor(NSS$Study.Area)
NSS$Species<-as.factor(NSS$Species)
  # remove na
NSS$Count<-NSS$Count %>% replace(is.na(.), 0)


#////////////////////////////////
# ISRS: Interact long-transform
#///////////////////////////////
rm(ISRS)
# IS
IS<-Interact_2 %>% 
  filter(recipsp!="NA") %>% 
  group_by(Study.Area,initsp,interaction,isout,) %>% 
  tally()
IS<-rename(IS,species=initsp)  
IS<-rename(IS,outcome=isout)
IS$role<-'IS' # identify IS/RS

# RS
RS<-Interact_2 %>% 
  filter(recipsp!="NA") %>% 
  group_by(Study.Area,recipsp,interaction,rsout) %>% 
  tally()
RS<-rename(RS,species=recipsp)  
RS<-rename(RS,outcome=rsout)
RS$role<-'RS' # identify IS/RS

# Combine IS+RS
ISRS<-rbind(IS,RS)
ISRS<-ISRS %>% relocate(1,2,6,3,4,5) 
ISRS<-ISRS %>% rename(n_ints=n)
ISRS<-ISRS %>% rename(Species=species)

  # merge with Composition_2
ISRS<-merge(ISRS,Composition_2,by=c('Study.Area','Species'),all=T)

# ISRS: add other calculated columns

  # n_ints - excluding Neutral
x<-ISRS %>% 
  group_by(Study.Area,Species) %>% filter(outcome!='NE') %>% 
  summarise(n_ints_xNE=sum(n_ints))
ISRS<-merge(ISRS,x,by=c('Study.Area','Species'),all=T) 
ISRS$n_ints_xNE<-ISRS$n_ints_xNE %>% replace(is.na(.), 0)
ISRS$max_obs<-ISRS$max_obs %>%  replace(is.na(.), 0)
  # N_initiatations - excluding Neutral
x<-ISRS %>% 
  group_by(Study.Area,Species) %>% filter(outcome!='NE') %>% filter(role=='IS') %>% 
  summarise(inits_xNE=sum(n_ints))
ISRS<-merge(ISRS,x,by=c('Study.Area','Species'),all=T) 
ISRS$inits_xNE<-ISRS$inits_xNE %>% replace(is.na(.), 0)
  # n_intiations - incl Neutral
x<-ISRS %>% 
  group_by(Study.Area,Species) %>% filter(role=='IS') %>% 
  summarise(inits=sum(n_ints))
ISRS<-merge(ISRS,x,by=c('Study.Area','Species'),all=T) 
ISRS$inits<-ISRS$inits %>% replace(is.na(.), 0)

# wrangle cats
x<-ISRS %>% 
  filter(Species=='Cat'|Species=='Otter'|
           Species=='Squirrel'|Species=='Long-tailed macaque') %>%
  group_by(Species) %>% 
  fill(everything(), .direction = "updown") %>% 
  distinct() %>% filter(n_ints==2)

ISRS<-ISRS %>% filter(Species!='Cat'&Species!='Otter'&
                                          Species!='Squirrel'&Species!='Long-tailed macaque')
ISRS<-rbind(ISRS,x)

# add visit weight
x<-Composition %>% group_by(Study.Area) %>% 
  summarise(effort=max(Surveyno))
ISRS<-merge(ISRS,x,by='Study.Area',all=T)

# Interactions / hour standardized by obs time
  # all ints, all ints without NE
  # all inits, all inits without NE
ISRS<-ISRS %>%
  group_by(Study.Area,Species) %>% 
  mutate(ints_HR=n_ints/effort) %>% 
  mutate(ints_xNE_HR=n_ints_xNE/effort) %>% 
  mutate(inits_HR=inits/effort) %>% 
  mutate(inits_xNE_HR=inits_xNE/effort)

ISRS<-ISRS %>% relocate(1,2,3,4,5,6,22,23,24,25,26,27,28,29,
                        7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
                        
ISRS<-ISRS %>% arrange(Study.Area,desc(n_ints))
ISRS$n_ints<-ISRS$n_ints%>% replace(is.na(.), 0)
ISRS$ints_HR<-ISRS$ints_HR%>% replace(is.na(.), 0)

#///////////////////////
# Species Pairs
#///////////////////////
# includes:
## all species pairs per study.area
## each interaction type & outcome
## plus species characteristics
## plus site characteristics
rm(sp.pairs)
# pairs for all interactions
sp.pairs <- Interact_2 %>% 
  select(Study.Area,initsp,recipsp,interaction,isout,rsout) %>% 
  group_by(Study.Area,initsp,recipsp,interaction,isout,rsout) %>% 
  count(rsout) %>% rename(pair_ints=n)

## merge species traits for IS
x<-Prof %>% select(Species,NestType,sp_lab,Avg_size,Avg_Weight,SG_status)
x<-x %>% rename(IS.NestType=NestType,
                IS.sp_lab=sp_lab,
                IS.size=Avg_size,
                IS.wt=Avg_Weight,
                IS.status=SG_status) 
sp.pairs<-merge(sp.pairs,x,by.x='initsp',by.y='Species')
## merge species traits for RS
x<-Prof %>% select(Species,NestType,sp_lab,Avg_size,Avg_Weight,SG_status)
x<-x %>% rename(RS.NestType=NestType,
                RS.sp_lab=sp_lab,
                RS.size=Avg_size,
                RS.wt=Avg_Weight,
                RS.status=SG_status) 
sp.pairs<-merge(sp.pairs,x,by.x='recipsp',by.y='Species')



#///////////////////////
# Indices_2
#///////////////////////


rm(Indices_2)
# Environment data
envpc<-Enviro %>% 
  select(Study.Area,
         `Site habitat types`,`Surrounding habitat types`,
         areaHa,
         canopypc,Vegpc,buildpc,artsurfacepc,waterpc,natsurfacepc,mangrovepc,
         n_cavity,cavs_canopy_sqm)
Indices_2<-merge(Indices,envpc,by='Study.Area')

# Total non-native "I" species proportion
x<-Composition_2 %>% 
  filter(SG_status=='I') %>% 
  group_by(Study.Area,Species) %>% 
  summarise(freq=mean(max.freq)) %>% 
  group_by(Study.Area) %>% 
  summarise(freq.I=sum(freq))
Indices_2<-merge(Indices_2,x,by='Study.Area')

# Total non-native "I" species number
x<-Composition_2 %>% 
  filter(SG_status=='I') %>% 
  group_by(Study.Area) %>% 
  summarise(n.I=sum(max_obs))
Indices_2<-merge(Indices_2,x,by='Study.Area')

# Parrot proportion
x<-Composition_2 %>% 
  filter(Species=="Monk parakeet"|Species=="Red-breasted parakeet"|Species=="Tanimbar corella"|
           Species=="Long-tailed parakeet"|Species=="Rose-ringed parakeet"|
           Species=='Yellow crested cockatoo'|Species=='Sulphur crested cockatoo') %>% 
  group_by(Study.Area,Species) %>% 
  summarise(freq=mean(max.freq)) %>% 
  group_by(Study.Area) %>% 
  summarise(freq.parrots=sum(freq))
Indices_2<-merge(Indices_2,x,by='Study.Area')

# Parrot n
x<-Composition_2 %>% 
  filter(Species=="Monk parakeet"|Species=="Red-breasted parakeet"|Species=="Tanimbar corella"|
           Species=="Long-tailed parakeet"|Species=="Rose-ringed parakeet"|
           Species=='Yellow crested cockatoo'|Species=='Sulphur crested cockatoo') %>% 
  group_by(Study.Area) %>% 
  summarise(n.parrots=sum(max_obs))
Indices_2<-merge(Indices_2,x,by='Study.Area')

# Explicit Cavity nester proportion
x<-Composition_2 %>% 
  filter(NestType=='Cavity') %>% 
  group_by(Study.Area,Species) %>% 
  summarise(max.freq=mean(max.freq)) %>% 
  group_by(Study.Area) %>% 
  summarise(cav.sp.freq=sum(max.freq))
Indices_2<-merge(Indices_2,x,by='Study.Area')

# total interactions per site
x<-ISRS %>% 
  filter(role=='IS') %>% 
  group_by(Study.Area) %>% 
  summarise(site.interactions=sum(n_ints))
Indices_2<-merge(Indices_2,x,by='Study.Area')

# avg tree height
x<-Interact_2 %>%
  select(Study.Area,tree_h,cav_h_m) %>% 
  filter(!is.na(tree_h)) %>% 
  filter(!is.na(cav_h_m)) %>% 
  group_by(Study.Area) %>% 
  summarise(meantree=mean(tree_h),
            meancav=mean(cav_h_m))
Indices_2<-merge(Indices_2,x,by='Study.Area')


#//////////////////////////////////////
# Adding Indices data to other DFs----
#//////////////////////////////////////
Composition_2<-merge(Composition_2,Indices_2,by='Study.Area',all=T)
ISRS<-merge(ISRS,Indices_2,by='Study.Area',all=T)
sp.pairs<-merge(sp.pairs,Indices_2,by='Study.Area',all=T)
sp.pairs<-sp.pairs %>% select(-20)
sp.pairs<-sp.pairs %>% mutate(size_diff=RS.size-IS.size) %>% 
  relocate(size_diff,.after = 'RS.size')

# extra interaction spread for indices
x<-ISRS %>% 
  ungroup() %>% 
  filter(role=='IS') %>% 
  select(Study.Area,interaction,n_ints) %>% 
  group_by(Study.Area,interaction) %>% 
  summarise(n_ints=sum(n_ints)) %>% 
  ungroup() %>% 
  spread(key=interaction,value = n_ints) %>% 
  replace(is.na(.), 0)
Indices_2<-merge(Indices_2,x,by='Study.Area')

# n_ints to Composition_2
x<-ISRS %>% 
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
    select(Study.Area,Species,interaction,n_ints) %>% 
  group_by(Study.Area,Species) %>% 
  summarise(n_ints=sum(n_ints)) 
Composition_2<-merge(Composition_2,x,by=c('Study.Area','Species'),all=T)
Composition_2$n_ints<-Composition_2$n_ints %>% replace(is.na(.), 0)
x<-ISRS %>% 
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
    select(Study.Area,Species,interaction,n_ints) %>% 
  filter(interaction!='Neutral') %>% 
  group_by(Study.Area,Species) %>% 
  summarise(n_ints_xNE=sum(n_ints)) 
Composition_2<-merge(Composition_2,x,by=c('Study.Area','Species'),all=T)
Composition_2$n_ints_xNE<-Composition_2$n_ints_xNE %>% replace(is.na(.), 0)
  
# n_initis to Composition_2
x<-ISRS %>% 
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
    filter(role=='IS') %>% 
  select(Study.Area,Species,interaction,n_ints) %>% 
  group_by(Study.Area,Species) %>% 
  summarise(n_initis=sum(n_ints)) 
Composition_2<-merge(Composition_2,x,by=c('Study.Area','Species'),all=T)
Composition_2$n_initis<-Composition_2$n_initis %>% replace(is.na(.), 0)
x<-ISRS %>% 
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
    filter(role=='IS') %>% 
    select(Study.Area,Species,interaction,n_ints) %>% 
  filter(interaction!='Neutral') %>% 
  group_by(Study.Area,Species) %>% 
  summarise(n_initis_xNE=sum(n_ints)) 
Composition_2<-merge(Composition_2,x,by=c('Study.Area','Species'),all=T)
Composition_2$n_initis_xNE<-Composition_2$n_initis_xNE %>% replace(is.na(.), 0)
#///////////////////////
# Enviro Long Transform
#///////////////////////

# just for charting

Enviro_2 <- Enviro %>% 
  select(Study.Area,canopypc,Vegpc,buildpc,artsurfacepc,waterpc,natsurfacepc,mangrovepc) %>% 
  gather(key='land_prop',value='proportion',-Study.Area)

Enviro_2$land_prop <- factor(Enviro_2$land_prop
                             ,levels = c('buildpc','artsurfacepc','natsurfacepc',
                                         'Vegpc','canopypc','waterpc','mangrovepc'))
levels(Enviro_2$land_prop)

# factor releveling----
# order sites by Shannon score
ISRS$Study.Area<-factor(ISRS$Study.Area,
                              levels = c(
                                'Changi Airport','Changi Village','Stirling Road','Sengkang Riverside Park',
                                'Pasir Ris Town Park','Palawan Beach','Springleaf'
                              ))
Indices_2$Study.Area<-factor(Indices_2$Study.Area,
                        levels = c(
                          'Changi Airport','Changi Village','Stirling Road','Sengkang Riverside Park',
                          'Pasir Ris Town Park','Palawan Beach','Springleaf'
                        ))

Composition_2$Study.Area<-factor(Composition_2$Study.Area,
                             levels = c(
                               'Changi Airport','Changi Village','Stirling Road','Sengkang Riverside Park',
                               'Pasir Ris Town Park','Palawan Beach','Springleaf'
                             ))
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#============================= DATA SUMMARIES =============================
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

x<-Composition %>% select(Study.Area) %>% n_distinct()
y<-Composition %>% group_by(Study.Area) %>% summarise(n=max(Surveyno)) %>% 
  ungroup() %>% summarise(sum(n)*1.5)
sprintf('%s sites were surveyed for a total of %s hours',x,y)

# 5 sites were surveyed. 
# Each site was surveyed for a total of 15 hours.
# Equal effort per site and species.
# Sites were selected based on NSS Parrot Count data to provide 
#   equal effort to each focal species.

#//////////////////////
# Composition summaries ----
#//////////////////////
# https://www.nparks.gov.sg/biodiversity/wildlife-in-singapore/species-list/bird

a<-nrow(Composition)
b<-Composition %>% select(Species) %>% n_distinct()
c<-round((b/407)*100,2)
sprintf('%s individuals observed in total',a)
sprintf('%s distinct species observed, equal to %s%% of avian species in Singapore',b,c)

# number by status
Comp.max %>% ungroup() %>% summarise(n=sum(max_obs))
Composition_2 %>% filter(SG_status=='R')%>% 
  select(Species) %>% n_distinct()
Composition_2 %>% filter(SG_status=='R')%>% 
  select(Species,max_obs) %>% summarise(n=sum(max_obs))
Composition_2 %>% filter(SG_status=='I')%>% 
  select(Species) %>% n_distinct()
Composition_2 %>% filter(SG_status=='I')%>% 
  select(Species,max_obs) %>% summarise(n=sum(max_obs))
Composition_2 %>% filter(SG_status=='V'|SG_status=='M'|SG_status=='N')%>% 
  select(Species) %>% n_distinct()
Composition_2 %>% filter(SG_status=='V'|SG_status=='M'|SG_status=='N')%>% 
  select(Species) %>% n_distinct()



#//////////////////////
# Interaction summaries----
#//////////////////////
d<-Interact %>% count(initsp) %>% summarise(sum(n))
e<-Interact %>% filter(rsout!='NE') %>% count(initsp) %>% summarise(sum(n))
f<-round((e/d)*100,2)
sprintf('%s observed interactions, of which %s [%s%%] were aggressive',d,e,f)

g<-Interact %>% summarise(n_distinct(initsp))
h<-round((g/b)*100,2)
i<-Interact %>% summarise(n_distinct(recipsp))
j<-round((i/b)*100,2)
sprintf('%s distinct initiator species, %s%% of the observed species pool',g,h)
sprintf('%s distinct initiator species, %s%% of the observed species pool',i,j)

ISRS %>% 
  filter(role=='IS'|role=='RS') %>% 
  summarise(n_distinct(Species))
51/90
#////////////////////
# Interaction pairs----
#///////////////////
int_pairs <- Interact_2 %>%
  #filter(interaction!='Neutral') %>% 
  count(initsp, recipsp) %>%
  complete(initsp, nesting(recipsp), fill = list(n = 0)) %>% 
  filter(n!='0') %>% 
  arrange(desc(n))
int_pairs %>% print(n=20) # top 10 interaction pairs
x<-nrow(int_pairs)
sprintf('%s unique species pairs were observed interacting',x)

int_pairs %>% 
  filter(initsp=='Red-breasted parakeet'|initsp=='Rose-ringed parakeet'|
         initsp=='Monk parakeet'|initsp=='Tanimbar corella'|
         initsp=='Yellow crested cockatoo'|initsp=='Sulphur crested cockatoo') %>% 
  filter(recipsp!='Red-breasted parakeet',recipsp!='Rose-ringed parakeet',
         recipsp!='Monk parakeet',recipsp!='Tanimbar corella',
         recipsp!='Yellow crested cockatoo',recipsp!='Sulphur crested cockatoo') %>% 
    group_by(recipsp) %>% summarise(n=sum(n)) %>% 
  mutate(freq=n/sum(n)*100) %>% arrange(desc(freq)) %>% 
  print(n=30)
# non native parrots excluded
# ~ Top recipients
  # Javan myna, LTP, house crow, LTP OPH, YCC, AGS, YVBB, Oriole,
  # Flameback, dollarbird ===
int_pairs %>% 
  group_by(recipsp) %>% summarise(n=sum(n)) %>% 
  mutate(freq=n/sum(n)*100) %>% arrange(desc(freq)) %>% 
  print(n=30)

#////////////////////////
# Cavity Nesters in focus----
#////////////////////////
x<-ISRS %>% 
  ungroup() %>% 
  filter(NestType=='Cavity') %>% 
  summarise(n=n_distinct(Species)) %>% summarise(sum(n))
y<-ISRS %>% 
  ungroup() %>% 
  filter(NestType=='Cavity') %>% filter(role=='IS') %>% 
  summarise(n=n_distinct(Species)) %>% summarise(sum(n))
y2<-ISRS %>% ungroup() %>% 
  filter(role=='IS') %>% filter(NestType=='Cavity') %>%  
  summarise(n=sum(n_ints))
y3<-round((y2/d)*100,2)
z<-round((x/b)*100,2)
z2<-round((y/b)*100,2)
sprintf('%s cavity nesting species involved in interactions, representing %s%% of the species pool',x,z)
sprintf('%s cavity nesters intiated %s [%s%%] of all interactions, despite only accounting for %s%% of the species pool',y,y2,y3,z2)

ISRS %>% filter(NestType=='Cavity') %>% ungroup() %>% 
  count(Species,NestType) %>% arrange(desc(n))
# List of cavity nesters involved in interactions

y4<-ISRS %>% ungroup() %>% 
  filter(role=='IS') %>% filter(NestType=='Cavity') %>%  
  filter(Species=='Red-breasted parakeet'|Species=='Rose-ringed parakeet'|
           Species=='Tanimbar corella'|Species=='Monk parakeet'|
           Species=='Yellow crested cockatoo'|Species=='Sulphur crested cockatoo') %>% 
  summarise(n=sum(n_ints))
y5<-round((y4/d)*100,2)
sprintf('Focal non native parrots initiated %s [%s%%] of all interactions',y4,y5)


rm(a,b,c,d,e,f,g,h,i,j,x,y,z,y2,y3,y4,y5,z2)


#============================#
# 9.d. Interaction Detail----
#============================#
ISRS %>% group_by(interaction) %>% 
  filter(role=='IS') %>% filter(interaction!='Neutral') %>% 
  summarise(n=sum(n_ints)) %>% mutate(freq=n/sum(n)*100)

ISRS %>% group_by(Species) %>%
  filter(interaction!='Neutral') %>% 
  summarise(n=sum(n_ints)) %>% mutate(freq=n/sum(n)*100) %>% arrange(desc(n))

ISRS %>% group_by(Species,outcome) %>%
  filter(interaction!='Neutral') %>% 
  summarise(n=sum(n_ints)) %>% mutate(freq=n/sum(n)*100)%>% 
  filter(outcome=='W') %>% arrange(desc(n))

ISRS %>% group_by(Species,outcome) %>%
  filter(interaction!='Neutral') %>% filter(role=='IS') %>% 
  summarise(n=sum(n_ints)) %>% mutate(freq=n/sum(n)*100)%>% 
  filter(outcome=='W') %>% arrange(desc(n))

#/////////////////////////
# Test for normality----

# Shapiro-wilk for small samples
## less than alpha 0.05 = not normal
## not less than alpha 0.05 = normal
shapiro.test(Indices_2$site.interactions) # normal
shapiro.test(Indices_2$Neutral) # normal
shapiro.test(Indices_2$Swoop) # normal
shapiro.test(Indices_2$Displace) # normal
shapiro.test(Indices_2$Threat) # normal
shapiro.test(Indices_2$Richness) # normal
shapiro.test(Indices_2$Shannon) # normal
shapiro.test(Indices_2$canopypc) # normal
shapiro.test(Indices_2$cavs_canopy_sqm) # normal, just about
skewness(Indices_2$canopypc)
kurtosis(Indices_2$canopypc)
skewness(Indices_2$Richness)
kurtosis(Indices_2$Richness)
# indices and survey site data are normally distributed

shapiro.test(ISRS$interaction) # not normal
shapiro.test(ISRS$ints_HR) # not normal
shapiro.test(sp.pairs$pair_ints) # not-normal
x<-Composition %>% sample_n(100)
shapiro.test(x$distance)  # not-normal
x<-Comp.max %>% filter(Study.Area!='Changi Airport')
ggplot(x,aes(max_obs))+geom_histogram() # not normal
x<-sp.pairs %>% ungroup() %>% 
  group_by(initsp,recipsp) %>% summarise(n=sum(pair_ints))
ggplot(x,aes(n))+geom_histogram() # not normal

skewness(x$n) # not normal
kurtosis(x$n)

# interaction data are measured ordinally
  # right skew
    # ///use: Spearmans-Rank correlation 

# Survey site data are normally distributed and can
x<-sp.pairs_2 %>% group_by()
z<-ggscatter(sp.pairs_2,'all_pair_ints','freq.I',
             add = 'reg.line',
             add.params = list(color = "blue", fill = "lightgray"),
             conf.int = TRUE)
z+stat_cor(method = 'spearman', label.x = 3, label.y = 30)

z<-x 
z<-cor.test(x$n_ints,
         x$max.freq,
            method='pearson')
z
# spearmans rho shows a positive association between
# n interactions and population density in a community

#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#=============================== CHARTS =================================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
# clean wrapped labels!!!!
ISRS$Species2 = str_wrap(ISRS$Species, width = 10)
Enviro_2$Study.Area2 = str_wrap(Enviro_2$Study.Area, width = 10)
Interact_2$Study.Area2 = str_wrap(Interact_2$Study.Area, width = 10)

# title styling
  # 90 deg rotated x axis
style90 <-  theme(plot.title = element_text(size=40,margin = margin(0,0,25,0)),
           axis.title.y = element_text(size=25,margin = margin(0,25,0,0)),
           axis.title.x = element_text(size=25,margin = margin(25,0,0,0)),
           axis.text.x = element_text(size=25,angle = 90, vjust = 0.5, hjust=1),
           axis.text.y = element_text(size = 25))
  # standard
style180 <-  theme(plot.title = element_text(size=20,margin = margin(0,0,25,0)),
                  axis.title.y = element_text(size=15,margin = margin(0,25,0,0)),
                  axis.title.x = element_text(size=15,margin = margin(25,0,0,0)),
                  axis.text.x = element_text(size=15, vjust = 0.5, hjust=1),
                  axis.text.y = element_text(size = 15))

style180Centered <-  theme(plot.title = element_text(size=20,margin = margin(0,0,25,0)),
                   axis.title.y = element_text(size=15,margin = margin(0,25,0,0)),
                   axis.title.x = element_text(size=15,margin = margin(25,0,0,0)),
                   axis.text.x = element_text(size=15),
                   axis.text.y = element_text(size = 15))

styleRA <-  theme(plot.title = element_text(size=20,margin = margin(0,0,25,0)),
                   axis.title.y = element_text(size=15,margin = margin(0,25,0,0)),
                   axis.title.x = element_text(size=15,margin = margin(25,0,0,0)))

#pilot

Pilot %>% 
  ggplot(aes(time,freq))+
  geom_col()+
  labs(x='Time of day',y='Interaction frequency',
       title = 'Pilot study interaction frequency')+
  theme_pubclean()+style90


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================== SURVEY SITES ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

## Alpha ----
# Shannon / Richness
Indices %>% 
  ggplot(aes(x=Shannon,y=Richness)) +
  geom_point(size=5,color='#4477AA')+
  xlim(1.5,4)+ylim(25,60)+
  labs(title = 'Alpha diversity index',
       x = 'Shannon Index', y='Species Richness')+
  geom_text_repel(aes(label=Study.Area),
                  nudge_y = 1.6,segment.color = NA,color='#555555',size=5)+
  theme_pubclean()+style180

# Simpson / Richness
Indices %>% 
  ggplot(aes(x=Simpson,y=Richness)) +
  geom_point(size=5,color='#4477AA')+
  labs(title = 'Alpha biodiversity indices',
       x = 'Simpson Index', y='Species Richness')+
  geom_text_repel(aes(label=Study.Area),
                  nudge_y = 1.6,segment.color = NA,color='#555555',size=5)+
  theme_pubclean()+style180


  # Table form
formattable(Indices) 

## Changi and Stirling/QT have the lowest BD and richness scoring
## highly urbanised areas 

## Springleaf, old plantation and secondary forest with canal
## some urbanisation surrounding but also close to central catchment NR

## Palawan beach is quite busy but low urbanisation. Surrounding area rich in
## dense secondary forest with old trees and little or no maintenance

## Sengkang / PRTP are managed urban parks but quite rich in vegatation. 
## food resources are plentiful


## Beta----
# Sorensen index
sorensen.plot<-sorensen.melted %>%
  ggplot(aes(x=Var1, y=Var2, fill = value))+
  geom_tile(color = "white")+
  labs(title = 'Beta diversity index (Sorensen)')+
    theme_pubclean()+
  theme(legend.title = element_blank(),
        axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.2,size=14),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22))+
    scale_fill_gradient2(low = "#EAECCC", high = "#364B9A",
                       mid="grey", midpoint = 0.5, limit = c(0,1),
                       breaks=c(0,0.5,1),
                       labels=c('No overlap','0.5','Max. overlap'))+
  geom_text(aes(label = value), color = "black", size = 6)+ #Adding values of Jaccard distance
  xlab("")+ylab("") #Removing the labels (x and y axis)
sorensen.plot

# higher value = greatest overlap / similarity
## Changi and stirling high similarity, PRTP not too dissimilar
## Airport has low overlap

## Land-use types----
Enviro_2 %>% 
  ggplot(aes(Study.Area,proportion,fill=land_prop))+
  geom_col(position='fill')+
  labs(y='Proportion of land cover',x='Study Area',
       title = 'Land type per study site')+
  scale_x_discrete(labels = function(Study.Area2) str_wrap(Study.Area2, width = 10))+
  scale_fill_discrete(name='Area use',labels=c('Buildings','Artificial surface',
                                               'Natural surface','Other vegetation',
                                               'Canopy','Waterbody','Mangrove'))


## RA curve----
x<-Composition_2 %>% 
  select(Study.Area,Species,max_obs,max.freq,SG_status) %>% 
  group_by(Study.Area) %>% 
  mutate(rank=dense_rank(desc(max.freq))) %>% 
  arrange(Study.Area,rank) %>% 
  group_by(Study.Area) %>%
  mutate(id = row_number()) %>% 
  mutate('Species status'=case_when(SG_status=='I'~'Introduced',
                              SG_status=='R'~'Resident',
                              SG_status=='M'~'Migrant/Visitor',
                              SG_status=='N'~'Migrant/Visitor',
                              SG_status=='V'~'Migrant/Visitor')) 
x$rank<-as.character(x$rank)
x$`Species status`<-factor(x$`Species status`,levels=c('Resident','Introduced','Migrant/Visitor'))

# proportional
x %>% filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(id,max.freq,color=`Species status`))+
  geom_point(shape=1,alpha=1)+
  geom_line(alpha=1,position = position_dodge(width=0.1))+
  facet_wrap(~Study.Area)+
  labs(title = 'Rank abundance curves',
       x='Rank',y='Abundance (percent)')+
  scale_x_continuous(limits = c(1, 54), 
                     breaks = c(1,5,10,15,20,25,30,35,40,45,50,54))+
  theme_pubclean()+styleRA+
  scale_colour_manual(values=c('Introduced'='#EE6677','Resident'='#4477AA',
                               'Migrant/Visitor'='#CCBB44'))
# freq diff style
x %>% filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(id,max.freq,color=`Species status`))+
  geom_col(fill='white')+
  facet_wrap(~Study.Area)+
  labs(title = 'Rank abundance curves',
       x='Rank',y='Abundance (proportion)')+
  scale_x_continuous(limits = c(0, 54), 
                     breaks = c(1,5,10,15,20,25,30,35,40,45,50,54))+
  theme_pubclean()+styleRA+
  theme(legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        strip.text = element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))+
  scale_colour_manual(values=c('Introduced'='#EE6677','Resident'='#4477AA',
                               'Migrant/Visitor'='#CCBB44'))

# n actual & diff style
x %>% filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(id,max_obs,color=`Species status`))+
  geom_col(fill='white')+
  facet_wrap(~Study.Area)+
  labs(title = 'Rank abundance curves',
       x='Rank',y='Abundance (n)')+
  scale_x_continuous(limits = c(0, 54), 
                     breaks = c(1,5,10,15,20,25,30,35,40,45,50,54))+
  theme_pubclean()+styleRA+
  theme(legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        strip.text = element_text(size=16))+
  scale_colour_manual(values=c('Introduced'='#EE6677','Resident'='#4477AA',
                               'Migrant/Visitor'='#CCBB44'))
# residency summary
x<-Composition_2 %>% 
  drop_na(SG_status) %>% 
  select(Study.Area,Species,max.freq,SG_status) %>% 
  mutate(Status=case_when(SG_status=='I'~'Introduced',
                                    SG_status=='R'~'Resident',
                                    SG_status=='M'~'Migrant/Visitor',
                                    SG_status=='N'~'Migrant/Visitor',
                                    SG_status=='V'~'Migrant/Visitor')) %>% 
  group_by(Study.Area,Status) %>% 
  summarise(n=sum(max.freq),
            n2=n_distinct(Study.Area,Species,Status))

x$Status<-factor(x$Status,levels=c('Resident','Introduced','Migrant/Visitor'))

  x %>% 
  ggplot(aes(Study.Area,n,fill=Status))+
  geom_col(position = 'fill') +
  labs(y='Proportion of community',
       title = 'Proportion of resident / non-native / migratory species')+
    scale_fill_manual(values=c('Introduced'='#EE6677','Resident'='#4477AA',
                                 'Migrant/Visitor'='#CCBB44'))+
    theme_pubclean()+
  theme(axis.title.x = element_blank())
  
describeBy(x,x$Status)  #two grouping variables  

# non native / introduced species are most prevalent in low biodiversity areas

## Species table----
x %>%
  select(-n2) %>% 
    spread(key = Study.Area,value=n) %>% 
    mutate(across(where(is.numeric), round, 2)) %>%
    rename("Status/Site"='Status') %>% 
    replace(is.na(.), 0) %>% 
    kable(align = 'lccccccc') %>% 
    kable_styling()

## Profile table----
Indices_2 %>% 
  group_by(Study.Area) %>% 
  mutate('Built area'=sum(buildpc+artsurfacepc), #28
         'Natural area'=sum(canopypc+Vegpc+natsurfacepc), #29
         'Water area'=sum(waterpc+mangrovepc)) %>%  #30
  select(1,3,5,6,28,29,30,
         17,19,18,16,27) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  rename(" "=Study.Area,
         'Survey area'='Site habitat types',
         'Surrounding area'='Surrounding habitat types',
         '% non-native spp.'=freq.I,
         '% cavity-nester spp.'=cav.sp.freq,
         '% parrot-spp.'=freq.parrots,
         'Cavities/sqm'=cavs_canopy_sqm,
         'Interspecific interactions'=n_ints
         ) %>% 
  arrange(desc(Shannon)) %>% 
  kable(align = 'lcllcccccccc') %>% 
  add_header_above(header=c(" "=2,
                            "Habitat description"=2,
                            "Land-use"=3,
                            "Occupying species"=3,
                            " "=2)) %>% 
  kable_styling()

#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#=============================== NSS DATA  ================================
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

# Pop trend----
NSS %>% 
  group_by(Year,Species) %>% 
  summarise(n=sum(Count)) %>% 
  ggplot(aes(Year,n,color=Species))+
  geom_point()+
  geom_smooth(se=F)+
  facet_wrap(~Species,scale='free')+
  labs(x='Year',y='Observations',title = 'NSS Parrot count')+
  theme(legend.position = 'none')

## MP not documented at all
## RBP continuous growth but populations may be reaching capacity
## RRP growth is high, potential to explode
## TC signifcant growth but fairly stable for several years

# PC / Study compare----

x<-Composition_2 %>% 
  select(Study.Area,Species,max_obs) %>% 
  filter(Species=="Monk parakeet"|Species=="Red-breasted parakeet"|Species=="Tanimbar corella"|
           Species=="Long-tailed parakeet"|Species=="Rose-ringed parakeet"|
           Species=='Yellow crested cockatoo'|Species=='Sulphur crested cockatoo')
x<-x %>% mutate(source='Study 2022')

y<-NSS %>% 
  filter(Year=='2019') %>% 
  select(Study.Area,Species,Count) %>% 
  rename(max_obs=Count) %>% 
  filter(Study.Area=='Sengkang Riverside Park'|Study.Area=='Springleaf'|
           Study.Area=='Changi Village'|
           Study.Area=='Palawan Beach') %>% 
  mutate(source='NSS 2019')
rm(z)
z<-merge(x,y,by=c('Study.Area','Species','max_obs','source'),all=T)

z %>% 
  filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(source,max_obs,fill=Species))+
  geom_col()+
  facet_wrap(~Study.Area)+
  labs(y='number of individuals',x='Survey & Year',
       title = 'Parrot population comparison 2019-2022')


#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#======================== TOP-LINE CORRELATIONS =========================
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////


#========================#
# SITE----
#========================#
Indices_2 %>% 
  group_by(Study.Area) %>% 
  (built_surf=sum(buildpc+artsurfacepc)) %>% 
  ggplot(aes(built_surf,Shannon))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Proportion urban land cover',y='Shannon')
# lm
x<-Indices_2 %>% 
  group_by(Study.Area) %>% 
  mutate(built_surf=sum(buildpc+artsurfacepc))
y<-lm(Shannon~built_surf,x)
y<-aov(Shannon~built_surf,x)
summary(y)
summary.aov(y)
# R² = .894, F(1,4)=33.75,p<0.00437)
# Urbanised land predicted reduced bird species richness (β = -38.367, p < .00437)


## BD and Richness declines with building and road cover
Indices_2 %>% 
  group_by(Study.Area) %>% 
  mutate(vegcan=sum(canopypc+Vegpc)) %>% 
  ggplot(aes(vegcan,Shannon))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Proportion greenery cover',y='Shannon')

# Linear models for all land types
x<-Indices_2 %>% 
  group_by(Study.Area) %>% 
  mutate(builtarea=sum(buildpc+artsurfacepc),
         greenarea=sum(canopypc+Vegpc))
y<-lm(Shannon~buildpc,GLMdata_scale)
summary(y)                        

#xxxxxxxxxxxxxxxxxx

Indices_2 %>% 
  ggplot(aes(Shannon,freq.parrots))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()
y<-aov(Richness~built_surf,x)

# CAVITY / NON NATIVES
Indices_2 %>% 
  filter(Study.Area!='Changi Airport') %>% 
  group_by(Study.Area) %>% 
  ggplot(aes(cavs_canopy_sqm,freq.I))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Proportion urban land cover',y='Shannon')
# lm
x<-Indices_2 %>% 
  filter(Study.Area!='Changi Airport')
y<-lm(freq.I~cavs_canopy_sqm,x)
summary(y)

# R² = .894, F(1,4)=33.75,p<0.00437)
# Urbanised land predicted reduced bird species richness (β = -38.367, p < .00437)

   
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================== INTERACTIONS ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

# obs / ints point
Composition_2 %>% 
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
  group_by(Species) %>% 
  summarise(max_obs=sum(max_obs),
            n_ints=sum(n_ints)) %>% 
  ggplot(aes(max_obs,n_ints,color=Species))+
  geom_jitter(height = 6,alpha=.8,size=3)+
  coord_trans(x='log10')+
  theme_pubclean()+
  labs(x='Individuals observed',y='Total number of interactions',
       title = 'Total observations and interactions')+
  scale_colour_manual(values=c('Red-breasted parakeet'='#CC3311',
                             'Monk parakeet'='#009988',
                             'Rose-ringed parakeet'='#EE3377',
                             'Tanimbar corella'='#EE7733',
                             'Yellow crested cockatoo'='#33BBEE',
                             'Sulphur crested cockatoo'='#0077BB'))+
  scale_y_continuous(limits = c(0, 300), oob = scales::squish)+style180
# majority of all observed sp. in study areas were not in parrot interaction networks

#/////////////////////////
# THIS ONE
  # sp involved in parrot interaction networks
Composition_2 %>% 
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
  filter(Species!="Monk parakeet"&Species!="Tanimbar corella"&
           Species!="Rose-ringed parakeet"&Species!="Red-breasted parakeet") %>%
  group_by(Species) %>% 
  summarise(max_obs=sum(max_obs),
            n_ints=sum(n_ints)) %>% 
  filter(n_ints>1) %>% 
  filter(max_obs>0) %>% 
  ggplot(aes(max_obs,n_ints))+
  geom_jitter(height = 4,width=1,alpha=.8,color='#CC3311',size=3)+
  #coord_trans(x='log10')+
  stat_poly_eq()+
  stat_poly_line()+
  theme_pubclean()+
  labs(x='Individuals observed',y='Total number of interactions',
       title = 'Aggressive interactions within focal species network')+
  geom_text_repel(data=Composition_2 %>% 
                    filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
                    filter(Species!="Monk parakeet"&Species!="Tanimbar corella"&
                             Species!="Rose-ringed parakeet"&Species!="Red-breasted parakeet") %>%
                    group_by(Species) %>% 
                    summarise(max_obs=sum(max_obs),
                              n_ints=sum(n_ints)) %>% 
                    filter(n_ints>18) %>% 
                    filter(max_obs>5),aes(label=Species),
                  nudge_y=4,size=3,
                  box.padding = 0.5, max.overlaps = Inf)+
  scale_y_continuous(limits = c(0, 200), oob = scales::squish)+style180

x<-Composition_2 %>% 
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
  filter(Species!="Monk parakeet"&Species!="Tanimbar corella"&
           Species!="Rose-ringed parakeet"&Species!="Red-breasted parakeet")  %>%
  group_by(Species) %>% 
  summarise(max_obs=sum(max_obs),
            n_ints=sum(n_ints))
# dependent = n ints
y<-lm(n_ints~max_obs,x)
  summary(y)

# species within the regular interaction network
## of the focal non-native parrots

# n interactions raw
ISRS %>% 
  filter(role!='IS') %>%
  group_by(Study.Area) %>% 
  summarise(n=sum(n_ints)) %>% 
  filter(n>2) %>% 
  ggplot(aes(Species,n))+
  geom_col()+
  geom_text(aes(label = n), hjust = -0.5)+
  coord_flip()+
   labs(x='Species',y='n',title='Total interactions')
# 
296+264+133+132+128
256+196+101+99+85
ISRS %>% 
  filter(role=='RS') %>% 
  summarise(n=sum(n_ints))
sp.pairs %>% 
  summarise(n=sum(pair_ints))



#============================================================#
## interaction frequency standardised based on MAX observations
#============================================================#
#library(tidytext)
x3<-Composition_2 %>%
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% # remove outliers
    group_by(Species,SG_status,NestType) %>% summarise(max_obs=sum(max_obs),
                                  n_ints=sum(n_ints),
                                  n_ints_xNE=sum(n_ints_xNE),
                                  n_initis=sum(n_initis),
                                  n_initis_xNE=sum(n_initis_xNE)) %>% 
  mutate(ints_freq=n_ints/max_obs,
         intsxNE_freq=n_ints_xNE/max_obs,
         initis_freq=n_initis/max_obs,
         initisxNE_freq=n_initis_xNE/max_obs) %>% 
  mutate(label=case_when(SG_status=='I'&NestType=='Cavity'~'I cavity nester',
                         SG_status=='I'&NestType=='Non-cavity'~'I non-cavity nester',
                         SG_status=='I'&NestType=='Cavity-optional'~'I optional-cavity nester',
                         SG_status=='R'&NestType=='Cavity'~'R cavity nester',
                         SG_status=='R'&NestType=='Non-cavity'~'R non-cavity nester',
                         SG_status=='R'&NestType=='Cavity-optional'~'R optional-cavity nester',
                         SG_status=='M'&NestType=='Cavity'~'M cavity nester',
                         SG_status=='M'&NestType=='Non-cavity'~'M non-cavity nester',
                         SG_status=='M'&NestType=='Cavity-optional'~'M optional-cavity nester'))
x3 %>% 
  ggplot(aes(max_obs,ints_freq))+
  geom_jitter(height = 4,width=1,alpha=.8,color='#CC3311',size=2)+
  #coord_trans(x='log10')+
  stat_poly_eq()+
  stat_poly_line()+
  theme_pubclean()

# All interactions / obs max----

x3 %>% 
  filter(max_obs>0&n_ints>1) %>% 
  ggplot(aes(reorder(Species,ints_freq),ints_freq,fill=label))+
  geom_col(alpha=.9)+
  coord_flip()+
  labs(y= 'Interaction frequency',
       x= 'Species',
       title = 'Number of interactions standardised by maximum observed individuals')+
  theme_pubclean()+
  style180+
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(size=13))+
  scale_fill_manual(values=c('I cavity nester'='#88CCEE',
                             'I non-cavity nester'='#44AA99',
                             'R cavity nester'='#DDCC77',
                             'R non-cavity nester'='#999933',
                             'R optional-cavity nester'='#117733',
                             'M non-cavity nester'='#CC6677'))
                    
                    
                    

# /// THIS ONE///
# N INTERACTIONS / obs ----
x3 %>% 
  ungroup() %>% 
  top_n(20,n_ints) %>% 
    ggplot(aes(reorder(Species,n_ints),n_ints,fill=label))+
  geom_col(alpha=.9)+
  coord_flip()+
  labs(y= 'Number of interactions',
       x= 'Species',
       title = 'Total interactions - Top 20 species')+
  theme_pubclean()+
  style180+
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(size=13))+
  scale_fill_manual(values=c('I cavity nester'='#88CCEE',
                             'I non-cavity nester'='#44AA99',
                             'R cavity nester'='#DDCC77',
                             'R non-cavity nester'='#999933',
                             'R optional-cavity nester'='#117733',
                             'M non-cavity nester'='#CC6677'))

# /// THIS ONE///
# FREQ INITIATIONS / obs ----
x3 %>% 
  ungroup() %>% 
  top_n(20,ints_freq) %>% 
  ggplot(aes(reorder(Species,ints_freq),ints_freq,fill=label))+
  geom_col(alpha=.9)+
  coord_flip()+
  labs(y= 'Interaction frequency',
       x= 'Species',
       title = 'Interaction frequency / observed individuals - Top 20 species')+
  theme_pubclean()+
  style180+
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(size=13))+
  scale_fill_manual(values=c('I cavity nester'='#88CCEE',
                             'I non-cavity nester'='#44AA99',
                             'R cavity nester'='#DDCC77',
                             'R non-cavity nester'='#999933',
                             'R optional-cavity nester'='#117733',
                             'M non-cavity nester'='#CC6677'))


 ## normalised\\\\\\\\\\\\
x3.norm<-x3 %>% filter(!is.na(initis_freq))
x3.norm<-x3.norm %>% filter(!is.na(label))
x3.norm$label<-as.factor(x3.norm$label)
x3.norm<-x3.norm %>% ungroup() %>% 
  mutate_if(is.numeric, funs(as.numeric(scale(.))))

x3.norm %>% ggplot(aes(n_ints))+geom_histogram()# binomial

y<-gam(ints_HR~s(max_obs),data=x3,family = 'poisson')
summary(y)

plot(y,rugplot = T, se = T)


# interesting----
x3.norm %>% 
  filter(max_obs!=0) %>% 
  ggplot(aes(max_obs,n_ints,color=label))+
  geom_point(size=2)+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Observed individuals',y='Total number of interactions',
       title = 'Total number of interactions plotted against the total number of individuals')+
  theme_pubclean()+style180

# n individuals is not a strong predictors that interactions will happen


x3 %>% 
  #filter(max_obs>0&n_initis>0) %>% 
  ggplot(aes(max_obs,ints_freq))+
  geom_point(size=4)+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Observed individuals',y='Number of / population',
       title = 'Total number of interactions standardized by population size')+
  theme_pubclean()+style180

x3.norm<-x3 %>% filter(!is.na(initis_freq))
x3.norm<-x3.norm %>% filter(!is.na(label))
x3.norm$label<-as.factor(x3.norm$label)
x3.norm<-x3.norm %>% ungroup() %>% 
  mutate_if(is.numeric, funs(as.numeric(scale(.))))

x3.norm %>% ggplot(aes(n_ints))+geom_histogram()#
y<-gam(as.factor(ints_freq)~max_obs,data=x3,family = 'binomial')
summary(y)

##########

#2. ROLES
# n IS RS
ISRS %>%
  filter(interaction!='Neutral') %>% 
  filter(sp_lab=='NNP'|Species=='Long-tailed parakeet') %>%  
  group_by(Species,role) %>% 
  summarise(n=sum(n_ints)) %>% 
  ggplot(aes(reorder(Species,-n),n,fill=role))+
  geom_col(position = 'stack')+ 
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  labs(x='Species',y='n',title='n interactions intiated and recieved')+
  scale_fill_manual(values=c('IS'='#994455','RS'='#EE99AA'))+
  theme_pubclean()+style180Centered

# % IS RS
ISRS %>%  
  filter(interaction!='Neutral') %>% 
  filter(sp_lab=='NNP'|Species=='Long-tailed parakeet') %>%  
  group_by(Species,role) %>% 
  summarise(n=sum(n_ints)) %>%
  filter(n!=0) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(Species,-n),freq,fill=role))+
  geom_col(position = 'fill')+theme_minimal()+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  labs(x='Species',y='Proprotion of interactions',title='Proportion aggressive interactions intiated and recieved')+
  scale_fill_manual(values=c('IS'='#994455','RS'='#EE99AA'))+
  theme_pubclean()+style180Centered

# 3. W/L/NE summary
# n W/L/NE all ints

ISRS %>% 
  group_by(Species,outcome) %>% 
  filter(interaction!='Neutral') %>% 
  filter(sp_lab=='NNP'|Species=='Long-tailed parakeet') %>%  
  summarise(n=sum(n_ints)) %>% 
  filter(n!=0) %>% 
  ggplot(aes(reorder(Species,-n),n,fill=outcome))+
  geom_col(position = 'stack')+ 
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  labs(x='Species',y='Number of interactions',title='Wins and Losses')+
  scale_fill_manual(values=c('W'='#4393c3','L'='#d6604d'))+
  theme_pubclean()+style180Centered

# % W/L/NE all ints
ISRS %>% 
  group_by(Species,outcome) %>% 
  filter(interaction!='Neutral') %>% 
  filter(sp_lab=='NNP'|Species=='Long-tailed parakeet') %>%  
  summarise(n=sum(n_ints)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(Species,-n),freq,fill=outcome))+
  geom_col(position = 'fill')+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='Proportion of interactions',title='Wins and Losses')+
  scale_fill_manual(values=c('W'='#4393c3','L'='#d6604d'))+
  theme_pubclean()+style180Centered


ISRS %>% 
  filter(outcome!="NE") %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
    group_by(Species,role,outcome) %>% 
  summarise(n=sum(n_ints)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(Species,-n),freq,fill=outcome))+
  geom_col(position = 'fill')+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#4393c3','L'='#d6604d'))+
  facet_wrap(~role)

ISRS %>% 
  filter(outcome!="NE") %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  group_by(Species,outcome) %>% 
  summarise(n=sum(n_ints)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(Species,-n),freq,fill=outcome))+
  geom_col(position = 'fill')+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#4393c3','L'='#d6604d'))

# actual n ints----
ISRS %>% 
  filter(role=='IS') %>% group_by(Species) %>% 
  #filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  ggplot(aes(interaction,n_ints))+geom_col(width=0.95)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)+
  theme(legend.position = 'none')+labs(y='n_ints',x='interaction',title='Interaction distribution: positively skewed')#+
  #facet_wrap(~Species)

# relative freq (%)
ISRS %>% 
  filter(role=='IS') %>% group_by(Species) %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  mutate(freq=n_ints/sum(n_ints)*100) %>% 
  ggplot(aes(interaction,freq))+geom_col(width=0.95)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)+
  theme(legend.position = 'none')+labs(y='relative frequency',x='interaction',title='Interaction distribution: positively skewed')+
  facet_wrap(~Species)

ISRS %>% 
  filter(interaction!='Neutral') %>% 
  filter(role=='IS') %>% group_by(Species) %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|
           Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet") %>%  
  mutate(freq=n_ints/sum(n_ints)*100) %>% 
    ggplot(aes(Species,freq,fill=Species))+
  geom_col(alpha=0.9)+
  labs(title = 'Frequency of ininitated interactions per species',
       y='Frequency')+
  facet_wrap(~interaction)+
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  theme_pubclean()+
  style180Centered+
    theme(axis.title.x = element_blank(),
          legend.position = 'none',
          strip.text = element_text(size=15))+
                             'Red-breasted parakeet'='#8A8A8A',
                             'Rose-ringed parakeet'='#575757',
                             'Tanimbar corella'='#000000'))
  
Interact_2 %>% 
  filter(initsp=='Tanimbar corella') %>% 
  filter(interaction=='Swoop') %>% 
  count(recipsp) %>% 
  mutate(f=n/sum(n)*100)

# Site difference x species----
ISRS %>% 
  filter(role=='IS') %>% group_by(Species) %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  filter(Study.Area!='Pasir Ris Town Park'|Species!='Red-breasted parakeet') %>% 
  filter(Study.Area!='Stirling Road'|Species!='Rose-ringed parakeet') %>% 
  ggplot(aes(Study.Area,fill=interaction,label=n_ints))+
  geom_bar(position='fill',width=0.9)+
  facet_grid(~Species,scales='free',space='free')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = 'Species aggressions across sites')

# Tanimbar corella 
  ## aggression across two separate sites is identical in frequency.
  ## most frequent contact / fight

# RRP
  ## also more often than other species to fight / contact 

# distance from nest----

# by spp. box
a<-Interact_2 %>% 
  filter(interaction!='Neutral') %>% 
  filter(nxt_cav<100) %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet")%>%  
  ggplot(aes(initsp,nxt_cav))+
  geom_boxplot(outlier.colour = 'red',outlier.shape = 1,outlier.size = 2)+
  geom_jitter(width=0.2,height=1.5,alpha=0.2,size=3,shape=20,color='#0077BB')+
    labs(y='Distance from cavity or roost (metres)',title='A) by species')+
  theme_pubclean()+style180Centered+
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  theme(axis.title.x = element_blank(),
        plot.title = element_text(vjust = -5))
# by interaction
b<-Interact_2 %>% 
  filter(interaction!='Neutral') %>% 
  filter(nxt_cav<100) %>% 
  #filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet")%>%  
  ggplot(aes(interaction,nxt_cav))+
  geom_boxplot(outlier.colour = 'red',outlier.shape = 1,outlier.size = 2)+
  geom_jitter(width=0.2,height=1.5,alpha=0.2,size=3,shape=20,color='#0077BB')+
  labs(y='Distance from cavity or roost (metres)',title='B) by interaction type')+
  theme_pubclean()+style180Centered+
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(vjust = -5))
# by study Area box
c<-Interact_2 %>% 
  filter(interaction!='Neutral') %>% 
  filter(nxt_cav<100) %>% 
  #filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet") %>%  
  ggplot(aes(Study.Area,nxt_cav))+
  geom_boxplot(outlier.colour = 'red',outlier.shape = 1,outlier.size = 2)+
  geom_jitter(width=0.2,height=1.5,alpha=0.2,size=3,shape=20,color='#0077BB')+
  labs(y='Distance from cavity or roost (metres)',title='C) by site')+
  theme_pubclean()+style180Centered+
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  theme(axis.title.x = element_blank(),
        plot.title = element_text(vjust = -5))
# by outcome
d<-Interact_2 %>% 
  filter(interaction!='Neutral') %>% 
  filter(nxt_cav<100) %>% 
  #filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet") %>%  
  ggplot(aes(isout,nxt_cav))+
  geom_boxplot(outlier.colour = 'red',outlier.shape = 1,outlier.size = 2)+
  geom_jitter(width=0.2,height=1.5,alpha=0.2,size=3,shape=20,color='#0077BB')+
  labs(y='Distance from cavity or roost (metres)',title='D) by outcome')+
  theme_pubclean()+style180Centered+
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(vjust = -5))

grid.arrange(a,b,c,d,top=textGrob("Interaction distance from cavity or roost",
                                  gp=gpar(fontsize=22),vjust=0.2))




# Site profile
Indices_2 %>% 
  filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(Study.Area,site.interactions))+
  geom_boxplot()+
  labs(y='n',title='Total interactions')+
  theme_pubclean()+style180Centered+
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  theme(axis.title.x = element_blank())
Indices_2 %>% 
  filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(Study.Area,cavs_canopy_sqm))+
  geom_boxplot()+
  labs(y='n',title='Nest holes per sqm')+
  theme_pubclean()+style180Centered+
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  theme(axis.title.x = element_blank())
Indices_2 %>% 
  filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(Study.Area,freq.I))+
  geom_boxplot()+
  labs(y='%',title='Proportion of introduced species')+
  theme_pubclean()+style180Centered+
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  theme(axis.title.x = element_blank())
Indices_2 %>% 
  filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(Study.Area,freq.parrots))+
  geom_boxplot()+
  labs(y='%',title='Proportion of parrot species')+
  theme_pubclean()+style180Centered+
  scale_x_discrete(labels = function(Species2) str_wrap(Species2, width = 10))+
  theme(axis.title.x = element_blank())




# negative bionomial reg
x<-ISRS %>% filter(!is.na(interaction))
str(ISRS)
mean(ISRS$n_ints);var(ISRS$n_ints)
# mean and variance are nowhere close so poisson is not usable
## data = overdispersed

x %>% 
  ggplot(aes(n_ints))+
  geom_bar()+facet_wrap(~Study.Area)
# data firs poisson distribution
x %>% 
  ggplot(aes(n_ints))+
  geom_bar()+facet_wrap(~NestType)
# data firs poisson distribution
x %>% 
  ggplot(aes(n_ints))+
  geom_bar()+facet_wrap(~Species)
# data firs poisson distribution

# actual model
y<-glm.nb(site.interactions ~ sp_lab, data = x)
summary(y)


#////////////////////////////////
# Standardize and LM ----
#////////////////////////////////

#////////////////////////////////
## with sp.pairs ----
#////////////////////////////////

#https://www.guru99.com/r-generalized-linear-model.html
# dev.off()

# select continuous variables
continuous <-select_if(sp.pairs, is.numeric)
summary(continuous)
# remove major outliers from size (mammals)
top_one_RS.size <- quantile(sp.pairs$RS.size, .99)
top_one_IS.size <- quantile(sp.pairs$IS.size, .99)

GLMdata <-sp.pairs %>%
  filter(RS.size<top_one_RS.size) %>% 
  filter(IS.size<top_one_IS.size)
rm(top_one_IS.size)
rm(top_one_RS.size)
#ranking for the normalization
GLMdata<-GLMdata %>% 
  mutate(rating=case_when(
  interaction=="Neutral"~"0", # no aggression
  interaction=="Displace"~"1", # remove from perch by landing
  interaction=="Threat"~"2", # vocal or flaring threats
  interaction=="Swoop"~"3", # fly within 60cm at species but not landing
  interaction=="Chase"~"4", # in-flight pursuit may or may not displace
  interaction=="Contact"~"5", # physical contact
  interaction=="Fight"~"5"))# multiple physical contact
GLMdata$rating<-as.numeric(GLMdata$rating)
GLMdata<-GLMdata %>% relocate(rating,.after = interaction)
GLMdata<-GLMdata %>% select(-IS.wt,-RS.wt,-`Site habitat types`,-`Surrounding habitat types`)

#Calculated columns
GLMdata<-GLMdata %>% 
  group_by(Study.Area) %>% 
  mutate(totalbuilt=sum(buildpc+artsurfacepc),
         totalgreen=sum(canopypc+Vegpc+natsurfacepc)) %>% 
  ungroup()

# STANDARDIZE
GLMdata_scale <- GLMdata %>% 
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
# gave an error but still worked

str(GLMdata_scale)

# factorise
GLMdata_scale$IS.NestType<-factor(GLMdata_scale$IS.NestType)
GLMdata_scale$RS.NestType<-factor(GLMdata_scale$RS.NestType)
GLMdata_scale$IS.sp_lab<-factor(GLMdata_scale$IS.sp_lab)
GLMdata_scale$RS.sp_lab<-factor(GLMdata_scale$RS.sp_lab)
GLMdata_scale$IS.status<-factor(GLMdata_scale$IS.status)
GLMdata_scale$RS.status<-factor(GLMdata_scale$RS.status)

# how many factor cols
factor <- data.frame(select_if(GLMdata_scale, is.factor))
ncol(factor)
# = 12
# Create graph for each column
graph <- lapply(names(factor),
                function(x) 
                  ggplot(factor, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))
graph

# remove NA's
GLMdata_scale<-GLMdata_scale %>% filter(!is.na(RS.status))
#run again from factor to check

# plots----
GLMdata_scale %>% 
  filter(IS.sp_lab=='NNP') %>% 
  filter(interaction!='Neutral') %>% 
  ggplot(aes(initsp,fill=interaction))+
  geom_bar(position = 'fill')

GLMdata_scale %>% 
  filter(IS.sp_lab=='NNP') %>% 
  filter(interaction!='Neutral') %>% 
  ggplot(aes(initsp,RS.size))+
  geom_boxplot()+
  stat_summary(fun='mean',color='red',shape=15)

# Recipient target species
  # first mean parrot sizes
highlight<-GLMdata_scale %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%
 group_by(initsp) %>% 
   summarise(RS.size=mean(IS.size))
highlight
  # now look at target species distribution by size
GLMdata_scale %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  ggplot(aes(RS.size)) +
  geom_density(aes(color=isout))+
  geom_vline(data=highlight,aes(xintercept=RS.size))+
  theme_pubclean()+
  facet_wrap(~initsp)+
  labs(y='Interaction frequency',
       x='Rise of recipient species',
       title='Frequency of interaction by type and recipient size')
## line shows the mean size of the intiator
  # -2 is the smallest birds 
  # +4 is the largest, herons, eagles
  # OPH is around 3 - can see the bump

# shannon standard----
GLMdata_scale %>% 
  group_by(Study.Area) %>% 
  summarise(Shannon=mean(Shannon),
            Richness=mean(Richness)) %>% 
  filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(x=Shannon,y=Richness)) +
  geom_point(size=5,color='#4477AA')+
  labs(title = 'Alpha biodiversity indices',
       x = 'Species diversity (Shannon)', y='Species Richness')+
  geom_text_repel(aes(label=Study.Area),
                  nudge_y = 0.16,segment.color = NA,color='#555555',size=5)+
  theme_pubclean()+style180+
  scale_x_continuous(limits = c(-1.5,1.5),
                     breaks = c(-1.5,-1.0,-0.5,0,0.5,1,1.5))+
  scale_y_continuous(limits = c(-1.5,2.5),
                     breaks = c(-1.5,-0.5,0.5,1.5,2.5))

# looking at size diff in initiated interactions
GLMdata_scale %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  ggplot(aes(size_diff)) +
  geom_density(aes(color=isout))+
  geom_vline(data=highlight,aes(xintercept=RS.size))+
  theme_pubclean()+
  facet_wrap(~initsp)+
  labs(y='Interaction frequency',
       x='Size difference of recipient species',
       title='Frequency of interaction and difference in recipient size')
## LTP & RBP have high n W/L against similar size = each other!

GLMdata_scale %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  ggplot(aes(size_diff)) +
  geom_density()+
  geom_vline(data=highlight,aes(xintercept=RS.size))+
  theme_pubclean()+
  facet_wrap(~initsp)+
  labs(y='Interaction frequency',
       x='Size difference of recipient species',
       title='All interactions by difference in size')
## TC have a broader interaction network
## RRP mostly interacting with species smaller
## MP mostly larger
## RBP and LTP skews toward smaller, not as much as RRP


# Looking at the link between parrot population density and environment
GLMdata_scale %>% 
  #filter(Study.Area!='Changi Airport') %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  ggplot(aes(Shannon)) +
  geom_density(aes(fill = initsp,color=initsp), alpha = 0.2) +
  theme_pubclean()
# TC more often found is low BD areas,
# RBP have a wide distribution
# LTP is higher BD areas
GLMdata_scale %>% 
  #filter(Study.Area!='Changi Airport') %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
    ggplot(aes(canopypc)) +
  geom_density(aes(color = initsp), alpha = 0.5) +
  theme_pubclean()+
  facet_wrap(~initsp)
# Variation of canopy cover within the sites is not a major factor
# these birds like open forest
# native LTP were seen most in one site which had the greatest canopy and veg
  # here also high aggression with RBP
GLMdata_scale %>% 
  #filter(Study.Area!='Changi Airport') %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  ggplot(aes(buildpc)) +
  geom_density(aes(color = initsp), alpha = 0.5) +
  theme_pubclean()+
  facet_wrap(~initsp)
# this also shows no strong preference amongst RBP, MP or RRP
# TC seem toprefer more built areas, perhaps this is the niche they can best 
  # exploit, there is little competition from birds of a similar size


#///////////////////////////////////
# Correllation matrix----
#//////////////////////////////////

## BLUE == NEGATIVE CORR.
## RED == POSITIVE CORR
## WHITE == NO CORR

#///////////////////////////////////
# SITES GENERAL
#//////////////////////////////////
library(GGally)
# Convert data to numeric
corr.site <- data.frame(lapply(GLMdata_scale, as.integer))
corr.site<-corr.site %>% select(Richness,Shannon,rating,site.interactions,cav.sp.freq,freq.I,freq.parrots,
                                n_cavity,cavs_canopy_sqm,canopypc,Vegpc,natsurfacepc,buildpc,
                                artsurfacepc,waterpc,mangrovepc) %>% 
  rename('Shannon index'=Shannon,
         'Total interactions/site'=site.interactions,
         'pc cavity nester spp.'=cav.sp.freq,
         'pc introduced spp.'=freq.I,
         'pc parrot spp.'=freq.parrots,
         'n cavities/site'=n_cavity,
         'cavity density/sqm'=cavs_canopy_sqm,
         'pc canopy cover'=canopypc,
         'pc vegetation cover'=Vegpc,
         'pc natural surface cover'=natsurfacepc,
         'pc buildings'=buildpc,
         'pc artificial surface'=artsurfacepc,
         'pc waterbodies'=waterpc,
         'pc mangroves'=mangrovepc)
# Plot 
ggcorr(corr.site,
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50",
       low="#2166AC", 
       mid="#F7F7F7",
       high="#B2182B",
       layout.exp = 2)+
  theme_pubclean()

## aggressive interactions positively correlated with number & density of cavities, and greater urban area
  ## negatively correlated with more diverse habitats, grassland, rivers, mangroves

## Shannon diversity is negatively correlated with several important factors:
  ## freq of cavity nesters and introduced species, greater cavity density and more aggressions
    ## positively correlated with waterbodies

## aggression level is not highly influenced by any particular environmental factor


#///////////////////////////////////
# ALL SPECIES
#//////////////////////////////////

# Convert data to numeric
corr <- data.frame(lapply(GLMdata_scale, as.integer))
corr<-corr %>% select(-IS.wt,-RS.wt,-Site.habitat.types,-Surrounding.habitat.types)
# Plot 
ggcorr(corr,
method = c("pairwise", "spearman"),
nbreaks = 6,
hjust = 0.8,
label = TRUE,
label_size = 3,
color = "grey50")

#///////////////////////////////////
# CORRELATION MATRIX NNP'S
#//////////////////////////////////


# Convert data to numeric
corr.parrots<-GLMdata_scale %>% filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|
                                         initsp=="Rose-ringed parakeet"| initsp=="Red-breasted parakeet")
corr.parrots <- data.frame(lapply(corr.parrots, as.integer))
corr.parrots<-corr.parrots %>% select(-IS.wt,-RS.wt,-Site.habitat.types,-Surrounding.habitat.types) 

# Plot 
ggcorr(corr.parrots,
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")

# test with ANOVA----

x<-GLMdata_scale %>% 
  #filter(isout!='NE') %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet")
anova <- aov(RS.size~isout, x)
summary(anova)
y<-lm(RS.size~isout, x)
summary(y)
# ANOVA confirms there is a large difference 



#////////////////////////////////
## with ISRS ----
#////////////////////////////////

#https://www.guru99.com/r-generalized-linear-model.html
# dev.off()

# select continuous variables
continuous <-select_if(ISRS, is.numeric)
summary(continuous)

GLMdata_ISRS <-ISRS 
#ranking for the normalization
GLMdata_ISRS<-GLMdata_ISRS %>% 
  mutate(rating=case_when(
    interaction=="Neutral"~"0", # no aggression
    interaction=="Displace"~"1", # remove from perch by landing
    interaction=="Threat"~"2", # vocal or flaring threats
    interaction=="Swoop"~"3", # fly within 60cm at species but not landing
    interaction=="Chase"~"4", # in-flight pursuit may or may not displace
    interaction=="Contact"~"5", # physical contact
    interaction=="Fight"~"5"))# multiple physical contact
GLMdata_ISRS$rating<-as.numeric(GLMdata_ISRS$rating)
GLMdata_ISRS<-GLMdata_ISRS %>% relocate(rating,.after = interaction)
GLMdata_ISRS<-GLMdata_ISRS %>% select(-Avg_Weight,-Sci_name,-IUCN_status,-SG_abundance,
                                      -Pref_hab,-Class,-`Site habitat types`,-`Surrounding habitat types`)

#Calculated columns
GLMdata_ISRS<-GLMdata_ISRS %>% 
  group_by(Study.Area) %>% 
  mutate(totalbuilt=sum(buildpc+artsurfacepc),
         totalgreen=sum(canopypc+Vegpc+natsurfacepc)) %>% 
  ungroup()

# STANDARDIZE
GLMdata_ISRS_scale <- GLMdata_ISRS %>% 
  mutate_if(is.numeric, funs(as.numeric(scale(.))))

str(GLMdata_ISRS_scale)

# factorise
GLMdata_ISRS_scale$role<-factor(GLMdata_ISRS_scale$role)

GLMdata_ISRS_scale<-GLMdata_ISRS_scale %>% filter(!is.na(SG_status))

# charts
  # shannon x status
GLMdata_ISRS_scale %>% 
  filter(Study.Area!='Changi Airport') %>% 
  mutate(status=case_when(SG_status=='I'~'Introduced',
                                    SG_status=='R'~'Resident',
                                    SG_status=='M'~'Migrant/Visitor',
                                    SG_status=='N'~'Migrant/Visitor',
                                    SG_status=='V'~'Migrant/Visitor')) %>% 
  ggplot(aes(Shannon)) +
  geom_density(aes(fill = status,color=status), alpha = 0.2) +
  labs(x = 'Species diversity (Shannon)', y='Density',
       title = 'Distribution of species by introduction status')+
    theme_pubclean()+style180+
  theme(legend.title=element_blank())+
  scale_x_continuous(limits = c(-1.5,1.5),
                     breaks = c(-1.5,-1.0,-0.5,0,0.5,1,1.5))+
  scale_fill_manual(values=c('Introduced'='#EE6677','Resident'='#4477AA',
                               'Migrant/Visitor'='#CCBB44'))+
scale_colour_manual(values=c('Introduced'='#EE6677','Resident'='#4477AA',
                           'Migrant/Visitor'='#CCBB44'))

## Introduced species more prevalent in lower BD areas
