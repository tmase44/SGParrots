
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
       tidyverse,vegan,lubridate,gridExtra,grid,ggrepel,reshape2,ggpmisc,
       BBmisc,stringr,
       ggpubr,AICcmodavg, #anova
       circlize, # interaction networks
       Distance, # transect analysis, relative abundance, density
       readxl,writexl)
library(gam)
library(psych)

#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================== 2. IMPORT DATA ================================  
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
# Trees
Tree <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                     sheet = "TC")

# NSS data
NSS <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/NSS/data/NSS_parrot_data_long.xlsx", 
                  sheet = "data")

# plot time data
 Pilot <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                    sheet = "pilottime")


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================ ALPHA BD INDICES ============================= 
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

#========================================#
# Simpson, Shannon, Richness == MAX----
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
Indices<-data.frame(Indices.max)
Indices <- rownames_to_column(Indices, "Study.Area")


# plot
Indices %>% 
  ggplot(aes(x=Simpson,y=Shannon)) +
  geom_point(aes(color=Richness), size=4)+
  labs(title = 'Alpha biodiversity indices',
       x = 'Simpson Index', y='Shannon Index',
       color='Richness')+
  geom_text_repel(aes(label=Study.Area),
                  nudge_y = 0.04,segment.color = NA)
# without Airport
Indices %>% 
  filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(x=Simpson,y=Shannon)) +
  geom_point(aes(color=Richness), size=4)+
  labs(title = 'Alpha biodiversity indices',
       x = 'Simpson Index', y='Shannon Index',
       color='Richness')+
  geom_text_repel(aes(label=Study.Area),
                  nudge_y = 0.04,segment.color = NA)

# table
formattable(Indices) 

## Changi and Stirling/QT have the lowest BD and richness scoring
  ## highly urbanised areas 

## Springleaf, old plantation and secondary forest with canal
  ## some urbanisation surrounding but also close to central catchment NR

## Palawan beach is quite busy but low urbanisation. Surrounding area rich in
  ## dense secondary forest with old trees and little or no maintenance

## Sengkang / PRTP are managed urban parks but quite rich in vegatation. 
  ## food resources are plentiful


# #/////////////////////////////////////////////////////////////////////////////#
# #/////////////////////////////////////////////////////////////////////////////#
# #============================= BETA BD INDICES ============================= 
# #/////////////////////////////////////////////////////////////////////////////#
# #/////////////////////////////////////////////////////////////////////////////#
# 
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

#Plotting the results – a base
sorensen.plot<-sorensen.melted %>%
  ggplot(aes(x=Var1, y=Var2, fill = value))+
  geom_tile(color = "white")+
  labs(title = 'Sorensen (Beta biodiversity index)')+
  theme(legend.title = element_blank())+
  scale_fill_gradient2(low = "white", high = "black",
                       mid="grey", midpoint = 0.5, limit = c(0,1),
                       breaks=c(0,0.5,1),
                       labels=c('No overlap','0.5','Max. overlap'))+
  geom_text(aes(label = value), color = "black", size = 4)+ #Adding values of Jaccard distance
  xlab("")+ylab("") #Removing the labels (x and y axis)

sorensen.plot

# higher value = greatest overlap / similarity
## Changi and stirling high similarity, PRTP not too dissimilar
## Airport has low overlap


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================== FIRST DATA TRANSFORMATION ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

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

# Factorise NSS data
NSS$Site<-as.factor(NSS$Site)
NSS$Site2<-as.factor(NSS$Site2)
NSS$Species<-as.factor(NSS$Species)
  # remove na
NSS$Count<-NSS$Count %>% replace(is.na(.), 0)


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#========================= SECOND DATA TRANSFORMATION =========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

#=============================#
# ISRS: Interact long-transform----
#=============================#
rm(ISRS)
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

# Combine IS+RS
ISRS<-rbind(IS,RS)
ISRS<-ISRS %>% relocate(1,2,3,4,5,7,6) 
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

ISRS<-ISRS %>% relocate(1,2,8,3,4,6,5,9,10,7,23,24,25,11,12,13,14,15,16,17,18,
                        19,20,21,22,26)
ISRS<-ISRS %>% arrange(Study.Area,desc(n_ints))

#=============================#
# Species Pairs----
#=============================#
rm(sp.pairs)
# pairs for all interactions
sp.pairs <- Interact_2 %>% 
  select(Study.Area,initsp,recipsp,interaction) %>% 
  group_by(Study.Area,initsp,recipsp,interaction) %>% 
  count(recipsp) %>% rename(all_pair_ints=n)
  # Neutral only
x<-Interact_2 %>% filter(rating=='0') %>%
  select(Study.Area,initsp,recipsp,interaction) %>% 
  group_by(Study.Area,initsp,recipsp,interaction) %>% 
  count(recipsp) %>% rename(NE_pair_ints=n)
  # Merge 
sp.pairs<-merge(sp.pairs,x,by=c('Study.Area','initsp','recipsp','interaction'),all = TRUE) %>% replace(is.na(.), 0) 
  # Aggressive only
x<-Interact_2 %>% filter(rating!='0') %>%
  select(Study.Area,initsp,recipsp,interaction) %>% 
  group_by(Study.Area,initsp,recipsp,interaction) %>% 
  count(recipsp) %>% rename(Agg_pair_ints=n)
  # Merge 
sp.pairs<-merge(sp.pairs,x,by=c('Study.Area','initsp','recipsp','interaction'),all = TRUE) %>% replace(is.na(.), 0)

## merge species traits for IS
x<-Prof %>% select(Species,NestType,sp_lab,Avg_size,Avg_Weight)
x<-x %>% rename(IS.NestType=NestType,
                IS.sp_lab=sp_lab,
                IS.size=Avg_size,
                IS.wt=Avg_Weight) 
sp.pairs<-merge(sp.pairs,x,by.x='initsp',by.y='Species')
## merge species traits for RS
x<-Prof %>% select(Species,NestType,sp_lab,Avg_size,Avg_Weight)
x<-x %>% rename(RS.NestType=NestType,
                RS.sp_lab=sp_lab,
                RS.size=Avg_size,
                RS.wt=Avg_Weight) 
sp.pairs<-merge(sp.pairs,x,by.x='recipsp',by.y='Species')

sp.pairs<-sp.pairs %>% relocate(3,2,1,4,5,6,7,8,12,9,13,10,14,11,15)
sp.pairs<-sp.pairs %>% mutate(size_diff=IS.size-RS.size)

# Parrots only for quick reference
sp.pairs.parrots<-sp.pairs %>% 
  filter(initsp=="Monk parakeet"|
           initsp=="Tanimbar corella"|
           initsp=="Rose-ringed parakeet"|
           initsp=="Red-breasted parakeet")
  

#============================#
# INDICES_2----
#============================#
rm(Indices_2)
# Environment
envpc<-Enviro %>% 
  select(Study.Area,areaHa,
         canopypc,Vegpc,buildpc,artsurfacepc,waterpc,natsurfacepc,mangrovepc,
         n_cavity)
Indices_2<-merge(Indices,envpc,by='Study.Area')

# Total non-native "I" species proportion
x<-Composition_2 %>% 
  filter(SG_status=='I') %>% 
  group_by(Study.Area,Species) %>% 
  summarise(freq=mean(max.freq)) %>% 
  group_by(Study.Area) %>% 
  summarise(freq.I=sum(freq))
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

# Explicit Cavity nester proportion
x<-Composition_3 %>% 
  filter(NestType=='Cavity') %>% 
  group_by(Study.Area,Species) %>% 
  summarise(max.freq=mean(max.freq)) %>% 
  group_by(Study.Area) %>% 
  summarise(cav.sp.freq=sum(max.freq))
Indices_2<-merge(Indices_2,x,by='Study.Area')

# All poss Cavity nester proportion
x<-Composition_3 %>% 
  filter(NestType=='Cavity-optional'|NestType=='Cavity') %>% 
  group_by(Study.Area,Species) %>% 
  summarise(freq=mean(max.freq)) %>% 
  group_by(Study.Area) %>% 
  summarise(cav.sp.freq2=sum(freq))
Indices_2<-merge(Indices_2,x,by='Study.Area')

### FROM ISRS

# Initiations
### Use ~IS~ ONLY otherwise 2x count
x<-Composition_3 %>% select(1,2,22:28) %>% 
  group_by(Study.Area) %>% 
  summarise(n_ints=sum(n_ints),
            n_ints_xNE=sum(n_ints_xNE),
            inits=sum(inits),
            inits_xNE=sum(inits_xNE),
            ints_HR=sum(ints_HR),
            ints_xNE_HR=sum(ints_xNE_HR))
Indices_2 <- merge(Indices_2,x,by='Study.Area')


#============================#
# 8.d. Tree & Cavity data----
#============================#

###


#===============================#
# 8.e. Enviro Long Transform----
#===============================#

# add properly summed Vegetation cover (less canopy cover)

Enviro_2 <- Enviro %>% 
  select(Study.Area,canopypc,Vegpc,buildpc,artsurfacepc,waterpc,natsurfacepc,mangrovepc) %>% 
  gather(key='land_prop',value='proportion',-Study.Area)

Enviro_2$land_prop <- factor(Enviro_2$land_prop
                             ,levels = c('buildpc','artsurfacepc','natsurfacepc',
                                         'Vegpc','canopypc','waterpc','mangrovepc'))
levels(Enviro_2$land_prop)

#===========================#
# other----
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
#=============================== x. NSS DATA  ================================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

# pop trends
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


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================ 9. DATA EXPLORATION ============================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

# 5 sites were surveyed. 
# Each site was surveyed for a total of 15 hours.
# Equal effort per site and species.
# Sites were selected based on NSS Parrot Count data to provide 
#   equal effort to each focal species.

#=========================================#
# 9.a. Composition / Overall summaries---- 
#=========================================#

nrow(Composition)
# 4,009 individuals observed in the composition surveys

x<-Composition %>% select(Species) %>% n_distinct()
x/407
# 92 distinct species observed observed in the composition surveys
  # = 23.58 % of all known species in Singapore
    # https://www.nparks.gov.sg/biodiversity/wildlife-in-singapore/species-list/bird
      # 407 spp


#================================#
# 9.b. Interaction summaries---- 
#================================#

Interact %>% count(initsp) %>% summarise(sum(n))
# 821 total interactions were observed 

Interact %>% filter(rsout=='NE') %>% count(initsp) %>% summarise(sum(n))
# of which 636 (78.89% were aggressive)
  # 179 (27.88% NEUTRAL)
821-179
179/642

n_distinct(ISRS$Species)/x
# 50 distinct species were observed involved in interactions
  # 54.34% of all species observed
   

Interact %>% summarise(n_distinct(initsp))
Interact %>% summarise(n_distinct(recipsp))
# 30 initiating species
# 46 recipient species


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
## 163 unique species pair interactions were observed

int_pairs %>% 
  group_by(recipsp) %>% summarise(n=sum(n)) %>% 
  mutate(freq=n/sum(n)*100) %>% arrange(desc(freq)) %>% 
  filter(recipsp!='Red-breasted parakeet',recipsp!='Rose-ringed parakeet',
         recipsp!='Monk parakeet',recipsp!='Tanimbar corella') %>% 
  print(n=30)
# non native parrots excluded
# ~ Top recipients
  # Javan myna, LTP, house crow, LTP OPH, YCC, AGS, YVBB, Oriole,
  # Flameback, dollarbird 


#=================================#
# 9.c. Cavity Nesters in focus----
#================================#

#  filter(Species!='Red-breasted parakeet',Species!='Rose-ringed parakeet',Species!='Monk parakeet',Species!='Tanimbar corella') %>% 
  
ISRS %>% 
  filter(role=='RS') %>%  filter(NestType=='Cavity') %>% # modify IS/RS
  summarise(n_distinct(Species))
# 21 cavity nesters involved in interactions
  # incl 6 parrots [4 focal sp.]
    # 19 were recipients
      # 15 were initators
ISRS %>% filter(NestType=='Cavity') %>%
  count(Species,NestType) %>% arrange(desc(n))
# List of cavity nesters involved in interactions

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


#============================#
# 9.d. Interaction Detail----
#============================#

ISRS %>% group_by(interaction) %>% 
  summarise(n=sum(n_ints)) %>% mutate(freq=n/sum(n)*100)
## Without NE
#                  n    %
# 1 Displace      436  36.6 
# 2 Threat        340  28.5 
# 3 Swoop         276  23.2 
# 4 Chase          92  7.72
# 5 Contact        36  3.02
# 6 Fight          12  1.01

## With NE
# 1 Neutral       324 21.4  
# 2 Displace      436 28.8  
# 3 Threat        340 22.4  
# 4 Swoop         276 18.2  
# 5 Chase          92  6.07 
# 6 Contact        36  2.37 
# 7 Fight          12  0.792
## frequency of interactions overall

ISRS %>% group_by(Species) %>%
  filter(interaction!='Neutral') %>% 
  summarise(n=sum(n_ints)) %>% mutate(freq=n/sum(n)*100) %>% arrange(desc(n))
# Top interacting sp.
#                              n    %
# 1 Tanimbar corella          209  17.5 
# 2 Red-breasted parakeet     193  16.2 
# 3 Javan myna                151  12.7 
# 4 Long-tailed parakeet      101  8.47
# 5 Rose-ringed parakeet       97  8.14
# 6 Monk parakeet              85  7.13
# 7 Oriental pied hornbill     70  5.87
# 8 House crow                 66  5.54
# 9 Yellow crested cockatoo    40  3.36
#10 Rock dove                  36  3.02

ISRS %>% group_by(Species,outcome) %>%
  filter(interaction!='Neutral') %>% 
  summarise(n=sum(n_ints)) %>% mutate(freq=n/sum(n)*100)%>% 
  filter(outcome=='W') %>% arrange(desc(n))
# Wins / all aggressions
#                                      n    %
# 1 Tanimbar corella        W         153  73.2
# 2 Red-breasted parakeet   W          92  47.7
# 3 Monk parakeet           W          70  82.4
# 4 Oriental pied hornbill  W          64  91.4
# 5 Rose-ringed parakeet    W          62  63.9
# 6 Long-tailed parakeet    W          40  39.6
# 7 Javan myna              W          39  25.8
# 8 House crow              W          18  27.3
# 9 Yellow crested cockatoo W          16  40  
#10 Large billed crow       W          10  66.7

ISRS %>% group_by(Species,outcome) %>%
  filter(interaction!='Neutral') %>% filter(role=='IS') %>% 
  summarise(n=sum(n_ints)) %>% mutate(freq=n/sum(n)*100)%>% 
  filter(outcome=='W') %>% arrange(desc(n))
# Wins / initiated aggressions
#                                      n    %
# 1 Tanimbar corella        W         133  80.1
# 2 Red-breasted parakeet   W          68  76.4
# 3 Monk parakeet           W          62  88.6
# 4 Rose-ringed parakeet    W          57  74.0
# 5 Oriental pied hornbill  W          39  100  
# 6 Long-tailed parakeet    W          29  50.9
# 7 Javan myna              W          14  63.6
# 8 Yellow crested cockatoo W          14  63.6
# 9 House crow              W          13  40.6
#10 Large billed crow       W           9  90  


#================================#
# 9.e. Tree / Cavity profiles----
#================================#

Tree %>% group_by(Study.Area) %>% summarise(n_distinct(id)) 
# Changi Village = 208 trees identified
# Stirling Road  = 144 trees

Tree %>% group_by(Study.Area) %>% summarise(n_distinct(tree_sp)) 
# Changi Village = 15 tree species
# Stirling Road  = 24 tree species

Tree %>% filter(cav=='y') %>% 
  group_by(Study.Area) %>% count(cav)

# Changi Village = 236 cavities 
# Stirling Road  = 77 cavities

Tree %>% 
  select(Study.Area,id,cav) %>% 
  distinct(Study.Area,id,cav) %>% 
  select(-id) %>% group_by(Study.Area,cav) %>% tally() %>% 
  mutate(freq=n/sum(n)*100)

# Changi Village = only 62 (29.8%) cavity bearing
# Stirling Road  = only 31 (21.5%) cavity bearing



#===========================#
# 9.f. Data distribution----
#===========================#

ISRS %>% filter(role=='IS') %>% group_by(Study.Area) %>% summarise(n=sum(n_ints)) %>% arrange(desc(n))
# Different number of samples at each site
# 1 Changi Village            254
# 2 Pasir Ris Town Park       172
# 3 Springleaf                158
# 4 Stirling Road             109
# 5 Sengkang Riverside Park    65
# 6 Palawan Beach              53

ISRS %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  group_by(Species) %>% summarise(n=sum(n_ints)) %>% arrange(desc(n))
# Different sample size each species
# 1 Red-breasted parakeet   285
# 2 Tanimbar corella        257
# 3 Long-tailed parakeet    132
# 4 Rose-ringed parakeet    127
# 5 Monk parakeet           118

# check for normality
ISRS %>% 
  filter(role=='IS') %>% 
  mutate(freq=n_ints/sum(n_ints)*100) %>% 
  ggplot(aes(interaction,freq))+geom_col(width=0.95)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)
# data are positively skewed, not normally distributed

ISRS %>% 
  filter(role=='IS') %>% group_by(Study.Area) %>% mutate(freq=n_ints/sum(n_ints)*100) %>% ggplot(aes(interaction,freq))+geom_col(width=0.95)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)+
  facet_wrap(~Study.Area)
# data can be even more skewed at the site level

ISRS %>% 
  filter(role=='IS') %>% filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>% group_by(Species) %>% mutate(freq=n_ints/sum(n_ints)*100) %>% ggplot(aes(interaction,freq))+geom_col(width=0.95)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)+
  facet_wrap(~Species)
# and also at the species level = frequency %
ISRS %>% 
  filter(role=='IS') %>% filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>% group_by(Species) %>% ggplot(aes(interaction,n_ints))+geom_col(width=0.95)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)+
  facet_wrap(~Species)
# actual n


# data is ordinal from least to most aggressive interaction (Likert scale 1-7)

# "it doesn’t matter which of the two statistical analyses you use to analyze 
  # your Likert data. If you have two groups and you’re analyzing five-point 
  # Likert data, both the 2-sample t-test and Mann-Whitney test have nearly 
  #equivalent type I error rates and power." 

# Try the two test, and if not the same, then it's difference in power!
# Reference: de Winter, J.C.F. and D. Dodou (2010), Five-Point Likert Items: t test versus Mann-Whitney-Wilcoxon, Practical Assessment, Research and Evaluation, 15(11).







#==========================#
# 9.g Statstical tests----
#==========================#

# Lakicevic 2012 Introduction to R for Terrestrial Ecology

# nonparametric tests start off with the assumption that the underlying data
# do not have a normal distribution (Adler, 2012).

# Nonparametric tests are also referred to as distribution-  free tests, and
# they can be particularly useful when the data are measured on a categorical
# scale, i.e., when we deal with an ordered data set (Petrie and Sabin, 2005). 
# It should be noted that nonparametric tests are less powerful in comparison to 
# equivalent parametric tests (Vuković, 1997).




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


#========================#
# SITE DESCRIPTIONS----
#========================#
Indices_2 %>% 
  group_by(Study.Area) %>% 
  mutate(built_surf=sum(buildpc+artsurfacepc)) %>% 
  ggplot(aes(built_surf,Richness.max))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Proportion urban land cover',y='Shannon')
## BD and Richness declines with building and road cover
Indices_2 %>% 
  group_by(Study.Area) %>% 
  mutate(vegcan=sum(canopypc+Vegpc)) %>% 
  ggplot(aes(vegcan,Richness.max))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Proportion greenery cover',y='Shannon')
## BD and Richness increases with total greenery cover 
## canopy cover and vegetation alone have minimal correlation, but 
  # together have a multiplicative effect

Indices_2 %>% 
  group_by(Study.Area) %>% 
  mutate(built_surf=sum(buildpc+artsurfacepc)) %>% 
  ggplot(aes(built_surf,inits))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Proportion urban land cover',y='Shannon')
Indices_2 %>% 
  group_by(Study.Area) %>% 
  mutate(vegcan=sum(canopypc+Vegpc)) %>% 
  ggplot(aes(vegcan,inits))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Proportion urban land cover',y='Shannon')
## No strong link between land type and interaction frequency

#=========================#
# i Richness x parrot prop
#=========================#

Indices_2 %>% 
  ggplot(aes(ints_HR,cav.sp.freq))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(aes(color=Study.Area))+
  labs(title = 'Parrot proportion in the community positively correlates with overall species Richness',
       y='Species richness',x='Proportion of parrots in the community')



#============================#
# ii. total obs x total ints.----
#============================#
#~~~~~~~~~~~~~~~~~~#
# All species 
#~~~~~~~~~~~~~~~~~~#
x1<-Composition_3 %>% 
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
  group_by(Species) %>% summarise(all_obs=sum(all_obs),
                                  n_ints=sum(n_ints)) 
x1 %>% 
  ggplot(aes(n_ints,all_obs))+
  geom_point()+
  stat_cor(method = 'spearman')+
  stat_poly_line()+
  geom_text_repel(data=subset(x1,n_ints>30),(aes(label=Species)))+
  labs(x = 'Number of interactions', y = ' Numer of individuals',
       title = 'All species')
x<-lm(n_ints~all_obs,data=Composition_3)
summary(x)
# Most species in the community do not fall (often) into the interaction
  # network of non native parrots
    # mostly: other parrots and highly abundant birds
      # Javan myna population of Changi is excluded from this

x1<-Composition_3 %>% 
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
  group_by(Species) %>% summarise(all_obs=sum(all_obs),
                                  inits=sum(inits))
x1 %>% 
  ggplot(aes(inits,all_obs))+
  geom_point()+
  stat_smooth(method = 'lm')+
  stat_poly_eq()+
  geom_text_repel(data=subset(x1,inits>30),(aes(label=Species)))+
  labs(x = 'Number of interactions', y = ' Numer of individuals',
       title = 'All species')
x<-lm(inits~all_obs,data=Composition_3)
summary(x)
# initiations with parrots, by other species are even fewer


#~~~~~~~~~~~~~~~~~~#
# Non-Focal species 
#~~~~~~~~~~~~~~~~~~#
# Parrots removed as these are the foccal species and biased
x2<-Composition_3 %>% 
  filter(Species!="Monk parakeet"&Species!='Tanimbar corella'&Species!='Rose-ringed parakeet'&Species!='Red-breasted parakeet')%>%  
  group_by(Species) %>% summarise(all_obs=sum(all_obs),
                                  n_ints=sum(n_ints)) %>% 
  filter(all_obs>0,n_ints>0)
x2 %>% 
  ggplot(aes(n_ints,all_obs))+
  geom_point()+ 
  stat_smooth(method = 'lm')+
  stat_poly_eq()+
  geom_text_repel(data=subset(x2,n_ints>20),(aes(label=Species)))+
  labs(x = 'Number of interactions', y = ' Numer of individuals',
       title = 'All species')
# looking at non-focal species only
  # only a handful of species directly initate interactions with parrots
    # mostly larger cavity competitors / predators

#//////// Outliers//////// 
# OPH were rarely observed but highly successful in displacing
 # larger birds when they came to forage or inspect cavities
# YCC, low abundance but aggressive and cooperating with Tanimbar Corellas
# Rock Doves, highly abundant but placid, little overlap for cavities
# House crows, not cavity users, but cavity foragers and roost competitors

#~~~~~~~~~~~~~~#
# Parrots only
#~~~~~~~~~~~~~~#
x3<-Composition_3 %>%
  filter(Species=="Monk parakeet"|Species=='Tanimbar corella'|
           Species=='Rose-ringed parakeet'|Species=='Red-breasted parakeet'|
           Species=='Long-tailed parakeet')%>%  
    group_by(Species) %>% summarise(all_obs=sum(all_obs),
                                  n_ints=sum(n_ints),
                                  n_ints_xNE=sum(n_ints_xNE)) %>% 
  mutate(ints_freq=n_ints/all_obs) %>% 
  mutate(intsxNE_freq=n_ints_xNE/all_obs)

x3 %>% 
  ggplot(aes(all_obs,n_ints_xNE))+
  geom_point()+ 
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_text_repel(data=subset(x3,all_obs>10),(aes(label=Species)))+
  labs(x = 'Number of individuals', y = ' Numer of interactions',
       title = 'Focal species')


#==============================#
# iii initiated interactions----
#==============================#

# parrot prop in community != more interactions
Indices_2 %>% 
  ggplot(aes(freq.nnparr,inits))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point()+
  labs(title = 'Greater proportion of parrots does not predict greater interaction frequency',
       y='n interactions',x='Proportion of parrots in the community')
# R2 = 0.05
## only 6% variation of interaction frequency is explained by parrot abundance
y<-lm(allIS~parrotprop,Indices_2)
#summary(y)

Indices_2 %>% 
  ggplot(aes(Richness.max,inits))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Richness of site is also not a driver of interaction frequency',
       y='n interactions',x='Species richness')
# R2 = <0.1%
# plot
grid.arrange(isxprop,isxrich,ncol=2)

Composition_3 %>% 
  ggplot(aes(Study.Area,ints_HR))+
  geom_col()

#===========================#
# v Cavity nester proportion----
#===========================#
# THIS----
Indices_2 %>% filter(Study.Area!='Sengkang Riverside Park') %>% 
  ggplot(aes(cav.sp.freq,ints_HR))+
           stat_poly_line(se=F)+
           stat_poly_eq()+
           geom_point(aes(color=Study.Area))
# greater aggression levels where the number of obligate cavity nesters is high

Indices_2 %>% filter(Study.Area!='Sengkang Riverside Park') %>% 
  ggplot(aes(cav.sp.freq,inits_xNE))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(aes(color=Study.Area))+
  labs(title = 'Cavity nester proportion = higher aggression')

# how about aggression level


#===============================#
# Species level correlations----
#===============================#

# Abundance
Composition_3 %>% group_by(Study.Area) %>% 
  filter(Study.Area!='Changi Airport R1'&
           Study.Area!='Changi Airport R3') %>%
  ggplot(aes(max_obs,n_ints))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(aes(color=Species,alpha=0.8))+
  labs(x='Abundance',y='total interaction involvement',
       title = 'Abundance / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))

# Proportion of community
Composition_3 %>% filter(Study.Area!='Changi Airport R1'&
                           Study.Area!='Changi Airport R3') %>% 
  ggplot(aes(max.freq,n_ints))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(aes(color=Species,alpha=0.8))+
  labs(x='Proportion of species in the community',y='total interaction involvement',
       title = 'Proportion of community / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))


#==========================#
# These are interesting----
#==========================#
  
# Proportion by site
Composition_3 %>% 
  ggplot(aes(max.freq,n_ints,color=Species))+
  geom_point(alpha=0.8)+
  labs(x='Proportion of species in the community',y='total interaction involvement',
       title = 'Proportion of community / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Sulphur crested cockatoo'='green','Blue rumped parrot'='red'))+
  facet_wrap(~Study.Area)

# Proportion by site / species facet
Composition_3 %>% filter(n_ints>5) %>% 
  ggplot(aes(max.freq,n_ints,color=Species))+
  geom_point(alpha=0.8)+
  labs(x='Proportion of species in the community',y='total interaction involvement',
       title = 'Proportion of community / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Sulphur crested cockatoo'='green','Blue rumped parrot'='red'))+
  facet_wrap(~Species)



# Parrot targets----
all.targets %>% 
  filter(initsp=="Monk parakeet"|initsp=='Tanimbar corella'|
           initsp=='Rose-ringed parakeet'|initsp=='Red-breasted parakeet')%>% 
  group_by(initsp) %>% 
  summarise(n=n_distinct(recipsp)) %>% 
  ggplot(aes(reorder(initsp,-n),n))+
  geom_col()+
  labs(x='Species',y='n unique species interacted with')

# body size
parrot.targets %>% filter(RS.size<150) %>% 
  ggplot(aes(initsp,RS.size))+
  geom_boxplot()+
  geom_jitter(width = 0.08,alpha=.4)+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution over all interactions')

# removing NE (Only aggressive ints.)
parrot.targets %>% filter(RS.size<150) %>% filter(n_NE==0) %>% 
  ggplot(aes(initsp,RS.size))+
  geom_boxplot()+
  geom_jitter(width = 0.08,alpha=.4)+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution of aggression recipients')
  
# removing NE & mean highlight
parrot.targets %>% #
  filter(RS.size<150) %>% 
  filter(n_NE==0) %>% 
  ggplot(aes(initsp,RS.size))+
  geom_boxplot(outlier.shape = 1)+
  stat_summary(fun='mean',color='red',shape=15)+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution of aggression recipients')

# All targets----
all.targets %>% filter(RS.size<200) %>% 
  ggplot(aes(IS.size,RS.size))+
  geom_jitter(width=1.5,height = 1.5,alpha=.4)


# boxplots with a few species
all.targets %>% filter(RS.size<150) %>% filter(n_NE==0) %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"| initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet"|initsp=="Javan myna"|initsp=="Oriental pied hornbill"|initsp=="Yellow crested cockatoo") %>% 
  ggplot(aes(initsp,RS.size))+
  geom_boxplot(outlier.shape = 1)+
  stat_summary(fun='mean',color='red',shape=15)+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution of aggression recipients')

# interaction type / RS.size ~ species
all.targets %>% filter(RS.size<150) %>% filter(n_NE==0) %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"| initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet") %>% 
  ggplot(aes(interaction,RS.size))+
  geom_boxplot(outlier.shape = 1)+
  stat_summary(fun='mean',color='red',shape=15)+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution of aggression recipients')+
  facet_wrap(~initsp)

# study.area / RS.size ~ species
all.targets %>% filter(RS.size<150) %>% filter(n_NE==0) %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"| initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet") %>% 
  group_by(Study.Area,initsp) %>% 
  mutate(n=n_Agg/sum(n_Agg)) %>% 
  filter(n<.75) %>% 
  ggplot(aes(interaction,n))+
  geom_col()+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution of aggression recipients')+
  facet_grid(Study.Area~initsp)



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================= 10.2 TOP-LINE REGRESSIONS ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
## andrews
# Generalized additive models were used because of the humpshaped
# relationship between the total number of interactions and the
# difference in body size (Zuur et al., 2011). 
  ## zuur p
# ## The data exploration did not show any clear linear patterns between roadkills and
# the explanatory variables; so we need to move on to using a GAM. Furthermore,
# an initial GLM with a Poisson distribution and logarithmic link function gave an
# overdispersion of 5, and we therefore proceed with a GAM with a negative bino-
#   mial distribution and logarithmic link function
ISRS %>% 
  ggplot(aes(Avg_size,n_ints))+
  geom_smooth()
Composition_3 %>% 
  ggplot(aes(Avg_size,inits))+
  geom_smooth()
Composition_3 %>% 
  ggplot(aes(Avg_size,inits_xNE))+
  geom_smooth()

# For all analysis of interspecies
# interactions, we summed interactions across all surveys for each
# site and included site and the species pair as categorical fixed effects.
# We examined the relationship between traits and interactions for two groups: 
# (a) all observed birds and 
# (b) cavity-breeding species. 
# Response variables included ‘total interactions’ (summed interactions between 
# species pairs), and ‘outcome’ which was the total number of wins for each species
# (e.g. the number of times rainbow lorikeet won vs. the common myna). 

x<-gam(n_ints~RS.NestType,data=all.targets)
summary(x)

x<-gam(n_ints~cav.sp.freq+freq.nnparr,data=Indices_2)
summary(x)

# For all models, explanatory variables included ‘difference in
# mean body size’, ‘site’, ‘species ID’. We used absolute body size difference
# as an explanatory variables for the models for total interactions
# while relative body size difference was used in the models of outcome.

ISRS %>% 
  ggplot(aes(Avg_size,n_ints))+
  geom_smooth()

#http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#spearman-rank-correlation-coefficient
# Kendall tau and Spearman rho, which are rank-based correlation coefficients (non-parametric)

x<-ggscatter(Composition_4,'n_ints','max.freq',
          add = 'reg.line',
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE)
x+stat_cor(method = 'spearman', label.x = 3, label.y = 30)


x<-cor.test(Composition_4$n_ints,
             Composition_4$max.freq,
             method='spearman',
             exact = F)
x
# spearmans rho shows a positive association between
  # n interactions and population density in a community

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
  select(Study.Area,Species,max.freq) %>% 
  summarise(max.freq=max(max.freq)) %>% arrange(Study.Area,desc(max.freq))

  
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

# RA curve
x<-Composition_3 %>% 
  select(Study.Area,Species,max_obs,max.freq) %>% 
  group_by(Study.Area) %>% 
  mutate(rank=dense_rank(desc(max.freq))) %>% 
  arrange(Study.Area,rank) %>% 
  group_by(Study.Area) %>%
  mutate(id = row_number())
x$rank<-as.factor(x$rank)

x %>% filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(id,max.freq,color=Study.Area))+
  geom_line(alpha=0.8)+
  geom_point(shape=1)+
  scale_x_continuous(breaks=c(1,10,20,54),
                 labels=c('1','5','10','18'))+
  facet_wrap(~Study.Area)+
  labs(title = 'Relative abundance curve',
       x='Abundance rank',y='Abundance')
y<-Composition_3 %>%
  select(Study.Area,Species,max.freq,SG_status) %>% 
  group_by(Study.Area,Species) %>% 
  arrange(Study.Area,desc(max.freq)) %>% 
  filter(max.freq>5)
view(y)
# Non-natives account for the majority of the community in every site
  # house crows, parrots, javan mynas

x<-Composition_3 %>% 
  drop_na(SG_status) %>% 
  select(Study.Area,Species,max.freq,SG_status) %>% 
  mutate(Status=case_when(SG_status=='I'~'Non-native',
                          SG_status=='R'~'Resident',
                          SG_status=='M'~'Migratory',
                          SG_status=='N'~'Migratory',
                          SG_status=='V'~'Migratory')) %>% 
  group_by(Study.Area,Status) %>% 
  summarise(n=sum(max.freq)) %>% 
  arrange(Study.Area,desc(n)) 
x$Status<-factor(x$Status,levels = c('Migratory','Resident','Non-native'))
x %>% 
  ggplot(aes(Study.Area,n,fill=Status))+
  geom_col(position = 'fill') +
  scale_fill_discrete(name = "Status")+
  labs(y='Proportion of community',
         title = 'Proportion of resident / non-native / migratory species')+
  theme(axis.title.x = element_blank())
# non native / introduced species are most prevalent in low biodiversity areas
    
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================== 10.4 CHARTS: INTERACTIONS ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

#pilot

Pilot %>% 
  ggplot(aes(time,freq))+
  geom_col()+
  labs(x='Time of day',y='Interaction frequency',
       title = 'Pilot study observations: Half-hourly interaction frequency')+
  theme_minimal()+
  theme(axis.text.x = element_text(size=25,angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 25),
      plot.title = element_text(size=40),
      axis.title = element_text(size=18))

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

#============================================================#
## interaction frequency standardised based on n observations
#============================================================#
x3<-Composition_3 %>%
  group_by(Species) %>% summarise(all_obs=sum(all_obs),
                                  n_ints=sum(n_ints),
                                  n_ints_xNE=sum(n_ints_xNE),
                                  inits=sum(inits),
                                  inits_xNE=sum(inits_xNE)) %>% 
  mutate(ints_freq=n_ints/all_obs) %>% 
  mutate(intsxNE_freq=n_ints_xNE/all_obs) %>% 
  mutate(inits_freq=inits/all_obs) %>% 
  mutate(inits_xNE_freq=inits_xNE/all_obs)

# all interactions / total obs all species
x3 %>% 
  filter(all_obs>=10&inits>0) %>% 
  ggplot(aes(reorder(Species,inits_freq),inits_freq))+
  geom_col()+coord_flip()+
  labs(y= 'n initated interactions / n observations',
       x= 'Species')
# parrots
x3 %>% 
  filter(Species=="Monk parakeet"|Species=='Tanimbar corella'|
           Species=='Rose-ringed parakeet'|Species=='Red-breasted parakeet'|
           Species=='Long-tailed parakeet')%>%  
  ggplot(aes(reorder(Species,inits_freq),inits_freq))+
  geom_col()+coord_flip()+
  labs(y= 'n initated interactions / n observations',
       x= 'Species')

# initiations - no NE / total obs all species
x3 %>% 
  filter(inits_xNE>1) %>% 
  ggplot(aes(reorder(Species,inits_xNE_freq),inits_xNE_freq))+
  geom_col()+coord_flip()+
  labs(y= 'n initated interactions / n observations',
       x= 'Species')

# parrots
x3 %>% 
  filter(Species=="Monk parakeet"|Species=='Tanimbar corella'|
           Species=='Rose-ringed parakeet'|Species=='Red-breasted parakeet'|
           Species=='Long-tailed parakeet')%>%  
  ggplot(aes(reorder(Species,inits_xNE_freq),inits_xNE_freq))+
  geom_col()+coord_flip()+
  labs(y= 'n initated interactions / n observations',
       x= 'Species')


#/////////////#
# inits relative to max pop per survey site
x4<-Composition_3 %>%
  group_by(Study.Area,Species) %>% summarise(max_obs=mean(max_obs),
                                  n_ints=sum(n_ints),
                                  n_ints_xNE=sum(n_ints_xNE),
                                  inits=sum(inits),
                                  inits_xNE=sum(inits_xNE)) %>% 
  mutate(ints_freq=n_ints/max_obs) %>% 
  mutate(intsxNE_freq=n_ints_xNE/max_obs) %>% 
  mutate(inits_freq=inits/max_obs) %>% 
  mutate(inits_xNE_freq=inits_xNE/max_obs)
levels(x4$interaction)
x4$interaction<-factor(x4$interaction,
                       levels = c('Neutral','Displace','Threat','Swoop','Chase','Contact','Fight'))

# parrots
x4 %>% 
  filter(Species=="Monk parakeet"|Species=='Tanimbar corella'|
           Species=='Rose-ringed parakeet'|Species=='Red-breasted parakeet'|
           Species=='Long-tailed parakeet')%>%
  ggplot(aes(reorder(Species,ints_freq),ints_freq))+
  geom_col()+
  coord_flip()+
  labs(y= 'n initated interactions / n observations',
       title = 'Frequency of intiated interactions')+
  theme(axis.title.y = element_blank())


x4<-Composition_4 %>%
  group_by(Study.Area,Species,interaction) %>% summarise(max_obs=mean(max_obs),
                                             n_ints=sum(n_ints),
                                             n_ints_xNE=sum(n_ints_xNE),
                                             inits=sum(inits),
                                             inits_xNE=sum(inits_xNE)) %>% 
  mutate(ints_freq=n_ints/max_obs) %>% 
  mutate(intsxNE_freq=n_ints_xNE/max_obs) %>% 
  mutate(inits_freq=inits/max_obs) %>% 
  mutate(inits_xNE_freq=inits_xNE/max_obs)

x4 %>% 
  filter(Species=="Monk parakeet"|Species=='Tanimbar corella'|
           Species=='Rose-ringed parakeet'|Species=='Red-breasted parakeet'|
           Species=='Long-tailed parakeet')%>%  
  filter(interaction!=0) %>% 
  ggplot(aes(reorder(Species,ints_freq),ints_freq))+
  geom_col(aes(fill=interaction),position='fill')+
  coord_flip()+
  labs(y= 'n initated interactions / n observations',
       title = 'Frequency of intiated interactions by type')+
  theme(axis.title.y = element_blank())

## !! ADD COMPOSTION_4 FOR INTERACTION TYPE AND STACKED BAR INTERACTION TO THIS

## by study site, the number of interactions standardised by abundance in
## that area. total obs per site / abundance per site

#=== LTP ===#
## high competition with RBP in Springleaf, RRP there too.
  # check NSS data
## actually quite aggressive in defense of their homes, but largely unsuccessful.

#=== MP ===#
## most aggressions in defense of the nest - crows, mynas.
## other interactions are less aggressive and clearing resource trees.
  ## immediately in the nest vicinity.

#=== RBP ===#
## faces cavity competition from TC & JM @ CV
## cavity competition with LTP at springleaf
## Springleaf, less competition, fewer JM, MP in stick nest, RRP neutral

#=== RRP ===#
## quite aggressive over all sites.
## not big populations, not many cavities seen.
## highly aggressive over food resources in a way not seen among RBP/MP.
## will even chase birds not competing for the same resources, but just happen 
  ## to be in the vicinity.

#=== TC ===#
## in general quite aggressive.
## seen clearing their roosting and nesting area through frequent swooping.
## not unusual to see them swoop large mammals / contact small mammals.
## highly territorial.
## presence of crows & larger birds exacerbates aggression.



##########

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

# actual n ints
ISRS %>% 
  filter(role=='IS') %>% group_by(Species) %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  ggplot(aes(interaction,n_ints))+geom_col(width=0.95)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)+
  theme(legend.position = 'none')+labs(y='n_ints',x='interaction',title='Interaction distribution: positively skewed')+
  facet_wrap(~Species)

# relative freq (%)
ISRS %>% 
  filter(role=='IS') %>% group_by(Species) %>% 
  filter(Species=="Monk parakeet"|Species=="Tanimbar corella"|Species=="Rose-ringed parakeet"|Species=="Red-breasted parakeet"|Species=="Long-tailed parakeet") %>%  
  mutate(freq=n_ints/sum(n_ints)*100) %>% 
  ggplot(aes(interaction,freq))+geom_col(width=0.95)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)+
  theme(legend.position = 'none')+labs(y='relative frequency',x='interaction',title='Interaction distribution: positively skewed')+
  facet_wrap(~Species)

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

# SUMMARY-SE 
x<-ISRS %>% 
  filter(role=='IS') %>% 
  select(Species,interaction,rating)
x$rating<-as.numeric(x$rating)

parrotmeans <- summarySE(x, measurevar =  "rating",
                         groupvar = c("Species", "interaction"),
                         na.rm = TRUE)
parrotmeans



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

