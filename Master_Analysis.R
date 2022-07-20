
#/////////////////////////////////////////////////////////////////////////////#
#============================  MASTER ANALYSIS   =============================
#/////////////////////////////////////////////////////////////////////////////#


#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#============================= LOAD PACKS ==================================  
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
library(pacman)
library(Ostats)
p_load(formattable,knitr,kableExtra, # nice tables
       tidyverse,vegan,lubridate,gridExtra,grid,ggrepel,reshape2,ggpmisc,
       BBmisc,stringr,Hmisc,moments,
       ggpubr,AICcmodavg, #anova
       circlize, # interaction networks
       Distance, # transect analysis, relative abundance, density
       readxl,writexl)
library(gam)

library(psych)



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
Indices<-data.frame(Indices.max)
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


#=============================#
# ISRS: Interact long-transform
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

ISRS<-ISRS %>% relocate(1,2,8,3,4,6,5,9,10,7,27,28,29,30,23,24,25,11,12,13,
                        14,15,16,17,18,
                        19,20,21,22,26)
ISRS<-ISRS %>% arrange(Study.Area,desc(n_ints))
ISRS$n_ints<-ISRS$n_ints%>% replace(is.na(.), 0)
ISRS$ints_HR<-ISRS$ints_HR%>% replace(is.na(.), 0)

#=============================#
# Species Pairs
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
# Indices_2
#============================#

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
  summarise(n_ints=sum(n_ints))
Indices_2<-merge(Indices_2,x,by='Study.Area')

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


#===============================#
# ISRS 2 adding Indices data
#===============================#
rm(sp.pairs_2)
sp.pairs_2<-merge(sp.pairs,Indices_2,by='Study.Area',all=T)

#===============================#
# Enviro Long Transform
#===============================#

# just for charting

Enviro_2 <- Enviro %>% 
  select(Study.Area,canopypc,Vegpc,buildpc,artsurfacepc,waterpc,natsurfacepc,mangrovepc) %>% 
  gather(key='land_prop',value='proportion',-Study.Area)

Enviro_2$land_prop <- factor(Enviro_2$land_prop
                             ,levels = c('buildpc','artsurfacepc','natsurfacepc',
                                         'Vegpc','canopypc','waterpc','mangrovepc'))
levels(Enviro_2$land_prop)

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
# Composition summaries 
#//////////////////////
# https://www.nparks.gov.sg/biodiversity/wildlife-in-singapore/species-list/bird

a<-nrow(Composition)
b<-Composition %>% select(Species) %>% n_distinct()
c<-round((b/407)*100,2)
sprintf('%s individuals observed in total',a)
sprintf('%s distinct species observed, equal to %s%% of avian species in Singapore',b,c)

#//////////////////////
# Interaction summaries
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


#////////////////////
# Interaction pairs
#///////////////////
int_pairs <- Interact_2 %>%
  count(initsp, recipsp) %>%
  complete(initsp, nesting(recipsp), fill = list(n = 0)) %>% 
  filter(n!='0') %>% 
  arrange(desc(n))
int_pairs %>% print(n=20) # top 10 interaction pairs
x<-nrow(int_pairs)
sprintf('%s unique species pairs were observed interacting',x)

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


#////////////////////////
# Cavity Nesters in focus
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
  filter(role=='RS') %>% filter(NestType=='Cavity') %>%  
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
  filter(Species=='Red-breasted parakeet'|Species=='Rose-ringed parakeet'|Species=='Tanimbar corella'|Species=='Monk parakeet') %>% 
  summarise(n=sum(n_ints))
y5<-round((y4/d)*100,2)
sprintf('Focal non native parrots initiated %s [%s%%] of all interactions',y4,y5)


rm(a,b,c,d,e,f,g,h,i,j,x,y,z,y2,y3,y4,y5,z2)


#============================#
# 9.d. Interaction Detail
#============================#
ISRS %>% group_by(interaction) %>% 
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
shapiro.test(Indices_2$n_ints) # normal
shapiro.test(Indices_2$Neutral) # normal
shapiro.test(Indices_2$Swoop) # normal
shapiro.test(Indices_2$Displace) # normal
shapiro.test(Indices_2$Threat) # normal
shapiro.test(Indices_2$Richness) # normal
shapiro.test(Indices_2$Shannon) # normal
shapiro.test(Indices_2$canopypc) # normal
shapiro.test(Indices_2$cavs_canopy_sqm) # normal
skewness(Indices_2$canopypc)
kurtosis(Indices_2$canopypc)
skewness(Indices_2$Richness)
kurtosis(Indices_2$Richness)
# indices and survey site data are normally distributed

shapiro.test(ISRS$rating) # not normal
shapiro.test(ISRS$ints_HR) # not normal
shapiro.test(sp.pairs$all_pair_ints) # not-normal
x<-Composition %>% sample_n(100)
shapiro.test(x$distance)  # not-normal
x<-Comp.max %>% filter(Study.Area!='Changi Airport')
ggplot(x,aes(max_obs))+geom_histogram() # not normal
x<-sp.pairs %>% ungroup() %>% 
  group_by(initsp,recipsp) %>% summarise(n=sum(all_pair_ints))
ggplot(x,aes(n))+geom_histogram() # not normal


ggplot(Interact_2,aes(rating))+geom_histogram()
skewness(Interact_2$rating)
kurtosis(Interact_2$rating)
skewness(x$n)
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

#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================== SURVEY SITES ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

## Alpha ----
# Shannon / Simpson / Richness
Indices %>% 
  ggplot(aes(x=Simpson,y=Shannon)) +
  geom_point(aes(color=Richness), size=4)+
  labs(title = 'Alpha biodiversity indices',
       x = 'Simpson Index', y='Shannon Index',
       color='Richness')+
  geom_text_repel(aes(label=Study.Area),
                  nudge_y = 0.04,segment.color = NA)
  # without Changi Airport
Indices %>% 
  filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(x=Simpson,y=Shannon)) +
  geom_point(aes(color=Richness), size=4)+
  labs(title = 'Alpha biodiversity indices',
       x = 'Simpson Index', y='Shannon Index',
       color='Richness')+
  geom_text_repel(aes(label=Study.Area),
                  nudge_y = 0.04,segment.color = NA)

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

## Land-use types----
Enviro_2 %>% 
  ggplot(aes(Study.Area,proportion,fill=land_prop))+
  geom_col(position='fill')+
  scale_x_discrete(labels = function(Study.Area2) str_wrap(Study.Area2, width = 10))+
  labs(y='Proportion of land cover',x='Study Area',title = 'Land type per study site')

## RA curve----
x<-Composition_2 %>% 
  select(Study.Area,Species,max_obs,max.freq) %>% 
  group_by(Study.Area) %>% 
  mutate(rank=dense_rank(desc(max.freq))) %>% 
  arrange(Study.Area,rank) %>% 
  group_by(Study.Area) %>%
  mutate(id = row_number())
x$rank<-as.character(x$rank)

x %>% filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(id,max.freq))+
  geom_point(shape=1,alpha=0.6)+
  geom_line(alpha=0.6,position = position_dodge(width=0.1))+
  facet_wrap(~Study.Area)+
  labs(title = 'Rank abundance curves',
       x='Rank',y='Abundance (percent)')+
  scale_x_continuous(limits = c(1, 54), 
                     breaks = c(1,5,10,15,20,25,30,35,40,45,50,54))+
  theme_pubclean()+
  theme(legend.position = 'none')

## Species status----
y<-Composition_2 %>%
  select(Study.Area,Species,max.freq,SG_status) %>% 
  group_by(Study.Area,Species) %>% 
  arrange(Study.Area,desc(max.freq)) %>% 
  filter(max.freq>5)
view(y)
# Non-natives account for the majority of the community in every site
# house crows, parrots, javan mynas

x<-Composition_2 %>% 
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

## Profile table----
Indices_2 %>% 
  group_by(Study.Area) %>% 
  mutate('Built area'=sum(buildpc+artsurfacepc), #28
         'Natural area'=sum(canopypc+Vegpc+natsurfacepc), #29
         'Water area'=sum(waterpc+mangrovepc)) %>% #30
  select(1,5,6,3,28,29,30,
         17,19,18,16,20) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  rename(Site=Study.Area,
         '% non-native spp.'=freq.I,
         '% cavity-nester spp.'=cav.sp.freq,
         '% parrot-spp.'=freq.parrots,
         'Cavities/sqm'=cavs_canopy_sqm,
         'Interspecific interactions'=n_ints
         ) %>% 
  kable(align = 'lllccccccccc') %>% 
  add_header_above(header=c(" "=4,
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
# SITE DESCRIPTIONS----
#========================#
Indices_2 %>% 
  filter(Study.Area!='Changi Airport') %>% 
  group_by(Study.Area) %>% 
  mutate(built_surf=sum(buildpc+artsurfacepc)) %>% 
  ggplot(aes(built_surf,Richness))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Proportion urban land cover',y='Shannon')
## BD and Richness declines with building and road cover
Indices_2 %>% 
  filter(Study.Area!='Changi Airport') %>% 
  group_by(Study.Area) %>% 
  mutate(vegcan=sum(canopypc+Vegpc)) %>% 
  ggplot(aes(vegcan,Richness))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  labs(x='Proportion greenery cover',y='Shannon')
## BD and Richness increases with total greenery cover 
## canopy cover and vegetation alone have minimal correlation, but 
  # together have a multiplicative effect

Indices_2 %>% 
  filter(Study.Area!='Changi Airport') %>% 
  ggplot(aes(n_ints,freq.I))+
  geom_point(aes(color=Study.Area))+
  stat_poly_line(se=F)+
  stat_poly_eq()
   
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================== INTERACTIONS ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#

#pilot

Pilot %>% 
  ggplot(aes(time,freq))+
  geom_col()+
  labs(x='Time of day',y='Interaction frequency',
       title = 'Pilot study observations: Half-hourly interaction frequency')+
  theme_pubclean()+
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



