# LOAD PACKS----
library(pacman)
p_load(formattable,knitr,kableExtra,tidyverse,vegan,
       lubridate,gridExtra,circlize,stringr,readxl,ggpmisc)


# IMPORT DATA----
old_Composition <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Composition")

ls(Composition)


# factorize
old_Composition$Species<-factor(old_Composition$Species)
old_Composition$Region.Label<-factor(old_Composition$Region.Label)
old_Composition$Study.Area<-factor(old_Composition$Study.Area)
old_Composition$ampm<-factor(old_Composition$ampm)
levels(old_Composition$Study.Area)

# Daily trend---
Daily.richness<-old_Composition %>% 
  select(Study.Area,Surveyno,Species) %>% 
  group_by(Study.Area,Surveyno) %>% 
  mutate(n = n_distinct(Species)) %>% ungroup() %>% select(-Species)

Daily.richness %>% 
  ggplot(aes(Surveyno,n,color=Study.Area))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))+
  labs(y = 'n species detected', x='survey number',
       title = 'Daily composition: detected unique species')


#histogram line parrots
old_Composition %>% 
  filter(Species=="Red-breasted parakeet" | Species=="Tanimbar corella"|
           Species=="Rose-ringed parakeet"|Species=="Long-tailed parakeet"|
           Species=="Monk parakeet") %>% 
  ggplot(aes(distance,color=Species,fill=Species)) + 
  geom_density(adjust=2,alpha=0.1)+labs(title = "Transect observations: Parrots")


# Summary of the survey----
unique(Composition$Species) # by name
n_distinct(Composition$Species) # by qty

#MAX daily counts----
Comp.max<-old_Composition %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% summarise(max_obs = max(n)) %>% 
  arrange(Study.Area,desc(max_obs))
#view(Comp.max)

comp.all<-old_Composition %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% summarise(sum=sum(n))

Comp.max %>% 
  ggplot(aes(Study.Area,max_obs))+
  geom_jitter(aes(color=Species),width=0.12,size=5,alpha=0.6,shape=20)+
  coord_trans(y='log10')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311',
                              'Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377',
                              'Tanimbar corella'='#33BBEE',
                              'Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))+
  theme_light()+
  labs(title = 'Max daily counts per site, species',color="Species")#change legend title!!

# no log scale
Comp.max %>% 
  ggplot(aes(Study.Area,max_obs))+
  geom_jitter(aes(color=Species),width=0.12,size=5,alpha=0.6,shape=20)+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311',
                              'Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377',
                              'Tanimbar corella'='#33BBEE',
                              'Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))+
  theme_light()+
  labs(title = 'Max daily counts per site, species',color="Species")#change legend title!!

# spread/gather for alpha measurement----
Comp.alpha<-comp.all %>% spread(key=Species,value = sum) %>% 
  replace(is.na(.), 0) %>% remove_rownames %>% 
  column_to_rownames(var="Study.Area")
#view(Comp.alpha)

#Richness----
fun.1<-function(x){sum(x>0)}
richness<-apply(Comp.alpha, 1, FUN=fun.1)
richness<-data.frame(richness)
colnames(richness)<-"Richness"
#view(richness)

#Shannon index----
for (Comp.alpha.row in 1:8)
{shannon<- matrix(diversity(Comp.alpha[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(Comp.alpha)
colnames(shannon)<-"Shannon"
#view(shannon)

#Simpson index----
for (Comp.alpha.row in 1:8)
{simpson<- matrix(diversity(Comp.alpha[,], index = "simpson"))}
simpson<-round(simpson,3)
#Adjusting the names of rows and columns
row.names(simpson)<-row.names(Comp.alpha)
colnames(simpson)<-"Simpson"
#view(simpson)

#Putting together all indices
Indices<-cbind(richness, shannon, simpson)
Indices<-data.frame(Indices)
#View(Indices)
#indices$Richness<-factor(indices$Richness)

# plot indices----
plot.indices<-Indices %>% 
  ggplot(aes(x=Simpson,y=Shannon,
                           label=row.names(Indices))) +
  geom_point(aes(color=Richness), size=4)+
  geom_text(hjust=0.7,vjust=-1.2)+
  labs(title = 'Alpha biodiversity of each survey site')
plot.indices
# table
formattable(Indices)

# species proportion per site----
Comp.max2<-Comp.max %>% 
  group_by(Study.Area) %>% 
  mutate(Proportion=round(max_obs/sum(max_obs)*100,2))

# propo + indices in one table
Comp.max3<-Comp.max2 %>% filter(Species=="Monk parakeet"|Species=="Red-breasted parakeet"|Species=="Tanimbar corella"|
           Species=="Long-tailed parakeet"|Species=="Rose-ringed parakeet") %>% 
  group_by(Study.Area) %>% 
  summarise(sum(Proportion))
Indices2<-cbind(richness, shannon, simpson,Comp.max3) %>% rename(ParrProp=5)
#rm(Indices2)
# plot proportion at sites----
plot.prop<-Comp.max2 %>% 
  filter(Species=="Monk parakeet"|Species=="Red-breasted parakeet"|Species=="Tanimbar corella"|
           Species=="Long-tailed parakeet"|Species=="Rose-ringed parakeet") %>% 
  ggplot(aes(Study.Area,Proportion,color=Species,size=max_obs))+
  geom_point()+labs(title = 'Proportion of parrots in community')  
plot.prop
#table
plot.prop.table<-Comp.max2 %>% 
  filter(Species=="Monk parakeet"|Species=="Red-breasted parakeet"|Species=="Tanimbar corella"|
           Species=="Long-tailed parakeet"|Species=="Rose-ringed parakeet")

grid.arrange(plot.indices,plot.prop,ncol=2)

#???????????????????????????????????????
#        TOP LINE SUMMARIES----
#???????????????????????????????????????

# 1. Richness x parrot proportion----
richxprop <-Indices2 %>% 
  ggplot(aes(ParrProp,Richness))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Parrot proportion in the community positively correlates with overall species Richness',
       y='Species richness',x='Proportion of parrots in the community')
#R2 = 0.85
## 85% variation in Richness is attributed to parrot abundance
y<-lm(Richness~ParrProp,Indices2)
summary(y)

shannxprop <-Indices2 %>% 
  ggplot(aes(ParrProp,Shannon))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Parrot proportion in the community positively correlates with Alpha Biodoversity',
       y='Shannon biodiversity',x='Proportion of parrots in the community')
#R2 = 0.75
## 75% variation in biodoversity (shann) is attributed to parrot abundance
y1<-lm(Shannon~ParrProp,Indices2)
summary(y1)
## plot----
grid.arrange(richxprop,shannxprop,ncol=2)

## Tanimbar corella may have a negative effect on Richness and BD within an area
## Stirling RD & CV are lowest on Rich&BD

# 2. initiated interactions----
#x<-isrsall %>% 
  filter(role=='IS') %>% group_by(Study.Area) %>% 
  summarise(allIS=sum(total))
#Indices2<-cbind(Indices2,x[,2])

# parrot prop in community != more interactions
isxprop<-Indices2 %>% 
  ggplot(aes(ParrProp,allIS))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Greater proportion of parrots does not predict greater interaction frequency',
       y='n interactions',x='Proportion of parrots in the community')
# R2 = 0.06
## only 6% variation of interaction frequency is explained by parrot abundance
y<-lm(allIS~ParrProp,Indices2)
summary(y)

isxrich<-Indices2 %>% 
  ggplot(aes(Richness,allIS))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Richness of site is also not a driver of interaction frequency',
       y='n interactions',x='Species richness')
# R2 = <0.1%
##plot----
grid.arrange(isxprop,isxrich,ncol=2)

# 3. average aggression rating by site----
r<-isrsall %>% filter(interaction!='Neutral') %>% 
  group_by(Study.Area) %>% 
  summarise(avgrating=mean(rating))
Indices2<-cbind(Indices2,r['avgrating'])

shanxagg<-Indices2 %>% 
  ggplot(aes(avgrating,Shannon))+
  stat_poly_line(se=F)+
    stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'On average, more aggressive interactions at less diverse sites',
       x='Average aggression rating',y='Shannon biodiversity')
# R2 = 0.58
richxagg<-Indices2 %>% 
  ggplot(aes(avgrating,Richness))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'On average, more aggressive interactions at less species rich sites',
       x='Average aggression rating',y='Species richness')
# R2 = 0.3
propxagg<-Indices2 %>% 
  ggplot(aes(avgrating,ParrProp))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Sites with greatest % of parrots have less aggressive interactions',
       x='Average aggression rating',y='Proportion of parrots in the community(%)')
# R2 = 0.43
## details of the species at each site may go a way to explain why this is

grid.arrange(shanxagg,richxagg,propxagg,ncol=3)

# Append environ data----
Enviro <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                     sheet = "Enviro")
Enviro<-Enviro %>% filter(Study.Area!='Palawan Beach')

envpc<-Enviro %>% 
  select(canopypc,Vegpc,buildpc,surfacepc,waterpc)
Indices2<-cbind(Indices2,envpc)

# 4. Land type x ints----
isxcanopy<-Indices2 %>% 
  ggplot(aes(canopypc,allIS))+stat_poly_line(se=F)+stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Correlation between Canopy cover and n interactions')
# R2 = 0.46
## 46% variation of interaction frequency is explained by % canopy cover
## canopy cover associated with cavity-suitable trees
### No relationship between urban area,water body area, 
### total vegetation area, road area
isxveg<-Indices2 %>% 
  ggplot(aes(Vegpc,allIS))+stat_poly_line(se=F)+stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Correlation between vegetation cover and n interactions')
isxbuild<-Indices2 %>% 
  ggplot(aes(buildpc,allIS))+stat_poly_line(se=F)+stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Correlation between building cover and n interactions')
isxsurface<-Indices2 %>% 
  ggplot(aes(surfacepc,allIS))+stat_poly_line(se=F)+stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Correlation between artificial surface cover and n interactions')

grid.arrange(isxcanopy,isxveg,isxbuild,isxsurface,ncol=2,nrow=2)

# ints / hr----
Indices2<-cbind(Indices2,m[,2])


Indices2 %>% 
  ggplot(aes(ParrProp,meanintshr))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
# interactions per hour is greater in low BD sites

