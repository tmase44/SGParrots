# LOAD PACKS----
library(pacman)
p_load(formattable,knitr,kableExtra,tidyverse,vegan,
       lubridate,gridExtra,circlize,stringr,readxl,ggpmisc)


# IMPORT DATA----
Composition <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Composition")
Composition<-Composition %>% filter(Study.Area!='Palawan Beach')
ls(Composition)


# factorize
Composition$Species<-factor(Composition$Species)
Composition$Region.Label<-factor(Composition$Region.Label)
Composition$Study.Area<-factor(Composition$Study.Area)
Composition$ampm<-factor(Composition$ampm)
levels(Composition$Study.Area)

# Daily trend---
Daily.richness<-Composition %>% 
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
Composition %>% 
  filter(Species=="Red-breasted parakeet" | Species=="Tanimbar corella"|
           Species=="Rose-ringed parakeet"|Species=="Long-tailed parakeet"|
           Species=="Monk parakeet") %>% 
  ggplot(aes(distance,color=Species,fill=Species)) + 
  geom_density(adjust=2,alpha=0.1)+labs(title = "Transect observations: Parrots")


# Summary of the survey----
unique(Composition$Species) # by name
n_distinct(Composition$Species) # by qty

#MAX daily counts----
Comp.max<-Composition %>% 
  group_by(Study.Area,Species,Surveyno) %>% 
  summarise(n=n()) %>% 
  select(-Surveyno) %>% summarise(max_obs = max(n)) %>% 
  arrange(Study.Area,desc(max_obs))
#view(Comp.max)

Comp.max %>% 
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

# spread/gather for alpha measurement----
Comp.alpha<-Comp.max %>% spread(key=Species,value = max_obs) %>% 
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
for (Comp.alpha.row in 1:5)
{shannon<- matrix(diversity(Comp.alpha[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(Comp.alpha)
colnames(shannon)<-"Shannon"
#view(shannon)

#Simpson index----
for (Comp.alpha.row in 1:5)
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
  geom_point(aes(color=Richness), size=4) +
  xlim(0.85,0.98)+ylim(1.8,4.1)+
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

# Richness x parrot proportion----
Indices2 %>% 
  ggplot(aes(ParrProp,Richness))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Parrot proportion in the community positively correlates with overall species Richness')
#R2 = 0.85
## 85% variation in Richness is attributed to parrot abundance

Indices2 %>% 
  ggplot(aes(ParrProp,Shannon))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Parrot proportion in the community positively correlates with Alpha Biodoversity')
#R2 = 0.75
## 75% variation in biodoversity (shann) is attributed to parrot abundance

## Tanimbar corella may have a negative effect on Richness and BD within an area
## Stirling RD & CV are lowest on Rich&BD

#all initated interactions----
x<-isrsall %>% 
  filter(role=='IS') %>% group_by(Study.Area) %>% 
  summarise(allIS=sum(total))
Indices2<-cbind(Indices2,x[,2])

# parrot prop in community != more interactions----
Indices2 %>% 
  ggplot(aes(ParrProp,allIS))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Greater proportion of parrots does not predict greater interaction frequency')
# R2 = 0.06
## only 6% variation of interaction frequency is explained by parrot abundance

# Append environ data
Enviro <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                          sheet = "Enviro")
Enviro<-Enviro %>% filter(Study.Area!='Palawan Beach')

envpc<-Enviro %>% 
  select(canopypc,Vegpc,buildpc,surfacepc,waterpc)
Indices2<-cbind(Indices2,envpc)

# Canopy cover x ints----
Indices2 %>% 
  ggplot(aes(canopypc,allIS))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+labs(title = 'Some correlation between Canopy cover and n interactions')
# R2 = 0.46
## 46% variation of interaction frequency is explained by % canopy cover
## canopy cover associated with cavity-suitable trees
### No relationship between urban area,water body area, 
### total vegetation area, road area

