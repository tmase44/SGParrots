# LOAD PACKS----
library(pacman)
p_load(formattable,knitr,kableExtra,tidyverse,vegan,
       lubridate,gridExtra,circlize,stringr,readxl,wesanderson)


# IMPORT DATA----
Composition <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Composition")
ls(Composition)

# factorize
Composition$Species<-factor(Composition$Species)
Composition$Region.Label<-factor(Composition$Region.Label)
Composition$Study.Area<-factor(Composition$Study.Area)
Composition$ampm<-factor(Composition$ampm)

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
           Species=="Rose ringed parakeet"|Species=="Long tailed parakeet"|
           Species=="Monk parakeet") %>% 
  ggplot(aes(Distance,color=Species,fill=Species)) + 
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

Comp.max %>% ggplot(aes(Study.Area,max_obs))+
  geom_jitter(aes(color=Species),width=0.18,size=3,alpha=0.5)+coord_trans(y='log2')+
  scale_color_manual(values=c('Red-breasted parakeet'='red','Monk parakeet'='#3ACF3A','Rose ringed parakeet'='purple',
                              'Tanimbar corella'='orange','Long tailed parakeet'='#1DACE8',"Others"="dark grey"))+
  #ylim(0,40)+ # outlier 75 mynas 
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
for (Comp.alpha.row in 1:4)
{shannon<- matrix(diversity(Comp.alpha[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(Comp.alpha)
colnames(shannon)<-"Shannon"
#view(shannon)

#Simpson index----
for (Comp.alpha.row in 1:4)
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
  xlim(0.8,0.98)+ylim(1.8,4.1)+
  geom_text(hjust=0.5,vjust=-1)+
  labs(title = 'Alpha biodiversity of each survey site')
plot.indices
# table
formattable(Indices)

# species proportion per site
Comp.max2<-Comp.max %>%
  group_by(Study.Area) %>% 
  mutate(Proportion=round(max_obs/sum(max_obs)*100,2))

# plot proportion at sites----
plot.prop<-Comp.max2 %>% 
  filter(Species=="Monk parakeet"|Species=="Red-breasted parakeet"|Species=="Tanimbar corella"|
           Species=="Long tailed parakeet"|Species=="Rose ringed parakeet") %>% 
  ggplot(aes(Study.Area,Proportion,color=Species,size=max_obs))+
  geom_point()+labs(title = 'Proportion of parrots in community')  
plot.prop
#table
plot.prop.table<-Comp.max2 %>% 
  filter(Species=="Monk parakeet"|Species=="Red-breasted parakeet"|Species=="Tanimbar corella"|
           Species=="Long tailed parakeet"|Species=="Rose ringed parakeet")
  
formattable(plot.prop.table)

grid.arrange(plot.indices,plot.prop,ncol=2)
