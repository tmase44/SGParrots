# LOAD PACKS----
library(pacman)
p_load(tidyverse,vegan,lubridate,gridExtra,circlize,stringr,readxl,wesanderson)


# IMPORT DATA----
Composition <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Composition")
ls(Composition)

# factorize
Composition$Object<-factor(Composition$Object)
Composition$Region.Label<-factor(Composition$Region.Label)
Composition$Study.Area<-factor(Composition$Study.Area)
changiv$Sample.Label<-factor(Composition$Sample.Label)
Composition$AMPM<-factor(Composition$AMPM)

# Daily trend---
Daily.richness<-Composition %>% 
  select(Study.Area,Surveyno,Object) %>% 
  group_by(Study.Area,Surveyno) %>% 
  mutate(n = n_distinct(Object)) %>% ungroup() %>% select(-Object)

Daily.richness %>% 
  ggplot(aes(Surveyno,n,color=Study.Area))+
  geom_line()+
  geom_point()+
  ylim(10,30)+
  labs(y = 'n species detected', x='survey number',
       title = 'Daily composition: detected unique species')

Daily.interact<-Interact %>% 
  group_by(site,surveyno) %>% 
  mutate(n = n_distinct(id)) %>% ungroup() %>% select(site,surveyno,n)
Daily.interact %>% 
  ggplot(aes(surveyno,n,color=site))+
  geom_line()+
  geom_point()+
  labs(y = 'n interactions observed', x='survey number',
       title = 'Daily interactions observed')




# hist
Composition %>% 
  filter(Distance<=150) %>% 
  ggplot(aes(Distance))+
  geom_histogram(bins = 10,
                 colour="black",fill="white")+
  scale_x_continuous(breaks=seq(0,50,by=5))+
  labs(title="Transect observations")+
  facet_wrap(~Study.Area)

# line hist
Composition %>% 
  ggplot(aes(Distance)) + 
  geom_density(adjust=2)

#for parrots
Composition %>% 
  filter(Object=="Red-breasted parakeet" | Object=="Tanimbar corella"|
           Object=="Rose ringed parakeet"|Object=="Long tailed parakeet"|
           Object=="Monk parakeet") %>% 
  ggplot(aes(Distance,color=Object)) + 
  geom_density(adjust=2)


# Summary of the survey----
unique(Composition$Object) # by name
n_distinct(Composition$Object) # by qty

# total species count, proportion total and daily obs
Compsum<-Composition %>%
  group_by(Study.Area,Object)%>%
  summarize(n=n())%>%
  mutate(freq=n/sum(n)*100) %>% 
  mutate(average_obs=(n)/8) %>% 
  arrange(Study.Area,desc(freq))
view(Compsum)
# with DATE variable
Compsumdat<-Composition %>%
  group_by(Study.Area,date,Object)%>%
  summarize(n=n())%>%
  mutate(freq=n/sum(n)*100) %>% 
  arrange(Study.Area,desc(freq))
#daily ALPHA measurement --> make per plot
alpha_chang<-Compsumdat %>% 
  filter(Study.Area=="Changi Village") %>% #change this
  select(-freq) %>% spread(key=Object,value=n) %>% 
  replace(is.na(.), 0) %>% remove_rownames %>% 
  column_to_rownames(var="date") %>% 
  ungroup() %>% select(-Study.Area)
view(alpha_chang)

#Richness----
fun.1<-function(x){sum(x>0)}
ch_richness<-apply(alpha_chang, 1, FUN=fun.1)
richness<-data.frame(ch_richness)
colnames(richness)<-"Richness"
view(richness)

#Shannon index----
for (alpha_chang.row in 1:4)
{shannon<- matrix(diversity(alpha_chang[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(alpha_chang)
colnames(shannon)<-"Shannon"
view(shannon)

#Simpson index----
for (alpha_chang.row in 1:4)
{simpson<- matrix(diversity(alpha_chang[,], index = "simpson"))}
simpson<-round(simpson,3)
#Adjusting the names of rows and columns
row.names(simpson)<-row.names(alpha_chang)
colnames(simpson)<-"Simpson"
view(simpson)

#Putting together all indices
Indices.Changi<-cbind(richness, shannon, simpson)
Indices.Changi<-data.frame(Indices.Changi)
View(Indices.Changi)
#indices$Richness<-factor(indices$Richness)

# taking the total daily count, then daily specific species----
  # and getting the proportion
top_changiv2<-changiv2 %>% 
  select(-freq) %>% 
  spread(key=Object,value=n) %>% 
  replace(is.na(.), 0) %>% 
  remove_rownames %>% 
  column_to_rownames(var="Date") %>% 
  mutate(total_count = rowSums(across(where(is.numeric)))) %>% 
  select(total_count)
view(top_changiv2)

top2_changiv2<-Compsumdat %>% 
  filter(Object=="Monk parakeet" | Object=="Red-breasted parakeet" | Object==
           "Tanimbar corella"|Object=="Long tailed parakeet"|Object=="Rose ringed parakeet") %>% 
  select(-freq) %>% 
  spread(key=Object,value=n) %>% 
  replace(is.na(.), 0) %>% 
  remove_rownames %>% 
  column_to_rownames(var="date")
view(top2_changiv2) 

top2_changiv2<-cbind(top2_changiv2,top_changiv2)
top2_changiv2
#convert to %
top2_changiv2<-round((top2_changiv2/top2_changiv2$total_count)*100,2)
top2_changiv2

indices2<-cbind(indices,top2_changiv2) %>% 
  select(-total_count)
indices<-data.frame(indices)
View(indices2)

# plot indices----
Indices.Changi %>% 
  ggplot(aes(x=Simpson,y=Shannon,
                           label=row.names(Indices.Changi))) +
  geom_point(aes(color=Richness), size=4) +
  geom_text(hjust=-0.2,vjust=0.1)+
  ylim(1.5,2.5)+xlim(0.72, 0.88)

# plot all obs by species----
Compsum %>% 
  ggplot(aes(freq,n,color=freq))+
  geom_point(size=1.5)+
  facet_wrap(~Object,ncol =6)+
  theme(legend.position = "right")+
  labs(x="frequency",y="n",
       title = "Frequency and number of observations by species")

Compsum %>% 
  ggplot(aes(freq,n,color=Object))+
  geom_point(size=2)+
  theme_bw()+
  theme(legend.position = "right")+
  labs(x="frequency",y="n",
       title = "Frequency and number of observations by species")+
  scale_colour_manual(values = unique(Compsum$Object), breaks=c("Red-breasted parakeet","Monk parakeet",
                    "Tanimbar corella","Rose ringed parakeet","Long tailed parakeet"))



  