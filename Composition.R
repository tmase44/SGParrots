library(tidyverse)
library(vegan)
library(lubridate)

changiv<-read.csv("Composition/Comp_ChangiVill.csv")
dim(changiv)
head(changiv)
class(changiv)
summary(changiv)
ls(changiv)

# factorize
changiv$Object<-factor(changiv$Object)
changiv$Region.Label<-factor(changiv$Region.Label)
changiv$Study.Area<-factor(changiv$Study.Area)
changiv$Sample.Label<-factor(changiv$Sample.Label)
changiv$AMPM<-factor(changiv$AMPM)

# dates 
changiv$Date<-dmy(changiv$Date)

# hist
changiv %>% 
  filter(Distance<=50) %>% 
  ggplot(aes(Distance))+
  geom_histogram(bins = 10,
                 colour="black",fill="white")+
  scale_x_continuous(breaks=seq(0,50,by=5))+
  labs(title="Transect observations: Changi Village")

# line hist
changiv %>% 
  ggplot(aes(Distance)) + 
  geom_density(adjust=2)

#for parrots
rbpobs<-changiv %>% 
  filter(Object=="Red-breasted parakeet" | Object=="Tanimbar corella")

rbpobs %>% 
  ggplot(aes(Distance,color=Object)) + 
  geom_density(adjust=2)


# Summary of the survey----
unique(changiv$Object) # by name
n_distinct(changiv$Object) # by qty = 32

# total species count, proportion total and daily obs
changiv %>%
  group_by(Object)%>%
  summarize(n=n())%>%
  mutate(freq=n/sum(n)*100) %>% 
  mutate(average_obs=(n)/8) %>% 
  arrange(desc(freq))
# with DATE variable
changiv2<-changiv %>%
  group_by(Date,Object)%>%
  summarize(n=n())%>%
  mutate(freq=n/sum(n)*100) %>% 
  arrange(desc(freq))
#daily ALPHA measurement
changialpha<-changiv2 %>% 
  select(-freq) %>% 
  spread(key=Object,value=n) %>% 
  replace(is.na(.), 0) %>% 
  remove_rownames %>% 
  column_to_rownames(var="Date")
view(changialpha)

#Richness----
fun.1<-function(x){sum(x>0)}
ch_richness<-apply(changialpha, 1, FUN=fun.1)
richness<-data.frame(ch_richness)
colnames(richness)<-"Richness"
view(richness)

#Shannon index----
for (changialpha.row in 1:4)
{shannon<- matrix(diversity(changialpha[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(changialpha)
colnames(shannon)<-"Shannon"
view(shannon)

#Simpson index----
for (changialpha.row in 1:4)
{simpson<- matrix(diversity(changialpha[,], index = "simpson"))}
simpson<-round(simpson,3)
#Adjusting the names of rows and columns
row.names(simpson)<-row.names(changialpha)
colnames(simpson)<-"Simpson"
view(simpson)

#Putting together all indices
indices<-cbind(richness, shannon, simpson)
indices<-data.frame(indices)
View(indices)
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

top2_changiv2<-changiv2 %>% 
  filter(Object=="Javan myna" | Object==
           "Red-breasted parakeet" | Object==
           "Tanimbar corella") %>% 
  select(-freq) %>% 
  spread(key=Object,value=n) %>% 
  replace(is.na(.), 0) %>% 
  remove_rownames %>% 
  column_to_rownames(var="Date")
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
indices %>% 
  ggplot(aes(x=Simpson,y=Shannon,
                           label=row.names(indices))) +
  geom_point(aes(color=Richness), size=4) +
  geom_text(hjust=-0.2,vjust=0.1)+
  ylim(1.5,2.5)+xlim(0.72, 0.88)

# plot all obs by species----
changiv2 %>% 
  ggplot(aes(freq,n,color=freq))+
  geom_point(size=1.5)+
  facet_wrap(~Object,ncol =6)+
  theme(legend.position = "right")+
  labs(x="frequency",y="n",
       title = "Frequency and number of observations by species")

changiv2 %>% 
  ggplot(aes(freq,n,color=Object))+
  geom_point(size=2)+
  theme_bw()+
  theme(legend.position = "right")+
  labs(x="frequency",y="n",
       title = "Frequency and number of observations by species")+
  scale_colour_manual(values = unique(changiv2$Object), breaks=c("Red-breasted parakeet","Javan myna",
                    "Tanimbar corella","House crow"))

# ALL COMP DATA----
library(readxl)
Composition <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                                       sheet = "Composition")
View(Composition)

x<-Composition %>% 
  select(Study.Area,dayno,Object) %>% 
  group_by(Study.Area,dayno) %>% 
  mutate(n = n_distinct(Object))
view(x)
  
x %>% 
  ggplot(aes(dayno,n,color=Study.Area))+
  geom_smooth(size=0.5)+
  geom_point()+
  ylim(10,40)+
  labs(y = 'n species detected', x='survey day',
       title = 'Daily composition: detected unique species')
  