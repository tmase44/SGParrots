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


  
  