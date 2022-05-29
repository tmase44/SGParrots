library(tidyverse)
library(vegan)
library(lubridate)

isichang<-read.csv("Interaction/inter.csv")
dim(isichang)
head(isichang)
class(isichang)
summary(isichang)
ls(isichang)
unique(isichang$initsp)

# Total intiations----
IS<-isichang %>% 
  filter(interaction!="NE") %>% 
  group_by(initsp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(freq))
view(IS)
# only 6 species initiated ----
  # TC initiated the most 33.6% of all

# Total receipts----
RS<-isichang %>% 
  filter(interaction!="NE") %>% 
  group_by(recipsp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(freq))
view(RS)
# RBP on the receiving end of most aggression, 33.6%

interactions<-isichang %>% 
  filter(interaction!="NE") %>% 
  group_by(initsp,isout) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>% 
  arrange(desc(isout)) %>% 
  arrange(desc(freq))
view(interactions)

# plot aggressor wins and losses
interactionspar<-interactions %>% 
  filter(initsp=="Monk Parakeet"|
           initsp=="Yellow crested cockatoo"|
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet") 

interactionspar %>% 
  ggplot(aes(freq,initsp,fill=isout))+
  geom_col()+
  theme_bw()+
  labs(title="Aggressor species wins and losses")+



# winning (or losing) aggressions by type----
interactiontypeL<-isichang %>% # change L W
  filter(interaction!="NE")%>%
  filter(isout=="L") %>% #change
  group_by(initsp,interaction) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>% 
  arrange(desc(initsp))
view(interactiontypeL)


interactiontype$initsp<-as.factor(interactiontype$initsp)
# plot winning aggressions
  # first arrange factors
interactiontypeW$initsp<-factor(interactiontypeW$initsp,
                                   levels = c("Yellow crested cockatoo","Rose ringed parakeet","Red-breasted parakeet",
                                              "Tanimbar corella","Monk Parakeet"))
levels(interactiontypeW$initsp)

interactiontypeW %>% 
  filter(initsp=="Monk Parakeet"|
           initsp=="Yellow crested cockatoo"|
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet") %>% 
  ggplot(aes(n,initsp,fill=interaction))+
  geom_col()+
  theme_bw()+
  labs(title="Aggressor wins by type")

# plot aggressions that failed

interactiontypeL %>% 
  filter(initsp=="Yellow crested cockatoo"|
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Monk Parakeet") %>% 
  ggplot(aes(n,initsp,fill=interaction))+
  geom_col()+
  theme_bw()+
  labs(title="Aggressor losses by type")

interactiontype %>% 
  filter(initsp=="Yellow crested cockatoo"|
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Monk Parakeet") %>% 
  ggplot(aes(freq,initsp,fill=interaction))+
  geom_col()+
  theme_bw()+
  labs(title="Proportion of aggressions")

interactiontype$initsp<-factor(interactiontype$initsp,
                                levels = c("Yellow crested cockatoo","Rose ringed parakeet","Red-breasted parakeet",
                                           "Tanimbar corella","Monk Parakeet"))
levels(interactiontype$initsp)
