library(tidyverse)
library(vegan)
library(lubridate)

isichang<-read.csv("Interaction/Inter_ChangiVill.csv")
dim(isichang)
head(isichang)
class(isichang)
summary(isichang)
ls(isichang)

# Total intiations----
IS<-isichang %>% 
  filter(Interaction!="NE") %>% 
  group_by(Initiator.sp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(freq))
view(IS)
# only 6 species initiated ----
  # TC initiated the most 33.6% of all

# Total receipts----
RS<-isichang %>% 
  filter(Interaction!="NE") %>% 
  group_by(Recipient.sp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(freq))
view(RS)
# RBP on the receiving end of most aggression, 33.6%

interactions<-isichang %>% 
  filter(Interaction!="NE") %>% 
  group_by(Initiator.sp,IS.outcome) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>% 
  arrange(desc(IS.outcome)) %>% 
  arrange(desc(freq))
view(interactions)

# plot aggressor wins and losses
interactions %>% 
  ggplot(aes(n,Initiator.sp,fill=IS.outcome))+
  geom_col()+
  theme_bw()+
  labs(title="Aggressor species wins and losses")

# winning aggressions by type
interactiontype<-isichang %>% 
  filter(Interaction!="NE")%>%
  filter(IS.outcome!="W") %>%  #change this----
  group_by(Initiator.sp,Interaction) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>% 
  arrange(desc(Initiator.sp))
view(interactiontype)

# plot winning aggressions
interactiontype %>% 
  ggplot(aes(n,Initiator.sp,fill=Interaction))+
  geom_col()+
  theme_bw()+
  labs(title="Aggressor wins by type")

# plot aggressions that failed
interactiontype %>% 
  ggplot(aes(n,Initiator.sp,fill=Interaction))+
  geom_col()+
  theme_bw()+
  labs(title="Aggressor losses by type")
