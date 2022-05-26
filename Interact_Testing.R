library(tidyverse)
rm(Survey_Data_Entry_Master)

ls(Survey_Data_Entry_Master)
Survey_Data_Entry_Master<-rename(Survey_Data_Entry_Master,
                                 Initiator = 'Initiator sp')
Survey_Data_Entry_Master$Initiator<-as.factor(Survey_Data_Entry_Master$Initiator)
Survey_Data_Entry_Master$Interaction<-as.factor(Survey_Data_Entry_Master$Interaction)

unique(Survey_Data_Entry_Master$Initiator)

# by nest dist----
filter<-Survey_Data_Entry_Master %>% 
  filter(Interaction!="Neutral") %>% 
  filter(`Recipient sp`!="N/A") %>% 
  filter(Initiator=="Monk Parakeet"|Initiator=="Red-breasted parakeet"|
           Initiator=="Tanimbar corella"|Initiator=="Javan Myna"|
           Initiator=="Rose ringed parakeet") %>% 
  group_by(Initiator,Interaction) %>%
  mutate(n=n()) %>% 
  mutate(freq=n/sum(n)*100)
view(filter)

filter%>% 
  ggplot(aes(`dist to closest cavity  (cm)`,freq,
             color=Interaction,
             alpha=1))+
  geom_jitter(height=0.5)+
  facet_wrap(~Initiator,2,3)+
  labs(title = "Distance (m) of initiated interactions from the nest")+
  theme_bw()

# by temp----
Survey_Data_Entry_Master %>% 
  filter(Interaction!="Neutral") %>% 
  filter(Initiator=="Monk Parakeet"|Initiator=="Red-breasted parakeet"|
           Initiator=="Tanimbar corella"|Initiator=="Javan Myna") %>% 
  ggplot(aes(Interaction,`Temp Celc.`,
             color=Interaction,
             alpha=2.5))+
  geom_count(position = 'dodge')+
   labs(title = "Initiated interaction variation by temperature (oC)")+
  ylim(25,35)

