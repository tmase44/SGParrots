library(tidyverse)
library(vegan)
library(lubridate)
library(gridExtra)

# ALL INTER-DATA----
library(readxl)
Interact <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Interactions")
View(Interact)
ls(Interact)
unique(Interact$initsp)

# Total intiations----
IS<-Interact %>% 
  filter(interaction!="NE") %>% 
  group_by(initsp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(freq))
view(IS)
# only 6 species initiated ----
  # TC initiated the most 33.6% of all

# Total receipts----
RS<-Interact %>% 
  filter(interaction!="NE") %>% 
  group_by(recipsp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(freq))
view(RS)
# RBP on the receiving end of most aggression, 33.6%

interactions<-Interact %>% 
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
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeett") 
# number
plot_wl_n<-interactionspar %>% 
  ggplot(aes(n,initsp,fill=isout))+
  geom_col()+
  geom_text(aes(label=round(n,digits = 1)),
            position = position_stack(vjust = .5))+
  theme_bw()+
  labs(title="Aggressor species W/L number")+
  theme(legend.position = 'bottom')
# proportion
plot_wl_f<-interactionspar %>% 
  ggplot(aes(freq,initsp,fill=isout))+
  geom_col()+
  geom_text(aes(label=round(freq,digits = 1)),
            position = position_stack(vjust = .5))+
  theme_bw()+
  labs(title="Aggressor species W/L proportion")+
  theme(legend.position = 'bottom',
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
grid.arrange(plot_wl_n,plot_wl_f,nrow=1,ncol=2)

# win loss species facet----
facet grid panels species and species, wins and losses

# winning (or losing) aggressions by type----
interactiontypeW<-Interact %>% # change L W
  filter(interaction!="NE")%>%
  filter(isout=="W") %>% #change
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
                                              "Tanimbar corella","Monk Parakeet","Long-tailed parakeett"))
levels(interactiontypeW$initsp)

interactiontypeW %>% 
  filter(initsp=="Monk Parakeet"|
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeett") %>% 
  ggplot(aes(n,initsp,fill=interaction))+
  geom_col()+
  theme_bw()+
  labs(title="Aggressor wins by type")

# plot aggressions that failed

interactiontypeL %>% 
  filter(initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Monk Parakeet"|
           initsp=="Long-tailed parakeett") %>% 
  ggplot(aes(n,initsp,fill=interaction))+
  geom_col()+
  theme_bw()+
  labs(title="Aggressor losses by type")

interactiontype %>% 
  filter(initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Monk Parakeet"|
           initsp=="Long-tailed parakeett") %>% 
  ggplot(aes(freq,initsp,fill=interaction))+
  geom_col()+
  theme_bw()+
  labs(title="Proportion of aggressions")

interactiontype$initsp<-factor(interactiontype$initsp,
                                levels = c("Yellow crested cockatoo","Rose ringed parakeet","Red-breasted parakeet",
                                           "Tanimbar corella","Monk Parakeet"))
levels(interactiontype$initsp)


