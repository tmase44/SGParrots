# LOAD PACKS----
library(pacman)
p_load(tidyverse,vegan,lubridate,gridExtra,circlize,stringr)


# IMPORT DATA----
library(readxl)
Interact <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Interactions")
View(Interact)
ls(Interact)
unique(Interact$initsp)
unique(Interact$interaction)

#...Factorize----
Interact$initsp<-as.factor(Interact$initsp)
Interact$recipsp<-as.factor(Interact$recipsp)
Interact$interaction<-as.factor(Interact$interaction)
Interact$isout<-as.factor(Interact$isout)
Interact$rsout<-as.factor(Interact$rsout)
#Interaction ratings
Interact<-Interact %>% 
  mutate(rating=case_when(
    interaction=="NE"~"1",
    interaction=="Displace"~"2",
    interaction=="Swoop"~"3",
    interaction=="Threat"~"4",
    interaction=="Chase"~"5",
    interaction=="Contact"~"6"))
Interact$rating<-factor(Interact$rating,
                                levels = c("1","2","3","4","5"))

# SUBSETS----
IS<-Interact %>% 
  filter(interaction!="NE") %>% 
  group_by(initsp,recipsp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(initsp)) %>% 
  arrange(desc(n))
view(IS)

# ...all initators----
initiators<-Interact %>% 
  filter(recipsp!="NA") %>% 
  group_by(initsp,interaction,rating,isout) %>% 
  tally()
initiators<-rename(initiators,species=initsp)  
initiators<-rename(initiators,outcome=isout)
initiators$role<-'IS' # identify IS/RS
view(initiators)

# ...all recipients----
recipients<-Interact %>% 
  filter(recipsp!="NA") %>% 
  group_by(recipsp,interaction,rating,rsout) %>% 
  tally()
recipients<-rename(recipients,species=recipsp)  
recipients<-rename(recipients,outcome=rsout)
recipients$role<-'RS' # identify IS/RS
view(recipients)

#...combine----
isrs<-rbind(initiators,recipients)
view(isrs)
isrs2<-isrs %>% 
  group_by(species,interaction,outcome,role) %>%
  summarise(total=sum(n))
view(isrs2)
isrs2$interaction<-factor(isrs2$interaction,
                        levels = c("NE","Displace","Swoop","Threat","Chase","Contact"))
unique(isrs2$interaction)
  
#CHARTS----
# clean wrapped labels!!!!
isrs2$species2 = str_wrap(isrs2$species, width = 5)
isrs2

#...Total interactions----
isrs2 %>% 
  filter(species=="Monk Parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet"| species=="Javan myna") %>%  
  ggplot(aes(species,total))+
  geom_col()+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='Total interactions')

#...W/L proportion----
isrs2 %>% 
  filter(species=="Monk Parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet"|species=="Javan myna") %>%  
  ggplot(aes(species,total,fill=outcome))+
  geom_col(position='fill')+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Wins, Losses, Neutral outcomes')

# ...Interaction proportion----
isrs2 %>% 
  filter(species=="Monk Parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet"|species=="Javan myna") %>%  
  ggplot(aes(species,total,fill=interaction))+
  geom_col(position='fill')+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion of parrot interactions')

# .......by W/L----
isrs2 %>% 
  filter(species=="Monk Parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet"|species=="Javan myna") %>%  
  filter(outcome!='NE') %>% 
  ggplot(aes(species,total,fill=interaction))+
  geom_col(position='fill')+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion of winning & losing interactions')+
  facet_wrap(~outcome,,2)

# ...Outcome proportion by species----
isrs2 %>% 
  filter(species=="Monk Parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet"| species=="Javan myna") %>%  
  filter(outcome!='NE') %>% 
  ggplot(aes(interaction,total,fill=outcome))+
  geom_col(position = 'fill')+
  facet_wrap(~species,2,3)+
  theme(axis.text.x = element_text
        (angle = 90, vjust = 0.5, hjust=1,size=7),
        legend.position = 'none')+
  labs(x='Interaction',y='%',title='Win/Loss proportion by species & action')+

# Interaction proportions BY SPECIES
isrs2 %>% 
  filter(species=="Monk Parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet"| species=="Javan myna") %>%  
  filter(outcome!='NE') %>% 
  ggplot(aes(species,total,fill=outcome))+
  geom_col(position = 'fill')+
  facet_wrap(~interaction,2,3)+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
    theme(legend.position = 'none',
          axis.text.x = element_text(size = 6.5))+
  labs(x='Interaction',y='n',title='Win/Loss proportion by action & species')



# n Interaction type----
isrs2 %>% 
  filter(species=="Monk Parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet"| species=="Javan myna") %>%  
  ggplot(aes(interaction,total,fill=species))+
  geom_col()+
  facet_wrap(~species,2,3)+
  theme(axis.text.x = element_text
        (angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none')+
  labs(x='Interaction',y='n',title='n Interaction type by species')





#W/L facet----
isrs2 %>% 
  filter(species=="Monk Parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet"| species=="Javan myna") %>%  
  filter(outcome!="NE") %>% 
  ggplot(aes(interaction,total,fill=species))+
  geom_col()+
  facet_wrap(outcome~species,4,6)+
  theme(axis.text.x = element_text
        (angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none')+
  labs(x='Interaction',y='n',title='n Wins and Losses by interaction')




#--------------------

# plot aggressor wins and losses
interactionspar<-interactions %>% 
  filter(initsp=="Monk Parakeet"|
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet") 
# number
interactionspar %>% 
  ggplot(aes(n,initsp,fill=isout))+
  geom_col()+
  geom_text(aes(label=round(n,digits = 1)),
            position = position_stack(vjust = .5))+
  theme_bw()+
  labs(title="Aggressor species W/L number")
# proportion
interactionspar %>% 
  ggplot(aes(freq,initsp,fill=isout))+
  geom_col()+
  geom_text(aes(label=round(freq,digits = 1)),
            position = position_stack(vjust = .5))+
  theme_bw()+
  labs(title="Aggressor species W/L proportion")

# win loss species facet----
#facet grid panels species and species, wins and losses
interactions2<-Interact %>% 
  filter(recipsp!="NA") %>% 
  filter(initsp=="Monk Parakeet"|
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet") %>%  
  group_by(initsp,recipsp,interaction,isout) %>% 
  tally() %>% 
  filter(n>2) %>% 
  select(-isout)
view(interactions2)

# CHORD----
#https://r-graph-gallery.com/chord-diagram.html

# Transform input data in a adjacency matrix
adjacencyData <- with(interactions2, table(initsp, recipsp))
# Make the circular plot
circos.par(start.degree = 0)
chordDiagram(adjacencyData, grid.col = grid.col,big.gap = 20,
             transparency = 0.5)
abline(h = 0, lty = 2, col = "#00000080")
circos.clear()

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
                                              "Tanimbar corella","Monk Parakeet","Long-tailed parakeet"))
levels(interactiontypeW$initsp)

interactiontypeW %>% 
  filter(initsp=="Monk Parakeet"|
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet") %>% 
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
           initsp=="Long-tailed parakeet") %>% 
  ggplot(aes(n,initsp,fill=interaction))+
  geom_col()+
  theme_bw()+
  labs(title="Aggressor losses by type")

interactiontype %>% 
  filter(initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Monk Parakeet"|
           initsp=="Long-tailed parakeet") %>% 
  ggplot(aes(freq,initsp,fill=interaction))+
  geom_col()+
  theme_bw()+
  labs(title="Proportion of aggressions")

interactiontype$initsp<-factor(interactiontype$initsp,
                                levels = c("Yellow crested cockatoo","Rose ringed parakeet","Red-breasted parakeet",
                                           "Tanimbar corella","Monk Parakeet","Long-tailed parakeet"))
levels(interactiontype$initsp)

# interaction volume by location

y<-interactions %>% 
  select(site,dayno,Object) %>% 
  group_by(Study.Area,dayno) %>% 
  mutate(n = n_distinct(Object))
view(x)


