library(tidyverse)
library(vegan)
library(lubridate)
library(gridExtra)
library(circlize)

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
  group_by(initsp,recipsp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(initsp)) %>% 
  arrange(desc(n))
view(IS)
# only 6 species initiated ----
  # TC initiated the most 33.6% of all

# Total receipts----
RS<-Interact %>% 
  filter(interaction!="NE") %>% 
  group_by(recipsp,initsp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(recipsp)) %>% 
  arrange(desc(n))
view(RS)
# RBP on the receiving end of most aggression, 33.6%

interactions<-Interact %>% 
  filter(recipsp!="NA") %>% 
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


