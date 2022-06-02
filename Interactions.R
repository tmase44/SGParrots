# LOAD PACKS----
library(pacman)
p_load(tidyverse,vegan,lubridate,gridExtra,circlize,stringr,readxl,wesanderson)


# IMPORT DATA----
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
    interaction=="Contact"~"6",
    interaction=="Fight"~"6"))
Interact$rating<-factor(Interact$rating,
                                levels = c("1","2","3","4","5","6"))
#... remove intra-specifics----
#Interact2<-Interact %>% 
 # filter(initsp!="Long-tailed parakeet" | recipsp!="Long-tailed parakeet")#



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
#... parrot filter
isrs2<-isrs %>% 
  filter(species=="Monk parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet"| species=="Javan myna") %>%  
  group_by(species,role,interaction,outcome) %>%
  summarise(total=sum(n))
view(isrs2)
isrs2$interaction<-factor(isrs2$interaction,
                        levels = c("NE","Displace","Swoop","Threat","Chase","Contact","Fight"))
isrs2$outcome<-factor(isrs2$outcome,
                          levels = c("W","NE","L"))
unique(isrs2$interaction)
levels(isrs2$species)
levels(isrs2$outcome)



#CHARTS----
# clean wrapped labels!!!!
isrs2$species2 = str_wrap(isrs2$species, width = 10)
isrs2
#palettes https://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html 

#...Total interactions----
isrs2 %>% 
  group_by(species) %>% 
  summarise(n=sum(total)) %>% 
  ggplot(aes(species,n))+
  geom_col()+
  geom_text(aes(label = n), vjust = -0.5)+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='Total interactions')

# n Interaction species----
isrs2 %>% 
  ggplot(aes(interaction,total,fill=species))+
  geom_col()+
  facet_wrap(~species,2,3)+
  theme(axis.text.x = element_text
        (angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none')+
  labs(x='Interaction',y='n',title='n Interaction type by species')

#...Roles----
isrs2 %>% 
  group_by(species,role) %>% 
  summarise(n=sum(total)) %>% 
  ggplot(aes(species,n,fill=role))+
  geom_col(position = 'stack')+ #W/N/L side by side
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='Role in interactions (initiator [IS] or recipient [RS]')+
  scale_fill_brewer(palette="Set3")

#...W/L proportion----
isrs2 %>% 
  ggplot(aes(species,total,fill=outcome))+
  geom_col(position='fill')+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Wins, Losses, Neutral outcomes')+
  scale_fill_brewer(palette="Spectral",direction=-1)

#...W/L proportion----
isrs2 %>% 
  ggplot(aes(species,total,fill=outcome))+
  geom_col(position='fill')+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Outcomes when IS or RS')+
  scale_fill_brewer(palette="Spectral",direction=-1)+
  facet_wrap(~role)

# ...Outcome proportion by species----
isrs2 %>% 
  filter(outcome!='NE') %>% 
  ggplot(aes(interaction,total,fill=outcome))+
  geom_col(position = 'fill')+
  facet_wrap(~species,2,3)+
  theme(axis.text.x = element_text
        (angle = 90, vjust = 0.5, hjust=1,size=7),
        legend.position = 'none')+
  labs(x='Interaction',y='%',title='Win/Loss proportion by species & action')


# ...Interaction proportion----
isrs2 %>% 
  ggplot(aes(species,total,fill=interaction))+
  geom_col(position='fill')+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion of parrot interactions')+
  theme_bw()+
  scale_fill_brewer(palette = "RdPu")

#...... By role----                    
isrs2 %>% 
  ggplot(aes(species,total,fill=interaction))+
  geom_col(position='fill')+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion of parrot interactions wether IS or RS')+
  scale_fill_brewer(palette = "RdPu")+
  facet_wrap(~role)

#......by W/L----
isrs2 %>% 
  filter(outcome!="NE") %>% 
  ggplot(aes(species,total,fill=interaction))+
  geom_col(position='fill')+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion of interactions Won or Lost')+
  scale_fill_brewer(palette = "YlGn")+
  facet_wrap(~outcome)



# Interaction proportions BY SPECIES
isrs2 %>% 
  filter(outcome!='NE') %>% 
  ggplot(aes(species,total,fill=outcome))+
  geom_col(position = 'fill')+
  facet_wrap(~interaction,2,3)+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
    theme(legend.position = 'none',
          axis.text.x = element_text(size = 6.5))+
  labs(x='Interaction',y='n',title='Win/Loss proportion by action & species')



# CHORD DATA PREP----
# prepare data frame
intchord<-Interact %>% 
  filter(recipsp!="NA") %>% 
  filter(initsp=="Monk parakeet"|
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet") %>%  
  group_by(initsp,recipsp,isout) %>% 
  tally() %>% 
  filter(n>1) %>% 
  select(-isout)
view(intchord)

levels(intchord$recipsp)

intchord<-intchord %>% 
  mutate(IS=case_when(
    initsp=="Monk parakeet"~"MP",initsp=="Tanimbar corella"~"TC",initsp=="Rose ringed parakeet"~"RRP",
           initsp=="Red-breasted parakeet"~"RBP",initsp=="Long-tailed parakeet"~"LTP"))
intchord<-intchord %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",
    recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    

inchord2<-intchord %>% ungroup %>% select(IS,RS,n)
    
# CHORD----
#https://r-graph-gallery.com/chord-diagram.html
  # Transform input data in a adjacency matrix
    #adjacencyData <- with(intchord, table(initsp, recipsp))
#http://opencolor.tools/palettes/wesanderson/ 

# Make the circular plot
chordDiagram(inchord2,grid.col=grid.col)
# transparency can be called above, default transparency = 0.5
circos.clear()

#... multi chord!!!!!----
par(mfrow = c(2, 3))
grid.col=c("MP"="#76A08A","TC"="#9A872D","RRP"="#F7B0AA","RBP"="#B62A3D","LTP"="#EDCB64",
                     "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey",        
                     "HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey",
                     "LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey",
                     "DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col)
title(main = "Parrot initiated interactions")
# highlight MP
grid.col.mp=c("MP"="#76A08A","TC"="grey","RRP"="grey","RBP"="grey","LTP"="grey",
           "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col.mp)
title(main = "Monk parakeet initiated interactions")
# highlight RBP
grid.col.rbp=c("MP"="grey","TC"="grey","RRP"="grey","RBP"="#B62A3D","LTP"="grey",
              "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col.rbp)
title(main = "Red-breasted parakeet initiated interactions")
# highlight TC
grid.col.tc=c("MP"="grey","TC"="#9A872D","RRP"="grey","RBP"="grey","LTP"="grey",
              "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col.tc)
title(main = "Tanimbar corella initiated interactions")
# highlight RRP
grid.col.rrp=c("MP"="grey","TC"="grey","RRP"="#F7B0AA","RBP"="grey","LTP"="grey",
              "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col.rrp)
title(main = "Rose-ringed parakeet initiated interactions")
# highlight LTP
grid.col.ltp=c("MP"="grey","TC"="grey","RRP"="grey","RBP"="grey","LTP"="#EDCB64",
              "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col.ltp)
title(main = "Long-tailed parakeet initiated interactions")
