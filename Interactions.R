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
Interact$Study.Area<-as.factor(Interact$Study.Area)
Interact$initsp<-as.factor(Interact$initsp)
Interact$recipsp<-as.factor(Interact$recipsp)
Interact$interaction<-as.factor(Interact$interaction)
Interact$isout<-as.factor(Interact$isout)
Interact$rsout<-as.factor(Interact$rsout)
#Interaction ratings
Interact<-Interact %>% 
  mutate(rating=case_when(
    interaction=="Neutral"~"1",
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
Interact %>% 
  filter(interaction!="Neutral") %>% 
  group_by(initsp,recipsp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(initsp)) %>% 
  arrange(desc(n))

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
#...no filter
isrsall<-isrs %>% 
  group_by(species,role,interaction,outcome) %>%
  summarise(total=sum(n))
view(isrsall)
isrsall$interaction<-factor(isrsall$interaction,
                          levels = c("Neutral","Displace","Swoop","Threat","Chase","Contact","Fight"))
isrsall$outcome<-factor(isrsall$outcome,
                      levels = c("W","NE","L"))

#... parrot filter
isrs2<-isrs %>% 
  filter(species=="Monk parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet") %>%  
  group_by(species,role,interaction,outcome) %>%
  summarise(total=sum(n))
view(isrs2)
isrs2$interaction<-factor(isrs2$interaction,
                        levels = c("Neutral","Displace","Swoop","Threat","Chase","Contact","Fight"))
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

#...1 Total interactions----

isrsall %>% group_by(species) %>% summarise(n=sum(total)) %>% 
  ggplot(aes(reorder(species,n),n,fill=species))+ # this order high to low
  geom_col(position='dodge',alpha=0.8)+coord_flip()+
  labs(x='Species',y='total interactions',title='Total interactions')+
  scale_y_continuous(expand = c(0,2))+ 
  scale_fill_manual(values=c('Red-breasted parakeet'='red','Monk parakeet'='#3ACF3A','Rose ringed parakeet'='purple',
                              'Tanimbar corella'='orange','Long-tailed parakeet'='#1DACE8',"Others"="dark grey"))
  
#...2 Parrot total interactions
isrs2 %>% 
  group_by(species) %>% 
  summarise(n=sum(total)) %>% 
  ggplot(aes(reorder(species,-n),n))+
  geom_col()+
  geom_text(aes(label = n), vjust = -0.5)+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='Total interactions')

#...2 Roles----
#...# n imitated and received interactions
isrs2 %>% 
  group_by(species,role) %>% 
  summarise(n=sum(total)) %>% 
  ggplot(aes(reorder(species,-n),n,fill=role))+
  geom_col(position = 'stack')+ 
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='n interactions intiated and recieved')+
  scale_fill_manual(values=c('IS'='#456355','RS'='#FCD16B'))

#... Proportion of imitated and received interactions
isrs2 %>% 
  group_by(species,role) %>% 
  summarise(n=sum(total)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(species,-n),freq,fill=role))+
  geom_col(position = 'fill')+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion interactions intiated and recieved')+
  scale_fill_manual(values=c('IS'='#456355','RS'='#FCD16B'))

#...n W/L all interactions----
isrs2 %>% 
  group_by(species,outcome) %>% 
  summarise(n=sum(total)) %>% 
  ggplot(aes(reorder(species,-n),n,fill=outcome))+
  geom_col(position = 'stack')+ 
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='n wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#00BFC4','NE'='#C4CFD0','L'='#F8766D'))

#... Proportion W/L/NE
isrs2 %>% 
  group_by(species,outcome) %>% 
  summarise(n=sum(total)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(species,-n),freq,fill=outcome))+
  geom_col(position = 'fill')+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#00BFC4','NE'='#C4CFD0','L'='#F8766D'))

#.... facet IS RS
isrs2 %>% 
  filter(outcome!="NE") %>% 
  group_by(species,role,outcome) %>% 
  summarise(n=sum(total)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(species,-n),freq,fill=outcome))+
  geom_col(position = 'fill')+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#00BFC4','NE'='#C4CFD0','L'='#F8766D'))+
  facet_wrap(~role)

#grid.arrange(p3,p4,p5,ncol=3)

# PROXIMITY TO NEST

Interact %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet"| initsp=="Javan myna") %>%  
  ggplot(aes(nxt_cav,color=interaction))+
  geom_jitter(stat='count',size=4,alpha=0.7)+xlim(0,100)+
  facet_wrap(~initsp)

