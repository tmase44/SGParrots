# LOAD PACKS----
library(pacman)
p_load(reshape2,formattable,knitr,kableExtra,tidyverse,vegan,
       lubridate,gridExtra,circlize,stringr,readxl,BBmisc,ggpmisc)
#library(caret)


# IMPORT DATA----
Interact <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Interactions")
Interact<-Interact %>% filter(Study.Area!='Palawan Beach')
#View(Interact)
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
    interaction=="Neutral"~"0",
    interaction=="Displace"~"1",
    interaction=="Threat"~"2",
    interaction=="Swoop"~"3",
    interaction=="Chase"~"4",
    interaction=="Contact"~"5",
    interaction=="Fight"~"6"))
Interact$rating<-as.numeric(Interact$rating)

Interact$interaction<-factor(Interact$interaction,
                          levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact","Fight"))


#PARROTS ONLY----
Interact2<-Interact %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|
                                 initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  mutate(SP=case_when(initsp=="Monk parakeet"~"MP",
                     initsp=="Tanimbar corella"~"TC",
                     initsp=="Rose-ringed parakeet"~"RRP",
                     initsp=="Red-breasted parakeet"~"RBP",
                     initsp=="Long-tailed parakeet"~"LTP"))


# ...all initators----
initiators<-Interact %>% 
  filter(recipsp!="NA") %>% 
  group_by(Study.Area,initsp,interaction,rating,isout,) %>% 
  tally()
initiators<-rename(initiators,species=initsp)  
initiators<-rename(initiators,outcome=isout)
initiators$role<-'IS' # identify IS/RS
# ...all recipients----
recipients<-Interact %>% 
  filter(recipsp!="NA") %>% 
  group_by(Study.Area,recipsp,interaction,rating,rsout) %>% 
  tally()
recipients<-rename(recipients,species=recipsp)  
recipients<-rename(recipients,outcome=rsout)
recipients$role<-'RS' # identify IS/RS
#...combine----
isrs<-rbind(initiators,recipients)

#...no filter
isrsall<-isrs %>% 
  group_by(Study.Area,species,role,interaction,rating,outcome) %>%
  summarise(total=sum(n))

isrsall$interaction<-factor(isrsall$interaction,
                          levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact","Fight"))
isrsall$outcome<-factor(isrsall$outcome,
                      levels = c("W","NE","L"))

#... parrot filter
isrs2<-isrs %>% 
  filter(species=="Monk parakeet"|species=="Tanimbar corella"|species=="Rose-ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet") %>%  
  group_by(species,Study.Area,role,interaction,rating,outcome) %>%
  summarise(total=sum(n))

isrs2$interaction<-factor(isrs2$interaction,
                        levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact","Fight"))
isrs2$outcome<-factor(isrs2$outcome,
                          levels = c("W","NE","L"))
levels(isrs2$interaction)
levels(isrs2$species)
levels(isrs2$outcome)

# DATA SUMMARY ----
Composition %>% summarise(n_distinct(Species)) ### 78 species observed in total
isrs %>% ungroup() %>%  summarise(n_distinct(species)) ### 48 species observed interacting
Interact %>% count(initsp) %>% summarise(sum(n)) ### 790 interactions in total
Interact %>% summarise(n_distinct(initsp)) ### 30 species observed initiating interactions
Interact %>% summarise(n_distinct(recipsp)) ### 44 species observed receiving interactions

#CHARTS----
# clean wrapped labels!!!!
isrs2$species2 = str_wrap(isrs2$species, width = 10)
isrs2
#palettes https://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html 

# 1. SUMMARY----

##Total interactions----
isrsall %>% group_by(species) %>% summarise(n=sum(total)) %>% 
  ggplot(aes(reorder(species,n),n,fill=species))+ # this order high to low
  geom_col(position='dodge',alpha=0.8)+coord_flip()+
  labs(x='Species',y='total interactions',title='Total interactions')+
  theme(legend.position = 'none')+
  scale_fill_manual(values=c('Red-breasted parakeet'='#CC3311',
                             'Monk parakeet'='#004488',
                             'Rose-ringed parakeet'='#EE3377',
                             'Tanimbar corella'='#33BBEE',
                             'Long-tailed parakeet'='#009988',"Others"="dark grey"))
 
##Parrot all interactions----
isrs2 %>% 
  group_by(species) %>% 
  summarise(n=sum(total)) %>% 
  ggplot(aes(reorder(species,-n),n))+
  geom_col()+
  geom_text(aes(label = n), vjust = -0.5)+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='Total interactions')

# install and loading packages
#install.packages("BBmisc")
#library(BBmisc)

# method = range for normalisation ----
isrsnorm <- isrsall %>% group_by(species) %>% summarise(n=sum(total)) %>% arrange(desc(n))
scaled_df_norm = normalize(isrsnorm[,2], method = "range", range = c(0, 1))
colnames(scaled_df_norm)[1]<-"total_norm"
summary(scaled_df_norm)
isrsnorm<-cbind(isrsnorm,scaled_df_norm)

# Involvement in interactions
isrsnorm %>%  
  ggplot(aes(reorder(species,total_norm),total_norm))+
  geom_col(position='dodge',alpha=0.8)+coord_flip()+
  labs(x='Species',y='proportion of interactions',title='Frequency of observed interactions')

# Proportion of IS
isrsall %>% group_by(species) %>% filter(role=='IS') %>% 
  summarise(n=sum(total)) %>% arrange(desc(n)) %>% 
  mutate(freq=n/sum(n)*100) %>% filter(freq>1) %>% 
  ggplot(aes(reorder(species,freq),freq))+
  geom_col(position='dodge',alpha=0.8)+coord_flip()+
  labs(x='Species',y='proportion of interactions',title='Frequency of intiated interactions')

# Proportion of RS
isrsall %>% group_by(species) %>% filter(role=='RS') %>% 
  summarise(n=sum(total)) %>% arrange(desc(n)) %>% 
  mutate(freq=n/sum(n)*100) %>% filter(freq>1) %>% 
  ggplot(aes(reorder(species,freq),freq))+
  geom_col(position='dodge',alpha=0.8)+coord_flip()+
  labs(x='Species',y='proportion of interactions',title='Frequency of received interactions')

# Proportion of RS with parrots removed as RS
isrsall %>% group_by(species) %>% 
  filter(species!="Monk parakeet"&species!="Tanimbar corella"&species!="Rose-ringed parakeet"&
           species!="Red-breasted parakeet") %>%
  filter(role=='RS') %>% 
  summarise(n=sum(total)) %>% arrange(desc(n)) %>% 
  mutate(freq=n/sum(n)*100) %>%
  ggplot(aes(reorder(species,freq),freq))+
  geom_col(position='dodge',alpha=0.8)+coord_flip()+scale_y_log10()+
  labs(x='Species',y='proportion of interactions',title='Frequency of received interactions without non-native parrots')



#2. ROLES----
## n IS RS----
isrs2 %>% filter(interaction!='Neutral') %>% 
  group_by(species,role) %>% 
  summarise(n=sum(total)) %>% 
  ggplot(aes(reorder(species,-n),n,fill=role))+
  geom_col(position = 'stack')+ 
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='n interactions intiated and recieved')+
  scale_fill_manual(values=c('IS'='#4a7b77','RS'='#f67e4b'))

# % IS RS----
isrs2 %>% filter(interaction!='Neutral') %>% 
  group_by(species,role) %>% 
  summarise(n=sum(total)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(species,-n),freq,fill=role))+
  geom_col(position = 'fill')+theme_minimal()+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion interactions intiated and recieved')+
  scale_fill_manual(values=c('IS'='#4a7b77','RS'='#f67e4b'))

# Neutral----
isrs2 %>% filter(interaction=='Neutral') %>% 
  group_by(species) %>% 
  summarise(n=sum(total)) %>%
    ggplot(aes(reorder(species,-n),n))+
  geom_col()+theme_minimal()+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='n neutral interactions')

# 3. W/L/NE summary
## n W/L/NE all ints----
isrs2 %>% 
  group_by(species,outcome) %>% 
  summarise(n=sum(total)) %>% 
  ggplot(aes(reorder(species,-n),n,fill=outcome))+
  geom_col(position = 'stack')+ 
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='n wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#4393c3','NE'='#f7f7f7','L'='#d6604d'))

## % W/L/NE all ints----
isrs2 %>% 
  group_by(species,outcome) %>% 
  summarise(n=sum(total)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(species,-n),freq,fill=outcome))+
  geom_col(position = 'fill')+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#4393c3','NE'='#f7f7f7','L'='#d6604d'))

## W/L by IS RS----
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
  scale_fill_manual(values=c('W'='#4393c3','L'='#d6604d'))+
  facet_wrap(~role)

#3. INTERACTIONS----
## IS split interaction types----
### absolute freq (n)----
isrs2 %>% 
  filter(role=='IS') %>% 
  ggplot(aes(interaction,total))+geom_col(width=0.95)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = 'none')+labs(y='absolute frequency',x='interaction',title='Interaction distribution: positively skewed')+
  facet_wrap(~species,scales='free')#+
  scale_fill_manual(values=c('Red-breasted parakeet'='#CC3311',
                             'Monk parakeet'='#004488',
                             'Rose-ringed parakeet'='#EE3377',
                             'Tanimbar corella'='#33BBEE',
                             'Long-tailed parakeet'='#009988'))

### relative freq (%)----
isrs2 %>% 
  filter(role=='IS') %>% group_by(species) %>% mutate(freq=total/sum(total)*100) %>% 
  ggplot(aes(interaction,freq))+geom_col(width=0.95)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)+
  theme(legend.position = 'none')+labs(y='relative frequency',x='interaction',title='Interaction distribution: positively skewed')+
  facet_wrap(~species)

# a bar chart has to be used because the variable of interest (interaction)
  # is not continuous numeric. It is continuous because interactions
    # are on a scale of least-most aggressive.
# 0 base value where 0 = not observed
# 6 bins = 6 distinct escalation levels
# distribution = skew right
#https://chartio.com/learn/charts/histogram-complete-guide/ 

  #grid.arrange(p3,p4,p5,ncol=3)

# PROXIMITY TO NEST----
## all interactions
Interact %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  ggplot(aes(y=interaction,nxt_cav))+geom_jitter(aes(color=initsp),width=3,alpha=0.6,size=1)+
  labs(y='observation n',x='distance from cavity',title='Distance of interaction from the nearest cavity')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311',
                             'Monk parakeet'='#004488',
                             'Rose-ringed parakeet'='#EE3377',
                             'Tanimbar corella'='#33BBEE',
                             'Long-tailed parakeet'='#009988'))
  
## species on Y axis
Interact %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  ggplot(aes(nxt_cav,initsp))+
  geom_jitter(aes(color=initsp),width=2,height=0.1,alpha=0.4,size=3,shape=20)+
  labs(y='Species observed',x='distance from cavity',title='Distance of interaction from the nearest cavity')+
  theme(legend.position = 'none')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311',
                             'Monk parakeet'='#004488',
                             'Rose-ringed parakeet'='#EE3377',
                             'Tanimbar corella'='#33BBEE',
                             'Long-tailed parakeet'='#009988'))

Interact %>% filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%
  filter(interaction!='Neutral') %>% 
  ggplot(aes(initsp,nxt_cav))+
  geom_boxplot()

Interact %>% filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%
  filter(interaction!='Neutral') %>% 
  ggplot(aes(initsp,food_dis))+
  geom_boxplot()


## foods
Interact %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  ggplot(aes(food_dis,initsp))+
  geom_jitter(aes(color=initsp),width=2,height=0.1,alpha=0.4,size=3,shape=20)+
  labs(y='Species observed',x='distance from food',title='Distance of interaction from the nearest food source')+
  theme(legend.position = 'none')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311',
                              'Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377',
                              'Tanimbar corella'='#33BBEE',
                              'Long-tailed parakeet'='#009988'))
# TABLES----

## true pop mean----
tmean<-Interact %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  select(interaction,rating) %>% 
  summarise('Aggression score'=round(mean(rating),2))
#1.63
tactions<-Interact %>%   
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  select(interaction,rating) %>% group_by(interaction) %>% 
  tally() %>% mutate(freq=round(n/sum(n)*100,2)) %>% select(-n) %>% 
  spread(key=interaction,value = freq) %>% replace(is.na(.), 0.001) %>% 
  add_column(Species='Population means', .before = 1)
tactions<-cbind(tactions,tmean[,1]) %>% relocate(1,9,2,3,4,5,6,7,8)
#view(tactions)

## AGGRESSION SCORE----
rating<-Interact %>% select(initsp,interaction,rating)%>%    
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  group_by(initsp) %>% summarise('Aggression score'=round(mean(rating),2)) %>% 
  arrange(match(initsp,c("Rose-ringed parakeet","Tanimbar corella","Red-breasted parakeet","Monk parakeet","Long-tailed parakeet"))) %>%
  rename(Species=initsp) #%>% column_to_rownames(var="Species")
#view(rating)
### Actions----
actions<-Interact %>% select(initsp,interaction,rating)%>%   
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  group_by(initsp,interaction) %>% 
  tally() %>% mutate(freq=round(n/sum(n)*100,2)) %>% select(-n) %>% 
  spread(key=interaction,value = freq) %>% replace(is.na(.), 0) %>% 
  arrange(match(initsp,c("Rose-ringed parakeet","Tanimbar corella","Red-breasted parakeet",
                             "Monk parakeet","Long-tailed parakeet"))) %>% rename(Species=initsp)
actions<-cbind(actions,rating[,2]) %>% relocate(1,9,2,3,4,5,6,7,8)
actions<-rbind(actions,tactions[1,])
#view(actions)
actions$Species<-actions$Species %>%
  factor(levels=c("Population means","Rose-ringed parakeet","Tanimbar corella","Red-breasted parakeet",
                             "Monk parakeet","Long-tailed parakeet")) 
actions<-actions %>% arrange(Species)

# ...formattable----
formattable(actions,
            align=c('r','c','c','c','c','c','c','c','c'),
            list(`Species` = formatter("span", style = ~ style(font.weight = "bold")),
                 'Neutral'=color_tile(customL,customH),
                 'Displace'=color_tile(customL,customH),
                 'Threat'=color_tile(customL,customH),
                 'Swoop'=color_tile(customL,customH),
                 'Chase'=color_tile(customL,customH),
                 'Contact'=color_tile(customL,customH),
                 'Fight'=color_tile(customL,customH),
                 'Aggression score'=color_tile(customL,customH))) 


# !! ABUN X INTS----  
abun<-Composition %>% 
  select(Species) %>% count(Species) %>% arrange(desc(n)) # n = total observed
  
recips<-isrsall %>%  
  group_by(species) %>% summarise(allints=sum(total)) %>% rename(Species=species) #sum = total interactions

abun<-merge(abun,recips,by="Species",all=T) %>%  replace(is.na(.), 0) 
  
abun %>% ggplot(aes(n,allints,color=Species))+
  geom_point(size=3,alpha=0.8)+
  labs(x='total abundance',y='total interaction involvement',
       title = 'Total observations / Total times involved in interactions')+
  scale_x_log10()+scale_y_log10()+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='purple'))

abun %>%  ggplot(aes(n,allints))+
  stat_poly_line()+
  stat_poly_eq()+
  geom_point(size=3,alpha=0.8)+
  labs(x='total abundance',y='total interaction involvement',
       title = 'Total observations / Total times involved in interactions')+
  scale_x_log10()+scale_y_log10()


## BY SITE!!! ----
abun2<-Composition %>% 
  select(Study.Area,Species) %>%  group_by(Study.Area,Species) %>% count(Species) %>% arrange(desc(n)) # n = total observed

recips2<-isrsall %>%  
  group_by(Study.Area,species) %>% summarise(allints=sum(total)) %>% rename(Species=species) #sum = total interactions

abun2<-merge(abun2,recips2,by=c("Study.Area","Species")) 

abun2 %>% filter(Study.Area!='Palawan Beach') %>% ggplot(aes(n,allints,color=Species))+
  geom_point(size=3,alpha=0.8)+
  labs(x='total observations',y='total interaction involvement',
       title = 'Total observations / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))+
  facet_wrap(~Study.Area)

abun2 %>% filter(Study.Area!='Palawan Beach') %>% 
  ggplot(aes(n,allints))+
  stat_poly_line()+
  stat_poly_eq()+
  geom_point(size=3,alpha=0.8)+
  labs(x='total observations',y='total interaction involvement',
       title = 'Total observations / Total times involved in interactions')+
  facet_wrap(~Study.Area)

### Add aggression rating
rm(abun3)
rm(agg)
agg<-isrsall %>%  
  mutate(agg=rating*total) %>% 
  group_by(Study.Area,species) %>% summarise(Aggression_weight=sum(agg)) %>% rename(Species=species) %>% 
  arrange(desc(Aggression_weight))

abun3<-merge(abun2,agg,by=c("Study.Area","Species")) 

abun3 %>% ggplot(aes(n,allints,color=Species,size=Aggression_weight))+
  geom_point(alpha=0.8)+
  labs(x='total observations',y='total interaction involvement',
       title = 'Total observations / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))+
  facet_wrap(~Study.Area)

DAB_master2<-merge(DAB_master,abun3,by=c('Study.Area','Species'))
DAB_master2<-merge(DAB_master2,nInts2,by=c('Study.Area','Species'))

# total ints x relative abundance----
DAB_master2 %>% ggplot(aes(Abundance,allints,color=Species))+
  geom_point(alpha=0.8)+
  labs(x='total observations',y='total interaction involvement',
       title = 'Total observations / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))+
  facet_wrap(~Study.Area)

# only IS x relative abundance----
DAB_master2 %>% ggplot(aes(Abundance,n,color=Species))+
  stat_poly_line()+
  stat_poly_eq()+
  geom_point(alpha=0.8)+
  labs(x='total observations',y='total interaction involvement',
       title = 'Total observations / Total times initiating interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))+
  facet_wrap(~Study.Area)

# only IS x relative abundance----
DAB_master2 %>% ggplot(aes(Abundance,n))+
  stat_poly_line()+
  stat_poly_eq()+
  geom_point(alpha=0.8)+
  labs(x='relative abundance',y='total interaction involvement',
       title = 'Total observations / Total times initiating interactions')+
  facet_wrap(~Study.Area)


## APPEND TO THIS CAVITY NESTER OR NOT!!! ----

x<-lm(n~allints,abun)
summary(x)

# N site visits / n ints per site----
Composition %>% group_by(Study.Area) %>% 
  filter(Study.Area!='Palawan Beach') %>%
  summarise(hours=max(Surveyno))
hourly<-Interact %>% filter(interaction!='Neutral') %>% group_by(Study.Area) %>% count(interaction) %>% 
  mutate(hours=case_when(Study.Area=='Changi Village'~10,
                         Study.Area=='Pasir Ris Town Park'~15,
                         Study.Area=='Palawan Beach'~1,
                         Study.Area=='Sengkang Riverside Park'~4,
                         Study.Area=='Springleaf'~10,
                         Study.Area=='Stirling Road'~6)) %>% 
  mutate(ints_hr=n/hours) %>% group_by(Study.Area) %>% mutate(sum=sum(n)) %>% mutate(sum_hr=sum(ints_hr)) %>% 
  group_by(Study.Area) %>% mutate(avg_hr=mean(ints_hr))
hourly
Indices3<-full_join(hourly,Indices2,by='Study.Area')
Indices3<-full_join(Indices3,cavs,by='Study.Area')
Indices3$interaction<-factor(Indices3$interaction,
                             levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact","Fight"))



