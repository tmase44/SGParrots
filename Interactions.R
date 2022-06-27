# LOAD PACKS----
library(pacman)
p_load(reshape2,formattable,knitr,kableExtra,tidyverse,vegan,
       lubridate,gridExtra,circlize,stringr,readxl,wesanderson)

# IMPORT DATA----
Interact <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Interactions")
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
  group_by(species,role,interaction,rating,outcome) %>%
  summarise(total=sum(n))

isrs2$interaction<-factor(isrs2$interaction,
                        levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact","Fight"))
isrs2$outcome<-factor(isrs2$outcome,
                          levels = c("W","NE","L"))
levels(isrs2$interaction)
levels(isrs2$species)
levels(isrs2$outcome)



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
  scale_y_continuous(expand = c(0,2))+
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

## who interacted with who MATRIX----
tmp <- Interact %>% select(initsp,recipsp) %>% group_by(initsp,recipsp) %>% mutate(count=n()) %>% 
  distinct(initsp,recipsp,.keep_all = TRUE) # remove dups
tmp <- pivot_wider(tmp,names_from = recipsp, values_from = count) %>% replace(is.na(.), 0) %>% arrange(initsp)
tmp <- tmp %>% column_to_rownames(var="initsp") # initsp to row names
tmp <- tmp %>% select(order(colnames(tmp))) # cols A-Z
view(tmp)
# creating correlation matrix
corr_mat <- round(cor(data),2)
# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
view(melted_corr_mat)
melted_corr_mat %>% ggplot(aes(Var1,Var2,fill=value))+geom_tile()

#2. ROLES----
## n IS RS----
isrs2 %>% 
  group_by(species,role) %>% 
  summarise(n=sum(total)) %>% 
  ggplot(aes(reorder(species,-n),n,fill=role))+
  geom_col(position = 'stack')+ 
  geom_text(aes(label = n),position=position_stack(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='n interactions intiated and recieved')+
  scale_fill_manual(values=c('IS'='#4a7b77','RS'='#f67e4b'))

# % IS RS----
isrs2 %>% 
  group_by(species,role) %>% 
  summarise(n=sum(total)) %>%
  mutate(freq=n/sum(n)*100) %>% 
  ggplot(aes(reorder(species,-n),freq,fill=role))+
  geom_col(position = 'fill')+theme_minimal()+
  geom_text(aes(label = round(freq,1)),position=position_fill(vjust=.5))+ 
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='%',title='Proportion interactions intiated and recieved')+
  scale_fill_manual(values=c('IS'='#4a7b77','RS'='#f67e4b'))

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
#### ..all species----
isrs %>%  
  group_by(species,outcome) %>% 
  summarise(n=sum(n)) %>% 
  ggplot(aes(reorder(species,n),n,fill=outcome))+
  geom_col(position = 'stack')+coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
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
  scale_fill_manual(values=c('W'='#4393c3','NE'='#f7f7f7','L'='#d6604d'))+
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
  ggplot(aes(interaction,freq,fill=species))+geom_col(width=0.95)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,40)+
  theme(legend.position = 'none')+labs(y='relative frequency',x='interaction',title='Interaction distribution: positively skewed')+
  facet_wrap(~species)+
  scale_fill_manual(values=c('Red-breasted parakeet'='#CC3311',
                             'Monk parakeet'='#004488',
                             'Rose-ringed parakeet'='#EE3377',
                             'Tanimbar corella'='#33BBEE',
                             'Long-tailed parakeet'='#009988'))


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
  geom_smooth()+
  xlim(0,80)+
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
  xlim(0,80)+
  labs(y='Species observed',x='distance from cavity',title='Distance of interaction from the nearest cavity')+
  theme(legend.position = 'none')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311',
                             'Monk parakeet'='#004488',
                             'Rose-ringed parakeet'='#EE3377',
                             'Tanimbar corella'='#33BBEE',
                             'Long-tailed parakeet'='#009988'))
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


v<-lm(nxt_cav~rating+interaction,data=Interact)
summary(v)


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

#kable
kbl(actions) %>% kable_material("striped") %>%
  row_spec(1,bold=T,background = '#f8eac1')

# WIN AGG SCORE----
ratingW<-Interact %>% select(initsp,interaction,isout,rating) %>% filter(interaction!='Neutral'& isout=="W") %>%    
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  group_by(initsp) %>% summarise('WIN Aggression score'=round(mean(rating),2)) %>% 
  arrange(match(initsp,c("Rose ringed parakeet","Tanimbar corella","Red-breasted parakeet","Monk parakeet","Long-tailed parakeet"))) %>%
  rename(Species=initsp) #%>% column_to_rownames(var="Species")
view(ratingW)

actionsW<-Interact %>% select(initsp,interaction,isout,rating) %>% filter(interaction!='Neutral'&isout=='W') %>%   
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  group_by(initsp,interaction) %>% 
  tally() %>% mutate(freq=round(n/sum(n)*100,2)) %>% select(-n) %>% 
  spread(key=interaction,value = freq) %>% replace(is.na(.), 0) %>% 
  arrange(match(initsp,c("Rose-ringed parakeet","Tanimbar corella","Red-breasted parakeet",
                         "Monk parakeet","Long-tailed parakeet"))) %>% rename(Species=initsp)
actionsW<-cbind(actionsW,ratingW[,2]) %>% relocate(1,8,2,3,4,5,6,7)
view(actionsW)

formattable(actionsW,
            align=c('r','c','c','c','c','c','c','c','c'),
            list(`Species` = formatter("span", style = ~ style(font.weight = "bold")),
                 'Neutral'=color_tile(customL,customH),
                 'Displace'=color_tile(customL,customH),
                 'Threat'=color_tile(customL,customH),
                 'Swoop'=color_tile(customL,customH),
                 'Chase'=color_tile(customL,customH),
                 'Contact'=color_tile(customL,customH),
                 'Fight'=color_tile(customL,customH)))

# site split----

rating2<-Interact %>% 
  select(Study.Area,initsp,interaction,rating) %>%filter(interaction!='Neutral') %>%    
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  group_by(Study.Area,initsp) %>% summarise('Aggression score'=round(mean(rating),2)) %>% 
  spread(key=Study.Area,value = 'Aggression score') %>% 
  arrange(match(initsp,c("Rose-ringed parakeet","Tanimbar corella","Red-breasted parakeet",
                         "Monk parakeet","Long-tailed parakeet"))) %>% 
  rename(Species=initsp)
rating2<-cbind(rating2,rating[,2,drop=FALSE]) %>% relocate(1,5,2,3,4)
view(rating2)

formattable(rating2,
            align=c('r','c','c','c','c'),
            list(`Species` = formatter("span", style = ~ style(font.weight = "bold"))))


rating %>% 
  ggplot(aes(sample=`Aggression score`))+
  stat_qq()

rating %>% 
  ggplot(aes(`Aggression score`))+
  geom_density()

isrs<-rbind(initiators,recipients)


# boxpoint all ints----
isrs2.lm %>% 
  ggplot(aes(species,rating))+
  geom_boxplot()+labs(x='Species',y='Interaction type',title='Interaction distribution by species')+
  geom_jitter(aes(color=interaction),width=0.1,alpha=0.23)
##violin----
isrs2.lm %>% 
  ggplot(aes(species,rating))+
  geom_violin()+
  geom_jitter(aes(color=interaction),width=0.2,height=0.3,alpha=0.3)


# species count x interactions----  

abun<-Composition %>% 
  filter(Species=="Red-breasted parakeet" | Species=="Tanimbar corella"|
           Species=="Rose-ringed parakeet"|Species=="Long-tailed parakeet"|
           Species=="Monk parakeet") %>% 
  select(Species) %>% count(Species)
  
recips<-Interact2 %>%
  select(initsp,recipsp) %>% 
  group_by(initsp) %>% mutate(n=n_distinct(recipsp)) %>% 
  summarise(recipsp_tot=mean(n))

abun<-cbind(abun,recips[,2])
abun %>% ggplot(aes(n,recips))+
  geom_point(aes(color=Species,size=4))+
  ylim(5,30)+
  labs(y='Numer of recipient species',x='n parrots observed',
       title = 'Parrot abundance vs number of species involved in interactions')


