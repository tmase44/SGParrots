# LOAD PACKS----
library(pacman)
p_load(formattable,knitr,kableExtra,tidyverse,vegan,
       lubridate,gridExtra,circlize,stringr,readxl,wesanderson)

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
    interaction=="Neutral"~"0",
    interaction=="Displace"~"1",
    interaction=="Threat"~"2",
    interaction=="Swoop"~"3",
    interaction=="Chase"~"3",
    interaction=="Contact"~"4",
    interaction=="Fight"~"4"))
Interact$rating<-as.numeric(Interact$rating)

Interact$interaction<-factor(Interact$interaction,
                          levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact","Fight"))


#PARROTS ONLY----
Interact2<-Interact %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|
                                 initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  mutate(SP=case_when(initsp=="Monk parakeet"~"MP",
                     initsp=="Tanimbar corella"~"TC",
                     initsp=="Rose ringed parakeet"~"RRP",
                     initsp=="Red-breasted parakeet"~"RBP",
                     initsp=="Long-tailed parakeet"~"LTP"))


# ...all initators----
initiators<-Interact %>% 
  filter(recipsp!="NA") %>% 
  group_by(initsp,interaction,rating,isout) %>% 
  tally()
initiators<-rename(initiators,species=initsp)  
initiators<-rename(initiators,outcome=isout)
initiators$role<-'IS' # identify IS/RS
# ...all recipients----
recipients<-Interact %>% 
  filter(recipsp!="NA") %>% 
  group_by(recipsp,interaction,rating,rsout) %>% 
  tally()
recipients<-rename(recipients,species=recipsp)  
recipients<-rename(recipients,outcome=rsout)
recipients$role<-'RS' # identify IS/RS
#...combine----
isrs<-rbind(initiators,recipients)

#...no filter
isrsall<-isrs %>% 
  group_by(species,role,interaction,rating,outcome) %>%
  summarise(total=sum(n))

isrsall$interaction<-factor(isrsall$interaction,
                          levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact","Fight"))
isrsall$outcome<-factor(isrsall$outcome,
                      levels = c("W","NE","L"))

#... parrot filter
isrs2<-isrs %>% 
  filter(species=="Monk parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|species=="Red-breasted parakeet"|species=="Long-tailed parakeet") %>%  
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
  scale_fill_manual(values=c('Red-breasted parakeet'='red','Monk parakeet'='#3ACF3A','Rose ringed parakeet'='purple',
                              'Tanimbar corella'='orange','Long-tailed parakeet'='#1DACE8',"Others"="dark grey"))
 
##Parrot all interactions----
isrs2 %>% 
  group_by(species) %>% 
  summarise(n=sum(total)) %>% 
  ggplot(aes(reorder(species,-n),n))+
  geom_col()+
  geom_text(aes(label = n), vjust = -0.5)+
  scale_x_discrete(labels = function(species2) str_wrap(species2, width = 10))+
  labs(x='Species',y='n',title='Total interactions')

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
  scale_fill_manual(values=c('IS'='#456355','RS'='#FCD16B'))

# % IS RS----
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
  scale_fill_manual(values=c('W'='#00BFC4','NE'='#C4CFD0','L'='#F8766D'))
#### ..all species----
isrs %>%  
  group_by(species,outcome) %>% 
  summarise(n=sum(n)) %>% 
  ggplot(aes(reorder(species,n),n,fill=outcome))+
  geom_col(position = 'stack')+coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x='Species',y='n',title='n wins, losses and neutral outcomes')+
  scale_fill_manual(values=c('W'='#00BFC4','NE'='#C4CFD0','L'='#F8766D'))

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
  scale_fill_manual(values=c('W'='#00BFC4','NE'='#C4CFD0','L'='#F8766D'))

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
  scale_fill_manual(values=c('W'='#00BFC4','NE'='#C4CFD0','L'='#F8766D'))+
  facet_wrap(~role)

#3. INTERACTIONS----
## IS split interaction types----
### absolute freq (n)----
isrs2 %>% 
  filter(interaction!='Neutral'& role=='IS') %>% 
  ggplot(aes(interaction,total,fill=species))+geom_col(width=1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = 'none')+labs(y='absolute frequency',x='interaction',title='Interaction distribution: positively skewed')+
  facet_wrap(~species,scales='free')

### relative freq (%)----
isrs2 %>% 
  filter(interaction!='Neutral'& role=='IS') %>% group_by(species) %>% mutate(freq=total/sum(total)*100) %>% 
  ggplot(aes(interaction,freq,fill=species))+geom_col(width=1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,50)+
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

# PROXIMITY TO NEST

Interact$interaction<-factor(Interact$interaction,
                            levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact","Fight"))


Interact %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  ggplot(aes(nxt_cav,rating,color=interaction))+
  geom_jitter(size=2,alpha=0.5,width=1.5)+xlim(0,80)+
  scale_y_continuous(breaks = c(1,2,3,4))+
  labs(title = 'Interaction proximity with nearest nest',
    y='Aggression level',x='Distance from nest')+
  facet_wrap(~initsp)


# SUBSETS----
Interact %>% 
  filter(interaction!="Neutral") %>% 
  group_by(initsp,recipsp) %>% 
  tally() %>% 
  mutate(freq=n/sum(n)*100) %>%
  arrange(desc(initsp)) %>% 
  arrange(initsp)

# TABLES----

## true pop mean----
tmean<-Interact %>% filter(interaction!='Neutral') %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  select(interaction,rating) %>% 
  summarise('Aggression score'=round(mean(rating),2))
#1.97
tactions<-Interact %>% filter(interaction!='Neutral') %>%   
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  select(interaction,rating) %>% group_by(interaction) %>% 
  tally() %>% mutate(freq=round(n/sum(n)*100,2)) %>% select(-n) %>% 
  spread(key=interaction,value = freq) %>% replace(is.na(.), 0.001) %>% 
  add_column(Species='Population means', .before = 1)
tactions<-cbind(tactions,tmean[,1]) %>% relocate(1,8,2,3,4,5,6,7)
#view(tactions)

## AGGRESSION SCORE----
rating<-Interact %>% select(initsp,interaction,rating) %>% filter(interaction!='Neutral') %>%    
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  group_by(initsp) %>% summarise('Aggression score'=round(mean(rating),2)) %>% 
  arrange(match(initsp,c("Rose ringed parakeet","Tanimbar corella","Red-breasted parakeet","Monk parakeet","Long-tailed parakeet"))) %>%
  rename(Species=initsp) #%>% column_to_rownames(var="Species")
#view(rating)
### Actions----
actions<-Interact %>% select(initsp,interaction,rating) %>% filter(interaction!='Neutral') %>%   
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  group_by(initsp,interaction) %>% 
  tally() %>% mutate(freq=round(n/sum(n)*100,2)) %>% select(-n) %>% 
  spread(key=interaction,value = freq) %>% replace(is.na(.), 0) %>% 
  arrange(match(initsp,c("Rose ringed parakeet","Tanimbar corella","Red-breasted parakeet",
                             "Monk parakeet","Long-tailed parakeet"))) %>% rename(Species=initsp)
actions<-cbind(actions,rating[,2]) %>% relocate(1,8,2,3,4,5,6,7)
actions<-rbind(actions,tactions[1,])
#view(actions)
actions$Species<-actions$Species %>%
  factor(levels=c("Population means","Rose ringed parakeet","Tanimbar corella","Red-breasted parakeet",
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
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  group_by(initsp) %>% summarise('WIN Aggression score'=round(mean(rating),2)) %>% 
  arrange(match(initsp,c("Rose ringed parakeet","Tanimbar corella","Red-breasted parakeet","Monk parakeet","Long-tailed parakeet"))) %>%
  rename(Species=initsp) #%>% column_to_rownames(var="Species")
view(ratingW)

actionsW<-Interact %>% select(initsp,interaction,isout,rating) %>% filter(interaction!='Neutral'&isout=='W') %>%   
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  group_by(initsp,interaction) %>% 
  tally() %>% mutate(freq=round(n/sum(n)*100,2)) %>% select(-n) %>% 
  spread(key=interaction,value = freq) %>% replace(is.na(.), 0) %>% 
  arrange(match(initsp,c("Rose ringed parakeet","Tanimbar corella","Red-breasted parakeet",
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
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>% 
  group_by(Study.Area,initsp) %>% summarise('Aggression score'=round(mean(rating),2)) %>% 
  spread(key=Study.Area,value = 'Aggression score') %>% 
  arrange(match(initsp,c("Rose ringed parakeet","Tanimbar corella","Red-breasted parakeet",
                         "Monk parakeet","Long-tailed parakeet"))) %>% 
  rename(Species=initsp)
rating2<-cbind(rating2,rating[,2,drop=FALSE]) %>% relocate(1,5,2,3,4)
view(rating2)

formattable(rating2,
            align=c('r','c','c','c','c'),
            list(`Species` = formatter("span", style = ~ style(font.weight = "bold"))))


# linear model ----
# following https://stats.stackexchange.com/questions/189396/final-test-for-statistical-significance-of-bird-aggression-scores

# compre means----
#check data is normally distributed with a Shapiro-Wilk test
#density
shapiro.test(rating$`Aggression score`)

rating %>% 
  ggplot(aes(sample=`Aggression score`))+
  stat_qq()

rating %>% 
  ggplot(aes(`Aggression score`))+
  geom_density()

isrs<-rbind(initiators,recipients)

#...winrate per IS for each action
modeldata<-isrs2 %>% 
  filter(outcome!='NE'&role=='IS') %>%
  group_by(species) %>% 
  mutate(winrate=total/sum(total)) %>% 
  group_by(species,interaction) %>% 
  mutate(int.winrate=total/sum(total)*100) %>% 
  group_by(species,interaction,outcome) %>% 
  mutate(agg=sum(winrate*rating))

ggplot(modeldata,aes(interaction,total))+
  geom_point()+stat_smooth(method='lm')+facet_wrap(~species)

mean(rating$`Aggression score`)
# 1.976

t.test(2.16,1.77,var.equal=TRUE)

isrs2.lm %>% 
  ggplot(aes(species,rating))+
  geom_boxplot()+
  geom_jitter(aes(color=interaction),width=0.1,alpha=0.23)
