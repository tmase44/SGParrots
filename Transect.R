library(pacman)
p_load(tidyverse,vegan,lubridate,gridExtra,circlize,stringr,readxl,Distance,writexl,ggrepel)

# IMPORT DATA----
Transect <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                          sheet = "Composition")
Transect<-Transect %>% filter(Study.Area!='Palawan Beach')
# based on df prepared in Composition
view(Transect)
ls(Transect)
unique(Transect$Species)


# Hi-Lo species by site----
Composition_Rank<-Transect %>% group_by(Study.Area,Species,Surveyno) %>% summarise(n=n()) %>% select(-Surveyno) %>% summarise(max_obs = max(n)) %>% 
  arrange(Study.Area,desc(max_obs))
view(Composition_Rank)
getwd()
write_xlsx(Composition_Rank,'C:/Users/tmaso/OneDrive/MSc Environmental Management/Dissertation/R-Analysis/SGParrots/Composition_Rank.xlsx')


# example transect----
changi_rbp<-Transect %>% filter(Study.Area=='Changi Village') %>% filter(Species=='Red-breasted parakeet') %>% 
  select(Region.Label,Study.Area,Area,Sample.Label,Effort,Species,distance)
# effort multiplier ---- 
  # because each transect was walked 8 times in total
changi_rbp$Effort <- changi_rbp$Effort * 8

# https://examples.distancesampling.org/Distance-lines/lines-distill.html

# check total encounters----
sum(!is.na(changi_rbp$distance)) # 193 observations = good!
ls(changi_rbp$Species)

# RBP----

# fun.conversion factor----
conversion.factor <- convert_units("meter", "kilometer", "hectare")
# where:
  # distance of measure = meter
  # transect length = kilometer
  # area = hectare

## simple detection function with half normal detection----
rbp.hn <- ds(data=changi_rbp, key="hn", adjustment=NULL,
              convert_units=conversion.factor)
summary(rbp.hn)

cutpoints <- c(0,5,10,15,20,30,40,50,65)

plot(rbp.hn,
     breaks=cutpoints,
     main="Halfnormal model: RBP transects")

# most detections took place between 20-40m from the observer
  # logical because high visibility and vocal nauture of RBP
    # detection below 15m almost certain except of birds are quiet or hidden

## uniform detection function----
rbp.unif.cos <- ds(changi_rbp, key="unif", adjustment="cos",
                    convert_units=conversion.factor)

## hazard rate detection function----
rbpn.hr.poly <- ds(changi_rbp, key="hr", adjustment="poly", 
                   convert_units=conversion.factor)
## model comparison----
AIC(rbp.hn,rbpn.hr.poly,rbp.unif.cos)
# AIC = Aike information criterion
  # LOWEST AIC = BEST FIT ----
    # in this case: hazard rate
## goodness of fit----
gof_ds(rbpn.hr.poly,
       main="Goodness of fit: RBP Hazard rate detection model")
# Goodness of fit results for ddf object
# Distance sampling Cramer-von Mises test (unweighted)
# Test statistic = 0.328923 p-value = 0.112286
# good fit 
knitr::kable(summarize_ds_models(rbp.hn,rbp.unif.cos,rbpn.hr.poly),digits=3,
             caption="Model comparison table.")
## compare plots----
par(mfrow=c(1,3))
plot(rbpn.hr.poly, breaks=cutpoints, main="Hazard rate")
plot(rbp.unif.cos, breaks=cutpoints, main="Uniform cosine")
plot(rbp.hn, breaks=cutpoints, main="Halfnormal")
# this shows that UNIFORM COSINE is actually a better fit
  # HR suggests implausibly high detection rate to 35m with excessively shap drop off
summary(rbp.unif.cos)
summary(rbpn.hr.poly)
#RBP abundance = 25.47 % relative representation in the ecosystem  
#RBP density = 3.71 birds per sqKM



# REPEAT----
##efforts by site:
Transect %>% group_by(Study.Area) %>% summarise(max(Surveyno))
###Changi Village                         10
###2 Palawan Beach                         1
###3 Pasir Ris Town Park                  12
###4 Sengkang Riverside Park               7
###5 Springleaf                           10
###6 Stirling Road                         8

tsect<-Transect %>% 
  filter(Study.Area=='Springleaf') %>%  
  filter(Species=="Long-tailed parakeet") %>% 
  select(Region.Label,Study.Area,Area,Sample.Label,Effort,Species,distance)
tsect$Effort <- tsect$Effort * 10
sum(!is.na(tsect$distance)) 
###Half normal----
x.hn <- ds(data=tsect, key="hn", adjustment=NULL,convert_units=conversion.factor)
##Uniform cosine
x.unif.cos <- ds(tsect, key="unif", adjustment="cos",convert_units=conversion.factor)
##Hazard rate poly
x.hr.poly <- ds(tsect, key="hr", adjustment="poly",convert_units=conversion.factor)
##compare
AIC(x.hn,x.hr.poly,x.unif.cos)
##plot
par(mfrow=c(1,3))
plot(x.hr.poly, breaks=cutpoints, main="Hazard rate")
plot(x.unif.cos, breaks=cutpoints, main="Uniform cosine")
plot(x.hn, breaks=cutpoints, main="Halfnormal")
summary(x.hr.poly)
summary(x.unif.cos)
summary(x.hn)

# Abundance & Density----

#?????????????????????#
#----MULTI SPECIES-----
#?????????????????????#
###Changi Village                         10
###2 Palawan Beach                         1
###3 Pasir Ris Town Park                  12
###4 Sengkang Riverside Park               7
###5 Springleaf                           10
###6 Stirling Road                         8
#https://workshops.distancesampling.org/duke-spatial-2015/practicals/1-detection-functions-solutions.html

## Changi----
Changi<-Transect %>% filter(Study.Area=='Changi Village')
Changi$Effort<-Changi$Effort*10
Changi<-Changi %>% select(Region.Label,Area,Sample.Label,Effort,distance,Species,Surveyno) %>% 
  rename(species=Species) %>% rename(visit=Surveyno)
convunit <- convert_units("meter", "kilometer", "hectare")
Changi.birds <- ds(data = Changi,
                   key="hn", convert_units = convunit,
                   formula=~species, truncation = 70)
gof_ds(Changi.birds)
Changi.ests <- dht2(ddf=Changi.birds, flatfile=Changi,
                    strat_formula = ~species, convert_units = convunit,
                    stratification = "object") 
view(Changi.ests)
changiDAB<-Changi.ests %>% 
  select(Area,df_var,species,n,p_var,p_average,ER,Abundance,Abundance_se,Abundance_CV,bigC,LCI,UCI) %>% 
  arrange(desc(Abundance)) %>% 
  mutate(Study.Area='Changi Village',.before=1)
# generate density results https://rdrr.io/cran/Distance/src/R/dht2.R
changiDAB<-changiDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)
view(changiDAB)

## Pasir Ris----
Pasir<-Transect %>% filter(Study.Area=='Pasir Ris Town Park')
Pasir$Effort<-Pasir$Effort*12
Pasir<-Pasir %>% select(Region.Label,Area,Sample.Label,Effort,distance,Species,Surveyno) %>% 
  rename(species=Species) %>% rename(visit=Surveyno)
convunit <- convert_units("meter", "kilometer", "hectare")
Pasir.birds <- ds(data = Pasir,
                  key="hn", convert_units = convunit,
                  formula=~species, truncation = 70)
gof_ds(Pasir.birds)
Pasir.ests <- dht2(ddf=Pasir.birds, flatfile=Pasir,
                   strat_formula = ~species, convert_units = convunit,
                   stratification = "object") 
view(Pasir.ests)
PasirDAB<-Pasir.ests %>% 
  select(Area,df_var,species,n,p_var,p_average,ER,Abundance,Abundance_se,Abundance_CV,bigC,LCI,UCI) %>% 
  arrange(desc(Abundance)) %>% 
  mutate(Study.Area='Pasir Ris Town Park',.before=1)
# generate density results https://rdrr.io/cran/Distance/src/R/dht2.R
PasirDAB<-PasirDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)
view(PasirDAB)

## Springleaf----
Springleaf<-Transect %>% filter(Study.Area=='Springleaf')
Springleaf$Effort<-Springleaf$Effort*10
Springleaf<-Springleaf %>% select(Region.Label,Area,Sample.Label,Effort,distance,Species,Surveyno) %>% 
  rename(species=Species) %>% rename(visit=Surveyno)
convunit <- convert_units("meter", "kilometer", "hectare")
Springleaf.birds <- ds(data = Springleaf,
                       key="hn", convert_units = convunit,
                       formula=~species, truncation = 70)
gof_ds(Springleaf.birds,chisq = T)
Springleaf.ests <- dht2(ddf=Springleaf.birds, flatfile=Springleaf,
                        strat_formula = ~species, convert_units = convunit,
                        stratification = "object") 
view(Springleaf.ests)
SpringleafDAB<-Springleaf.ests %>% 
  select(Area,df_var,species,n,p_var,p_average,ER,Abundance,Abundance_se,Abundance_CV,bigC,LCI,UCI) %>% 
  arrange(desc(Abundance)) %>% 
  mutate(Study.Area='Springleaf',.before=1)
# generate density results https://rdrr.io/cran/Distance/src/R/dht2.R
SpringleafDAB<-SpringleafDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)
view(SpringleafDAB)

## Sengkang----
Sengkang<-Transect %>% filter(Study.Area=='Sengkang Riverside Park')
Sengkang$Effort<-Sengkang$Effort*7
Sengkang<-Sengkang %>% select(Region.Label,Area,Sample.Label,Effort,distance,Species,Surveyno) %>% 
  rename(species=Species) %>% rename(visit=Surveyno)
convunit <- convert_units("meter", "kilometer", "hectare")
Sengkang.birds <- ds(data = Sengkang,
                     key="hn", convert_units = convunit,
                     formula=~species, truncation = 70)
gof_ds(Sengkang.birds)
Sengkang.ests <- dht2(ddf=Sengkang.birds, flatfile=Sengkang,
                      strat_formula = ~species, convert_units = convunit,
                      stratification = "object") 
view(Sengkang.ests)
SengkangDAB<-Sengkang.ests %>% 
  select(Area,df_var,species,n,p_var,p_average,ER,Abundance,Abundance_se,Abundance_CV,bigC,LCI,UCI) %>% 
  arrange(desc(Abundance)) %>% 
  mutate(Study.Area='Sengkang Riverside Park',.before=1)
# generate density results https://rdrr.io/cran/Distance/src/R/dht2.R
SengkangDAB<-SengkangDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)
view(SengkangDAB)

## Stirling/Queenstown----
Stirling<-Transect %>% filter(Study.Area=='Stirling Road')
Stirling$Effort<-Stirling$Effort*8
Stirling<-Stirling %>% select(Region.Label,Area,Sample.Label,Effort,distance,Species,Surveyno) %>% 
  rename(species=Species) %>% rename(visit=Surveyno)
convunit <- convert_units("meter", "kilometer", "hectare")
Stirling.birds <- ds(data = Stirling,
                     key="hn", convert_units = convunit,
                     formula=~species, truncation = 70)
gof_ds(Stirling.birds)
Stirling.ests <- dht2(ddf=Stirling.birds, flatfile=Stirling,
                      strat_formula = ~species, convert_units = convunit,
                      stratification = "object") 
view(Stirling.ests)
StirlingDAB<-Stirling.ests %>% 
  select(Area,df_var,species,n,p_var,p_average,ER,Abundance,Abundance_se,Abundance_CV,bigC,LCI,UCI) %>% 
  arrange(desc(Abundance)) %>% 
  mutate(Study.Area='Stirling Road',.before=1)
# generate density results https://rdrr.io/cran/Distance/src/R/dht2.R
StirlingDAB<-StirlingDAB %>% 
  mutate(Density = Abundance/Area,
         df_var2 = df_var/Area^2) %>%
  mutate(Density_se = sqrt(Abundance_se^2/Area^2)) %>%
  mutate(Density_CV = Density_se/Density)
view(StirlingDAB)

### MERGE ALL
DAB_master <- rbind(changiDAB,PasirDAB,SpringleafDAB,StirlingDAB,SengkangDAB)
DAB_master<-DAB_master %>% filter(species!='Total') %>% rename(Species=species)
view(DAB_master)


## Fixed scale RAD----
RAD %>% 
  ggplot(aes(RA,D))+
  geom_jitter(aes(color=Study.Area),width=2,height=.5,size=3,alpha=.7,shape=20)+
  facet_wrap(~Study.Area)+
  theme(legend.position = 'none')+
  labs(x='Relative abundance',y='Density',title='Correlation between distance-based density estimates and relative abundances')#+
  scale_color_manual(values=c('Changi Village'='#CC3311','Pasir Ris Sports Center'='#004488','Q'='#EE3377','Springleaf'='#33BBEE'))

## Free scale RAD----
RAD %>% ggplot(aes(RA,D))+
  geom_jitter(aes(color=Study.Area),width=2,height=.5,size=3,alpha=.7,shape=20)+
  facet_wrap(~Study.Area,scales = 'free')+
  theme(legend.position = 'none')+
  labs(x='Relative abundance',y='Density',title='Correlation between distance-based density estimates and relative abundances')+
  scale_color_manual(values=c('Changi Village'='#CC3311','Pasir Ris Sports Center'='#004488','Queenstown Stadium'='#EE3377','Springleaf'='#33BBEE'))

# n initiations per site and species-----
nInts<-isrs %>% 
  group_by(Study.Area,species) %>% summarise(n=sum(n)) %>% arrange(Study.Area,desc(n)) %>% rename(Species=species)
view(nInts)

RAD2<-merge(RAD,nInts,by.x=c("Study.Area","Species"),by.y=c("Study.Area","species"),all.x=TRUE)
RAD2<-RAD2 %>% replace(is.na(.), 0)
RAD2 %>% ggplot(aes(RA,n))+
  geom_jitter(aes(color=Species),width=2,height=.5,size=2)+
  facet_wrap(~Study.Area,scales = 'free')+
  labs(x='Relative abundance',y='n interactions',title='Correlation between relative abundance and total interaction frequency')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311',
                              'Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377',
                              'Tanimbar corella'='#33BBEE',
                              'Long-tailed parakeet'='#009988'))

RAD2$CavityYN<-as.factor(RAD2$CavityYN)
cavs<-RAD2 %>% group_by(Study.Area,CavityYN) %>% count(CavityYN) %>% group_by(Study.Area) %>% 
  mutate(freq=n/sum(n)*100) %>% filter(CavityYN=='4')
  
## initations only-----
nInts2<-isrs %>% 
  filter(role=='IS') %>% 
  group_by(Study.Area,species) %>% summarise(n=sum(n)) %>% arrange(Study.Area,desc(n)) %>% rename(Species=species)
view(nInts)

RAD3<-merge(RAD,nInts2,by.x=c("Study.Area","Species"),by.y=c("Study.Area","species"),all.x=TRUE)
RAD3<-RAD3 %>% replace(is.na(.), 0) %>% filter(n>0)
RAD3 %>% ggplot(aes(n,RA))+
  geom_jitter(width=2,height=.5,size=2,shape=20)+
  stat_smooth(method='lm')+
  theme_bw()+
  labs(x='n initiated interactions',y='Relative abundance',title='Correlation between relative abundance and initiated interaction frequency')

