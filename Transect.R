library(pacman)
p_load(tidyverse,vegan,lubridate,gridExtra,circlize,stringr,readxl,Distance,writexl,ggrepel)

# IMPORT DATA----
Transect <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                          sheet = "Composition")
# based on df prepared in Composition
view(Transect)
ls(Transect)
unique(Transect$Species)

# Hi-Lo species by site----
TTop10<-Transect %>% group_by(Study.Area,Species,Surveyno) %>% summarise(n=n()) %>% select(-Surveyno) %>% summarise(max_obs = max(n)) %>% 
  arrange(Study.Area,desc(max_obs))
view(TTop10)
getwd()
write_xlsx(TTop10,'C:/Users/tmaso/OneDrive/MSc Environmental Management/Dissertation/R-Analysis/SGParrots/TTop10.xlsx')


# Changi transect
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
changi_x<-Transect %>% filter(Study.Area=='Pasir Ris Town Park') %>%  filter(Species=="Red-breasted parakeet") %>% 
  select(Region.Label,Study.Area,Area,Sample.Label,Effort,Species,distance)
changi_x$Effort <- changi_x$Effort * 8
sum(!is.na(changi_x$distance)) 
###Half normal----
x.hn <- ds(data=changi_x, key="hn", adjustment=NULL,
            convert_units=conversion.factor)
##Uniform cosine
x.unif.cos <- ds(changi_x, key="unif", adjustment="cos",
                  convert_units=conversion.factor)
##Hazard rate poly
x.hr.poly <- ds(changi_x, key="hr", adjustment="poly", 
                 convert_units=conversion.factor)
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
## IMPORT DATA----
RAD <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                          sheet = "DenAb")
ls(RAD)
#Top 
TTopAB<-RAD %>% arrange(Study.Area,desc(RA))
view(TTopAB)

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
  group_by(Study.Area,species) %>% summarise(n=sum(n)) %>% arrange(Study.Area,desc(n))
view(nInts)

RAD2<-merge(RAD,nInts,by.x=c("Study.Area","Species"),by.y=c("Study.Area","species"),all.x=TRUE)
RAD2<-RAD2 %>% replace(is.na(.), 0)
RAD2 %>% ggplot(aes(RA,n))+
  geom_jitter(aes(color=factor(CavityYN)),width=2,height=.5,size=2,shape=20)+
  stat_smooth(method='lm')+
  theme_bw()+
  labs(x='Relative abundance',y='n interactions',title='Correlation between relative abundance and total interaction frequency')+
  scale_color_manual(values=c('0'='#33BBEE',
                              '1'='#004488',
                              '2'='#EE3377'))

## initations only-----
nInts2<-isrs %>% 
  filter(role=='IS') %>% 
  group_by(Study.Area,species) %>% summarise(n=sum(n)) %>% arrange(Study.Area,desc(n))
view(nInts)

RAD3<-merge(RAD,nInts2,by.x=c("Study.Area","Species"),by.y=c("Study.Area","species"),all.x=TRUE)
RAD3<-RAD3 %>% replace(is.na(.), 0) %>% filter(n>0)
RAD3 %>% ggplot(aes(n,RA))+
  geom_jitter(width=2,height=.5,size=2,shape=20)+
  stat_smooth(method='lm')+
  theme_bw()+
  labs(x='n initiated interactions',y='Relative abundance',title='Correlation between relative abundance and initiated interaction frequency')

# REgression----
lmRAD3s<-lm(n~RA,data=RAD3)
summary(lmRAD3s)
# *** between n ints & RA
hist(lmRAD3s$resid)
qqnorm(lmRAD3s$resid)

lmRAD3m<-lm(n~RA+CavityYN,data=RAD3)
summary(lmRAD3m)
# *** between n ints & RA
# ** between n ints and cavity nesters


