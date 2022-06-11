library(pacman)
p_load(MASS,DescTools,formattable,knitr,kableExtra,tidyverse,vegan,
       lubridate,gridExtra,circlize,stringr,readxl)

# STATISTICAL TESTS----

##data description: 
### rating / interaction = ordinal /categorical / likert data; scale 1-4
### nominal 1: native, non-native
### nominal 2: species

#example: Cochran-Armitage----
dose <- matrix(c(10,9,10,7, 0,1,0,3), 
               byrow=TRUE, nrow=2, 
               dimnames=list(resp=0:1, dose=0:3))
Desc(dose)
CochranArmitageTest(dose)

# non-parametric tests----
#?????

# Ordinal logistic regression----
#https://www.r-bloggers.com/2019/06/how-to-perform-ordinal-logistic-regression-in-r/
# explicit order to categories (rating)

# data prep----
# ...all initators----
init.lm<-Interact2 %>% filter(recipsp!="NA") %>% select(initsp,interaction,rating,isout)
init.lm<-rename(init.lm,species=initsp)  
init.lm<-rename(init.lm,outcome=isout)
init.lm$role<-'IS' # identify IS/RS
# ...all recipients----
recip.lm<-Interact2 %>% filter(recipsp!="NA") %>% select(recipsp,interaction,rating,rsout)
recip.lm<-rename(recip.lm,species=recipsp)  
recip.lm<-rename(recip.lm,outcome=rsout)
recip.lm$role<-'RS' # identify IS/RS
#...combine----
isrs2.lm<-rbind(init.lm,recip.lm)
isrs2.lm<-isrs2.lm %>% filter(species=="Monk parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|
                        species=="Red-breasted parakeet"|species=="Long-tailed parakeet")

isrs2.lm$species<-isrs2.lm$species %>% factor(levels=c("Rose ringed parakeet","Tanimbar corella","Red-breasted parakeet",
                  "Monk parakeet","Long-tailed parakeet"))
  

# THIS IS FOR ALL INTERACTION IS AND RS
isrs2.stat<-isrs2.lm %>% filter(interaction!="Neutral") %>% 
  filter(species=="Monk parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|
           species=="Red-breasted parakeet"|species=="Long-tailed parakeet") %>% 
    mutate(status=factor(case_when(
    species=="Red-breasted parakeet"~"0",
    species=="Tanimbar corella"~"0",
    species=="Rose ringed parakeet"~"0",
    species=="Monk parakeet"~"0",
    species=="Long-tailed parakeet"~"1"))) %>% 
  mutate(WL=factor(case_when(
    outcome=="W"~"1",
    outcome=="L"~"0"))) %>% select(interaction,rating,status,WL)

isrs2.stat$rating<-as.factor(isrs2.stat$rating)
view(isrs2.stat)
levels(isrs2.stat$rating)
levels(isrs2.stat$interaction)
levels(isrs2.stat$WL)
levels(isrs2.stat$status)
# status 0 = non native, 1 = native LTP
# WL 0 = loss, 1 = W

#summary stats----
summary(isrs2.stat)
#Making frequency table
table(isrs2.stat$rating, isrs2.stat$status)

#Dividing data into training and test set----
#Random sampling 
samplesize = 0.70*nrow(isrs2.stat)
set.seed(100)
index = sample(seq_len(nrow(isrs2.stat)), size = samplesize)
#Creating training and test set 
datatrain = isrs2.stat[index,]
datatest = isrs2.stat[-index,]

#Build ordinal logistic regression model----
# MASS from library
model= polr(rating ~ status, data = datatrain, Hess = TRUE)
summary(model)

#Compute confusion table and misclassification error----
predictagg = predict(model,datatest)
table(datatest$rating, predictagg)
mean(as.character(datatest$rating) != as.character(predictagg))

# poor----







