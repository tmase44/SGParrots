library(pacman)
p_load(tidyverse,vegan,lubridate,gridExtra,circlize,stringr,readxl,Distance)

# IMPORT DATA----
Transect <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                          sheet = "Composition")
# based on df prepared in Composition
view(Transect)
ls(Transect)
unique(Transect$Species)

# Top10 species by site----
TTop10<-Transect %>% group_by(Study.Area,Species,Surveyno) %>% summarise(n=n()) %>% select(-Surveyno) %>% summarise(max_obs = max(n)) %>% 
  arrange(Study.Area,desc(max_obs)) %>% top_n(10)
view(TTop10)

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

# simple detection function with half normal detection----
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

# uniform detection function----
rbp.unif.cos <- ds(changi_rbp, key="unif", adjustment="cos",
                    convert_units=conversion.factor)

# hazard rate detection function----
rbpn.hr.poly <- ds(changi_rbp, key="hr", adjustment="poly", 
                   convert_units=conversion.factor)
# model comparison----
AIC(rbp.hn,rbpn.hr.poly,rbp.unif.cos)
# AIC = Aike information criterion
  # LOWEST AIC = BEST FIT ----
    # in this case: hazard rate

# goodness of fit----
gof_ds(rbpn.hr.poly,
       main="Goodness of fit: RBP Hazard rate detection model")
# Goodness of fit results for ddf object
# Distance sampling Cramer-von Mises test (unweighted)
# Test statistic = 0.328923 p-value = 0.112286
# good fit 

knitr::kable(summarize_ds_models(rbp.hn,rbp.unif.cos,rbpn.hr.poly),digits=3,
             caption="Model comparison table.")

# compare plots----
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

# TANIMBAR CORELLA----
changi_tc<-Transect %>% filter(Study.Area=='Changi Village') %>%  filter(Species=="Tanimbar corella") %>% 
  select(Region.Label,Study.Area,Area,Sample.Label,Effort,Species,distance)
changi_tc$Effort <- changi_tc$Effort * 8
view(changi_tc)

# check total encounters----
sum(!is.na(changi_tc$distance)) # 83 observations = good!

changi_tc %>% 
  ggplot(aes(distance,))+
  geom_histogram(bins = 11,
                 binwidth = 5,#sets bins to same as 'by' count below
                 center = 0,#aligns label to middle of bin,
                 color="black",fill="white")+
  scale_x_continuous(breaks=seq(0,60,by=5))+
  labs(x="Distance (m)", y="Frequency",
       title = "TC line transects")

#Half normal----
tc.hn <- ds(data=changi_tc, key="hn", adjustment=NULL,
             convert_units=conversion.factor)
#Uniform cosine
tc.unif.cos <- ds(changi_tc, key="unif", adjustment="cos",
                   convert_units=conversion.factor)
#Hazard rate poly
tc.hr.poly <- ds(changi_tc, key="hr", adjustment="poly", 
                   convert_units=conversion.factor)
#compare
AIC(tc.hn,tc.hr.poly,tc.unif.cos)
# all are not great
par(mfrow=c(1,3))
plot(tc.hr.poly, breaks=cutpoints, main="Hazard rate")
plot(tc.unif.cos, breaks=cutpoints, main="Uniform cosine")
plot(tc.hn, breaks=cutpoints, main="Halfnormal")
#Uniform cosine----
knitr::kable(summarize_ds_models(tc.hn,tc.unif.cos,tc.hr.poly),digits=3,
             caption="Model comparison table.")
summary(tc.unif.cos)
summary(tc.hn)

# REPEAT
changi_x<-Transect %>% filter(Study.Area=='Changi Village') %>%  filter(Species=="House crow") %>% 
  select(Region.Label,Study.Area,Area,Sample.Label,Effort,Species,distance)
#changi_tc$Effort <- changi_tc$Effort * 8
#Half normal----
x.hn <- ds(data=changi_x, key="hn", adjustment=NULL,
            convert_units=conversion.factor)
#Uniform cosine
x.unif.cos <- ds(changi_x, key="unif", adjustment="cos",
                  convert_units=conversion.factor)
#Hazard rate poly
x.hr.poly <- ds(changi_x, key="hr", adjustment="poly", 
                 convert_units=conversion.factor)
#compare
AIC(x.hn,x.hr.poly,x.unif.cos)
# all are not great
par(mfrow=c(1,3))
plot(x.hr.poly, breaks=cutpoints, main="Hazard rate")
plot(x.unif.cos, breaks=cutpoints, main="Uniform cosine")
plot(x.hn, breaks=cutpoints, main="Halfnormal")
summary(x.hr.poly)
summary(x.unif.cos)
summary(x.hn)
