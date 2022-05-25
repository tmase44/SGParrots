library(tiyverse)
library(Distance)

# based on df prepared in Composition
# distance col must have lower case d----
changiv<-rename(changiv,distance = Distance)

# distance measurements by species
changiv %>% 
  ggplot(aes(distance))+
  geom_histogram(bins = 11,
                 binwidth = 5,#sets bins to same as 'by' count below
                 center = 0,#aligns label to middle of bin,
                 color="black",fill="white")+
  scale_x_continuous(breaks=seq(0,60,by=5))+
  labs(x="Distance (m)", y="Frequency",
       title = "Line transects")+
  facet_wrap(~Object,ncol =6)

ts_rbp<-changiv %>% 
  filter(Object == "Red-breasted parakeet") %>% 
  select(Region.Label,Study.Area,Area,Sample.Label,Effort,Object,distance)
view(ts_rbp)

# effort multiplier ---- 
  # because each transect was walked 8 times in total
ts_rbp$Effort <- ts_rbp$Effort * 8


# https://examples.distancesampling.org/Distance-lines/lines-distill.html

# check total encounters----
sum(!is.na(ts_rbp$distance)) # 154 observations = good!

# RBP----
# plot transects----
ts_rbp %>% 
  ggplot(aes(distance,))+
  geom_histogram(bins = 11,
                 binwidth = 5,#sets bins to same as 'by' count below
                 center = 0,#aligns label to middle of bin,
                 color="black",fill="white")+
  scale_x_continuous(breaks=seq(0,60,by=5))+
  labs(x="Distance (m)", y="Frequency",
       title = "RBP line transects")
# or
hist(ts_rbp$distance, xlab="Distance (m)",
     main="RBP line transects")#

# fun.conversion factor----
conversion.factor <- convert_units("meter", "kilometer", "hectare")
# where:
  # distance of measure = meter
  # transect length = kilometer
  # area = hectare

# simple detection function with half normal detection----
rbp.hn <- ds(data=ts_rbp, key="hn", adjustment=NULL,
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
rbp.unif.cos <- ds(ts_rbp, key="unif", adjustment="cos",
                    convert_units=conversion.factor)

# hazard rate detection function----
rbpn.hr.poly <- ds(ts_rbp, key="hr", adjustment="poly", 
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

knitr::kable(summarize_ds_models(rbp.hn,
                                 rbp.unif.cos,
                                 rbpn.hr.poly),digits=3,
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
#RBP abundance = 31.36 % relative representation in the ecosystem  
#RBP density = 3.71 birds per sqKM

# TANIMBAR CORELLA----
ts_tc<-changiv %>% 
  filter(Object == "Tanimbar corella") %>% 
  select(Region.Label,Study.Area,Area,Sample.Label,Effort,Object,distance)
ts_tc$Effort <- ts_tc$Effort * 8
view(ts_tc)

# check total encounters----
sum(!is.na(ts_tc$distance)) # 49 observations = good!

ts_tc %>% 
  ggplot(aes(distance,))+
  geom_histogram(bins = 11,
                 binwidth = 5,#sets bins to same as 'by' count below
                 center = 0,#aligns label to middle of bin,
                 color="black",fill="white")+
  scale_x_continuous(breaks=seq(0,60,by=5))+
  labs(x="Distance (m)", y="Frequency",
       title = "TC line transects")

ts_tc[42, 5] = 0.112

#Half normal----
tc.hn <- ds(data=ts_tc, key="hn", adjustment=NULL,
             convert_units=conversion.factor)
summary(rbp.hn)
#Uniform cosine
tc.unif.cos <- ds(ts_tc, key="unif", adjustment="cos",
                   convert_units=conversion.factor)
#Hazard rate poly
tc.hr.poly <- ds(ts_tc, key="hr", adjustment="poly", 
                   convert_units=conversion.factor)
#compare
AIC(tc.hn,tc.hr.poly,tc.unif.cos)
# all are not great
par(mfrow=c(1,3))
plot(tc.hr.poly, breaks=cutpoints, main="Hazard rate")
plot(tc.unif.cos, breaks=cutpoints, main="Uniform cosine")
plot(tc.hn, breaks=cutpoints, main="Halfnormal")
#NOT ENOUGH DATA FOR TC----

