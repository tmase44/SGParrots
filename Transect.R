library(tiyverse)
library(Distance)

# based on df prepared in Composition
# distance col must have lower case d----
changiv<-rename(changiv,distance = Distance)

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











