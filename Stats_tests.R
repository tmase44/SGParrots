library(pacman)
p_load(kableExtra,Rmisc,rcompanion,brant,ordinal,MASS,DescTools,formattable,knitr,kableExtra,tidyverse,vegan,
       lubridate,gridExtra,circlize,stringr,readxl)

# Ordinal logistic regression----
#https://www.r-bloggers.com/2019/06/how-to-perform-ordinal-logistic-regression-in-r/
# explicit order to categories (rating)

# data prep----
# ...all initators----
model<-Interact_2 %>% filter(recipsp!="NA")%>% select(initsp,interaction,rating,isout)
model<-rename(init.lm,species=initsp)  
model<-rename(init.lm,outcome=isout)
model$role<-'IS' # identify IS/RS

model<-model %>% filter(species=="Monk parakeet"|species=="Tanimbar corella"|species=="Rose ringed parakeet"|
                        species=="Red-breasted parakeet"|species=="Long-tailed parakeet")
#edit
#init.lm<-init.lm %>% filter(species=="Rose ringed parakeet"|species=="Long-tailed parakeet")


init.lm$species<-init.lm$species %>% factor(levels=c("Long-tailed parakeet","Rose ringed parakeet","Tanimbar corella","Red-breasted parakeet",
                  "Monk parakeet"))
  

init.lm<-init.lm %>% 
  select(interaction,rating,species,outcome) %>% 
    mutate(status=factor(case_when(
    species=="Red-breasted parakeet"~"0",
    species=="Tanimbar corella"~"0",
    species=="Rose ringed parakeet"~"0",
    species=="Monk parakeet"~"0",
    species=="Long-tailed parakeet"~"1"))) 

init.lm$rating<-as.factor(init.lm$rating)
levels(init.lm$rating)
levels(init.lm$interaction)
levels(init.lm$outcome)
levels(init.lm$status)
levels(init.lm$species)
# status 0 = non native, 1 = native LTP
# WL 0 = loss, 1 = W

# CLM cumulative link model - for ordinal / categorical data----
## log-log link is used because interaction data is positively skewed
### https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf
#### https://www.youtube.com/watch?v=rrRrI9gElYA !!!!! WATCH

# Interaction type ~ species----
## Null model----
modelnull<-clm(as.factor(init.lm$interaction)~1,
               data=init.lm,
               link='loglog')

## Actual model----
model1<-clm(as.factor(init.lm$interaction)~species,
               data=init.lm,
               link='loglog')

anova(modelnull,model1)

# p >0.5 = signif *
# likelihood ratio = 11.987, signifcant (>10)
nagelkerke(fit = model1,
           null = modelnull)
# another check for model fit
##McFadden                           0.00924532
##Cox and Snell (ML)                 0.02974310
#Nagelkerke (Cragg and Uhler)       0.03092320
summary(model1)

# LTP is first referece category - summary shows, strong variance between:
## RRP-LTP ***
## TC-LTP  **
## RRP-LTP *
## MP-LTP  ''
### there is a signif diff between RRP and LTP when it comes to aggression
confint(model1)#confidence interval
exp(coef(model1))#odds ratios
exp(confint(model1))

modelt<-polr(as.factor(interaction)~species,
             data=init.lm,
             Hess = TRUE)
summary(modelt)
brant(modelt)
#H0: Parallel Regression Assumption holds - can trust regression results
summary(modelt)#no pvalues!

# MEANS / SD / SE / CI  ----
init.lm2<-init.lm
levels(init.lm2$rating)
init.lm2$rating<-init.lm2$rating %>% as.character(init.lm$rating)
init.lm2$rating<-init.lm2$rating %>% as.numeric(init.lm$rating)

parrotmeans <- summarySE(init.lm2, measurevar="rating", groupvars="species")
parrotmeans
formattable(parrotmeans)

# W/L ~ species----
## Null model----
modelnull<-clm(as.factor(init.lm$outcome)~1,
               data=init.lm,
               link='logit')

## Actual model----
model1<-clm(as.factor(init.lm$outcome)~species,
            data=init.lm,
            link='logit')

anova(modelnull,model1)
# p >0.5 = signif *
# likelihood ratio = 11.987, signifcant (>10)
nagelkerke(fit = model1,
           null = modelnull)
# another check for model fit
##McFadden                           0.00924532
##Cox and Snell (ML)                 0.02974310
#Nagelkerke (Cragg and Uhler)       0.03092320
summary(model1)
# LTP is first referece category - summary shows, strong variance between:
## RRP-LTP ***
## TC-LTP  **
## RRP-LTP *
## MP-LTP  ''
### there is a signif diff between RRP and LTP when it comes to aggression
confint(model1)#confidence interval
exp(coef(model1))#odds ratios
exp(confint(model1))

modelt<-polr(as.factor(outcome)~species,
             data=init.lm,
             Hess = TRUE)
summary(modelt)
brant(modelt)
#H0: Parallel Regression Assumption holds - can trust regression results
summary(modelt)#no pvalues!









install.packages('orddom')

library(remotes)
install_version("orddom", "3.1")

# data prep
x<-Interact_2 %>% select(initsp,rating)
x<-x %>% filter(initsp=='Tanimbar corella'|
                  initsp=='Red-breasted parakeet'|
                  initsp=='Long-tailed parakeet'|  
                  initsp=='Rose-ringed parakeet'|         
                  initsp=='Monk parakeet'|
                  initsp=='Oriental pied hornbill'|
                  initsp=='Javan myna'|
                  initsp=='House crow')


#// MANN-WHITNEY U TEST//----

# Summary of the data

# loading the package
group_by(x,initsp) %>%
  summarise(
    count = n(),
    median = median(rating, na.rm = TRUE),
    IQR = IQR(rating, na.rm = TRUE))

# loading package for boxplot
library(ggpubr)
ggboxplot(x, x = "initsp", y = "rating",
          ylab = "rating", xlab = "initsp")

res <- wilcox.test(rating~ initsp,
                   data = x,
                   exact = FALSE)
res

## not aceptable because this is for only 2 factor levels

## kruskal wallace or non-parametric ANOVA is the way to go

#Kruskal-Wallis test is a nonparametric test, so the normality assumption
  # is not required. However, the independence assumption still holds.

kruskal.test(rating ~ initsp,
             data = x)

# The P-Value is .000074. The result is significant at p < .05.

# post hoc test to decide which species is most different----
## DUNN TEST----
install.packages('FSA')
library(FSA)

dunnTest(rating ~ initsp,
         data = x,
         method = "bonferroni"
)

# pairwise wolcoxon ----
pairwise.wilcox.test(x$rating,x$initsp,
                     p.adjust.method = "holm")


#library(ggstatsplot)

ggbetweenstats(
  data = x,
  x = initsp,
  y = rating,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)



x<-Interact_2 %>% select(Study.Area,rating)
x<-x %>% filter(initsp=='Tanimbar corella'|
                  initsp=='Red-breasted parakeet'|
                  initsp=='Long-tailed parakeet'|  
                  initsp=='Rose-ringed parakeet'|         
                  initsp=='Monk parakeet'|
                  initsp=='Oriental pied hornbill'|
                  initsp=='Javan myna'|
                  initsp=='House crow')

kruskal.test(rating ~ Study.Area,
             data = x)


# post hoc test to decide which species is most different----
## DUNN TEST----
install.packages('FSA')
library(FSA)

dunnTest(rating ~ Study.Area,
         data = x,
         method = "bonferroni"
)

# pairwise wolcoxon ----
pairwise.wilcox.test(x$rating,x$Study.Area,
                     p.adjust.method = "holm")


#library(ggstatsplot)

ggbetweenstats(
  data = x,
  x = Study.Area,
  y = rating,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "violin",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)

install.packages('dunn.test')
library(dunn.test)

dunn.test(x$rating, x$Study.Area, kw=TRUE, list=TRUE)


