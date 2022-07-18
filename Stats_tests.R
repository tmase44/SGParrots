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

https://www.statology.org/dunns-test/

## kruskal wallace or non-parametric ANOVA is the way to go

#Kruskal-Wallis test is a nonparametric test, so the normality assumption
  # is not required. However, the independence assumption still holds.

kruskal.test(rating ~ initsp,
             data = Interact_2)

# The P-Value is .000074. The result is significant at p < .05.

# post hoc test to decide which species is most different----
## DUNN TEST----
install.packages('FSA')
library(FSA)

dunnTest(rating ~ initsp,
         data = Interact_2,
         method = "bonferroni"
)

# pairwise wolcoxon ----
pairwise.wilcox.test(Interact_2$rating,Interact_2$initsp,
                     p.adjust.method = "holm")


#library(ggstatsplot)

ggbetweenstats(
  data = Interact_2,
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
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)

install.packages('dunn.test')
library(dunn.test)

capture.output(x<-as.data.frame(dunn.test(Interact_2$rating, Interact_2$initsp, 
                                          kw=TRUE, list=TRUE,altp = T)))



#########################old----
#=========================#
# i Richness x parrot prop
#=========================#

Indices_2 %>% 
  ggplot(aes(ints_HR,cav.sp.freq))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(aes(color=Study.Area))+
  labs(title = 'Parrot proportion in the community positively correlates with overall species Richness',
       y='Species richness',x='Proportion of parrots in the community')



#============================#
# ii. total obs x total ints.----
#============================#
#~~~~~~~~~~~~~~~~~~#
# All species 
#~~~~~~~~~~~~~~~~~~#
x1<-ISRS %>%
  group_by(Species) %>% summarise(max_obs=sum(max_obs),
                                  n_ints=sum(n_ints)) 
x1 %>% 
  ggplot(aes(n_ints,max_obs))+
  geom_point()+
  stat_cor(method = 'spearman')+
  stat_poly_line()+
  geom_text_repel(data=subset(x1,n_ints>30),(aes(label=Species)))+
  labs(x = 'Number of interactions', y = ' Numer of individuals',
       title = 'All species')
x<-lm(n_ints~all_obs,data=Composition_3)
summary(x)
# Most species in the community do not fall (often) into the interaction
# network of non native parrots
# mostly: other parrots and highly abundant birds
# Javan myna population of Changi is excluded from this

x1<-Composition_3 %>% 
  filter(Study.Area!='Changi Airport'|Species!='Javan myna') %>% 
  group_by(Species) %>% summarise(all_obs=sum(all_obs),
                                  inits=sum(inits))
x1 %>% 
  ggplot(aes(inits,all_obs))+
  geom_point()+
  stat_smooth(method = 'lm')+
  stat_poly_eq()+
  geom_text_repel(data=subset(x1,inits>30),(aes(label=Species)))+
  labs(x = 'Number of interactions', y = ' Numer of individuals',
       title = 'All species')
x<-lm(inits~all_obs,data=Composition_3)
summary(x)
# initiations with parrots, by other species are even fewer


sp.pairs_2 %>% 
  filter(Study.Area!='Changi Airport') %>% 
  group_by(Study.Area) %>% mutate(n=sum(Agg_pair_ints)) %>% 
  ggplot(aes(cav.sp.freq,n))+
  geom_point()+ 
  stat_smooth(method = 'lm')+
  stat_poly_eq()

#~~~~~~~~~~~~~~~~~~#
# Non-Focal species 
#~~~~~~~~~~~~~~~~~~#
# Parrots removed as these are the foccal species and biased
x2<-Composition_3 %>% 
  filter(Species!="Monk parakeet"&Species!='Tanimbar corella'&Species!='Rose-ringed parakeet'&Species!='Red-breasted parakeet')%>%  
  group_by(Species) %>% summarise(all_obs=sum(all_obs),
                                  n_ints=sum(n_ints)) %>% 
  filter(all_obs>0,n_ints>0)
x2 %>% 
  ggplot(aes(n_ints,all_obs))+
  geom_point()+ 
  stat_smooth(method = 'lm')+
  stat_poly_eq()+
  geom_text_repel(data=subset(x2,n_ints>20),(aes(label=Species)))+
  labs(x = 'Number of interactions', y = ' Numer of individuals',
       title = 'All species')
# looking at non-focal species only
# only a handful of species directly initate interactions with parrots
# mostly larger cavity competitors / predators

#//////// Outliers//////// 
# OPH were rarely observed but highly successful in displacing
# larger birds when they came to forage or inspect cavities
# YCC, low abundance but aggressive and cooperating with Tanimbar Corellas
# Rock Doves, highly abundant but placid, little overlap for cavities
# House crows, not cavity users, but cavity foragers and roost competitors

#~~~~~~~~~~~~~~#
# Parrots only
#~~~~~~~~~~~~~~#
x3<-Composition_3 %>%
  filter(Species=="Monk parakeet"|Species=='Tanimbar corella'|
           Species=='Rose-ringed parakeet'|Species=='Red-breasted parakeet'|
           Species=='Long-tailed parakeet')%>%  
  group_by(Species) %>% summarise(all_obs=sum(all_obs),
                                  n_ints=sum(n_ints),
                                  n_ints_xNE=sum(n_ints_xNE)) %>% 
  mutate(ints_freq=n_ints/all_obs) %>% 
  mutate(intsxNE_freq=n_ints_xNE/all_obs)

x3 %>% 
  ggplot(aes(all_obs,n_ints_xNE))+
  geom_point()+ 
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_text_repel(data=subset(x3,all_obs>10),(aes(label=Species)))+
  labs(x = 'Number of individuals', y = ' Numer of interactions',
       title = 'Focal species')


#==============================#
# iii initiated interactions----
#==============================#

# parrot prop in community != more interactions
Indices_2 %>% 
  ggplot(aes(freq.nnparr,inits))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point()+
  labs(title = 'Greater proportion of parrots does not predict greater interaction frequency',
       y='n interactions',x='Proportion of parrots in the community')
# R2 = 0.05
## only 6% variation of interaction frequency is explained by parrot abundance
y<-lm(allIS~parrotprop,Indices_2)
#summary(y)

Indices_2 %>% 
  ggplot(aes(Richness.max,inits))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(size=4)+
  labs(title = 'Richness of site is also not a driver of interaction frequency',
       y='n interactions',x='Species richness')
# R2 = <0.1%
# plot
grid.arrange(isxprop,isxrich,ncol=2)

Composition_3 %>% 
  ggplot(aes(Study.Area,ints_HR))+
  geom_col()

#===========================#
# v Cavity nester proportion----
#===========================#
# THIS----
Indices_2 %>% filter(Study.Area!='Sengkang Riverside Park') %>% 
  ggplot(aes(cav.sp.freq,ints_HR))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(aes(color=Study.Area))
# greater aggression levels where the number of obligate cavity nesters is high

Indices_2 %>% filter(Study.Area!='Sengkang Riverside Park') %>% 
  ggplot(aes(cav.sp.freq,inits_xNE))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(aes(color=Study.Area))+
  labs(title = 'Cavity nester proportion = higher aggression')

# how about aggression level


#===============================#
# Species level correlations----
#===============================#

# Abundance
Composition_3 %>% group_by(Study.Area) %>% 
  filter(Study.Area!='Changi Airport R1'&
           Study.Area!='Changi Airport R3') %>%
  ggplot(aes(max_obs,n_ints))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(aes(color=Species,alpha=0.8))+
  labs(x='Abundance',y='total interaction involvement',
       title = 'Abundance / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))

# Proportion of community
Composition_3 %>% filter(Study.Area!='Changi Airport R1'&
                           Study.Area!='Changi Airport R3') %>% 
  ggplot(aes(max.freq,n_ints))+
  stat_poly_line(se=F)+
  stat_poly_eq()+
  geom_point(aes(color=Species,alpha=0.8))+
  labs(x='Proportion of species in the community',y='total interaction involvement',
       title = 'Proportion of community / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Blue rumped parrot'='red'))


#==========================#
# These are interesting----
#==========================#

# Proportion by site
Composition_3 %>% 
  ggplot(aes(max.freq,n_ints,color=Species))+
  geom_point(alpha=0.8)+
  labs(x='Proportion of species in the community',y='total interaction involvement',
       title = 'Proportion of community / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Sulphur crested cockatoo'='green','Blue rumped parrot'='red'))+
  facet_wrap(~Study.Area)

# Proportion by site / species facet
Composition_3 %>% filter(n_ints>5) %>% 
  ggplot(aes(max.freq,n_ints,color=Species))+
  geom_point(alpha=0.8)+
  labs(x='Proportion of species in the community',y='total interaction involvement',
       title = 'Proportion of community / Total times involved in interactions')+
  scale_color_manual(values=c('Red-breasted parakeet'='#CC3311','Monk parakeet'='#004488',
                              'Rose-ringed parakeet'='#EE3377', 'Tanimbar corella'='#33BBEE','Long-tailed parakeet'='#009988',
                              'Yellow crested cockatoo'='#DDAA33','Sulphur crested cockatoo'='green','Blue rumped parrot'='red'))+
  facet_wrap(~Species)



# Parrot targets----
all.targets %>% 
  filter(initsp=="Monk parakeet"|initsp=='Tanimbar corella'|
           initsp=='Rose-ringed parakeet'|initsp=='Red-breasted parakeet')%>% 
  group_by(initsp) %>% 
  summarise(n=n_distinct(recipsp)) %>% 
  ggplot(aes(reorder(initsp,-n),n))+
  geom_col()+
  labs(x='Species',y='n unique species interacted with')

# body size
parrot.targets %>% filter(RS.size<150) %>% 
  ggplot(aes(initsp,RS.size))+
  geom_boxplot()+
  geom_jitter(width = 0.08,alpha=.4)+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution over all interactions')

# removing NE (Only aggressive ints.)
parrot.targets %>% filter(RS.size<150) %>% filter(n_NE==0) %>% 
  ggplot(aes(initsp,RS.size))+
  geom_boxplot()+
  geom_jitter(width = 0.08,alpha=.4)+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution of aggression recipients')

# removing NE & mean highlight
parrot.targets %>% #
  filter(RS.size<150) %>% 
  filter(n_NE==0) %>% 
  ggplot(aes(initsp,RS.size))+
  geom_boxplot(outlier.shape = 1)+
  stat_summary(fun='mean',color='red',shape=15)+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution of aggression recipients')

# All targets----
all.targets %>% filter(RS.size<200) %>% 
  ggplot(aes(IS.size,RS.size))+
  geom_jitter(width=1.5,height = 1.5,alpha=.4)


# boxplots with a few species
all.targets %>% filter(RS.size<150) %>% filter(n_NE==0) %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"| initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet"|initsp=="Javan myna"|initsp=="Oriental pied hornbill"|initsp=="Yellow crested cockatoo") %>% 
  ggplot(aes(initsp,RS.size))+
  geom_boxplot(outlier.shape = 1)+
  stat_summary(fun='mean',color='red',shape=15)+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution of aggression recipients')

# interaction type / RS.size ~ species
all.targets %>% filter(RS.size<150) %>% filter(n_NE==0) %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"| initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet") %>% 
  ggplot(aes(interaction,RS.size))+
  geom_boxplot(outlier.shape = 1)+
  stat_summary(fun='mean',color='red',shape=15)+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution of aggression recipients')+
  facet_wrap(~initsp)

# study.area / RS.size ~ species
all.targets %>% filter(RS.size<150) %>% filter(n_NE==0) %>% 
  filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"| initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet") %>% 
  group_by(Study.Area,initsp) %>% 
  mutate(n=n_Agg/sum(n_Agg)) %>% 
  filter(n<.75) %>% 
  ggplot(aes(interaction,n))+
  geom_col()+
  labs(x='Species',y='Body length (cm)',
       title = 'Body size distribution of aggression recipients')+
  facet_grid(Study.Area~initsp)



#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
#======================= 10.2 TOP-LINE REGRESSIONS ===========================
#/////////////////////////////////////////////////////////////////////////////#
#/////////////////////////////////////////////////////////////////////////////#
## andrews
# Generalized additive models were used because of the humpshaped
# relationship between the total number of interactions and the
# difference in body size (Zuur et al., 2011). 
## zuur p
# ## The data exploration did not show any clear linear patterns between roadkills and
# the explanatory variables; so we need to move on to using a GAM. Furthermore,
# an initial GLM with a Poisson distribution and logarithmic link function gave an
# overdispersion of 5, and we therefore proceed with a GAM with a negative bino-
#   mial distribution and logarithmic link function
ISRS %>% 
  ggplot(aes(Avg_size,n_ints))+
  geom_smooth()
Composition_3 %>% 
  ggplot(aes(Avg_size,inits))+
  geom_smooth()
Composition_3 %>% 
  ggplot(aes(Avg_size,inits_xNE))+
  geom_smooth()

# For all analysis of interspecies
# interactions, we summed interactions across all surveys for each
# site and included site and the species pair as categorical fixed effects.
# We examined the relationship between traits and interactions for two groups: 
# (a) all observed birds and 
# (b) cavity-breeding species. 
# Response variables included ‘total interactions’ (summed interactions between 
# species pairs), and ‘outcome’ which was the total number of wins for each species
# (e.g. the number of times rainbow lorikeet won vs. the common myna). 

x<-gam(n_ints~RS.NestType,data=all.targets)
summary(x)

x<-gam(n_ints~cav.sp.freq+freq.nnparr,data=Indices_2)
summary(x)

# For all models, explanatory variables included ‘difference in
# mean body size’, ‘site’, ‘species ID’. We used absolute body size difference
# as an explanatory variables for the models for total interactions
# while relative body size difference was used in the models of outcome.

ISRS %>% 
  ggplot(aes(Avg_size,n_ints))+
  geom_smooth()

#http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#spearman-rank-correlation-coefficient
# Kendall tau and Spearman rho, which are rank-based correlation coefficients (non-parametric)

x<-ggscatter(Composition_4,'n_ints','max.freq',
             add = 'reg.line',
             add.params = list(color = "blue", fill = "lightgray"),
             conf.int = TRUE)
x+stat_cor(method = 'spearman', label.x = 3, label.y = 30)


x<-cor.test(Composition_4$n_ints,
            Composition_4$max.freq,
            method='spearman',
            exact = F)
x
# spearmans rho shows a positive association between
# n interactions and population density in a community

