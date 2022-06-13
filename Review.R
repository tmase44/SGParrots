# Review script----

parrotmeans

## Distribution: Interaction x Species
ggplot(isrs2.lm,aes(species,rating))+
  geom_violin(color='grey',alpha=0)+
  geom_jitter(aes(color=interaction),width=0.2,height=0.3,alpha=0.23)+
  geom_errorbar(data = parrotmeans,
                aes(ymin=rating-se,ymax=rating+se),width=0.2)+
  labs(x='Aggression rating',y='Interaction levels',title='Interaction distribution x Species')

## relative frequency: Interaction x Species
isrs2 %>% 
  filter(role=='IS') %>% group_by(species) %>% mutate(freq=total/sum(total)*100) %>% 
  ggplot(aes(interaction,freq,fill=species))+geom_col(width=1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylim(0,50)+
  theme(legend.position = 'none')+labs(y='relative frequency',x='interaction',title='Interaction distribution: positively skewed')+
  facet_wrap(~species)

# Table: Aggression scoring and proportional interactions
# ...formattable----
formattable(actions,
            align=c('r','c','c','c','c','c','c','c','c'),
            list(`Species` = formatter("span", style = ~ style(font.weight = "bold")),
                 'Neutral'=color_tile(customL,customH),'Displace'=color_tile(customL,customH),'Threat'=color_tile(customL,customH),'Swoop'=color_tile(customL,customH),'Chase'=color_tile(customL,customH),'Contact'=color_tile(customL,customH),'Fight'=color_tile(customL,customH),'Aggression score'=color_tile(customL,customH))) 

# CLM for aggression ----
## Null model----
modelnull<-clm(as.factor(init.lm$interaction)~1,data=init.lm,link='loglog')
## Actual model----
model1<-clm(as.factor(init.lm$interaction)~species, data=init.lm,link='loglog')
##anova
anova(modelnull,model1)
# p >0.5 = signif * & likelihood ratio = 11.987, signifcant (>10)
nagelkerke(fit = model1, null = modelnull)
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
summary(modelt)

# CLM for win / loss likelihood----
## Null model----
modelnull<-clm(as.factor(init.lm$outcome)~1,data=init.lm,link='logit')

## Actual model----
model1<-clm(as.factor(init.lm$outcome)~species,data=init.lm,link='logit')
#anova
anova(modelnull,model1)
# p = ***
# likelihood ratio = 25.8
nagelkerke(fit = model1,null = modelnull)
# another check for model fit
## McFadden                            0.0353770
## Cox and Snell (ML)                  0.0630181
## Nagelkerke (Cragg and Uhler)        0.0749170
summary(model1)
# LTP is first referece category - summary shows, strong variance between:
## RRP-LTP **
## TC-LTP  ***
## RRP-LTP ***
## MP-LTP  ***
### non-native specieshave significantly higher win rate in interspecific interactions
confint(model1)#confidence interval
exp(coef(model1))#odds ratios
exp(confint(model1))
modelt<-polr(as.factor(outcome)~species,
             data=init.lm,
             Hess = TRUE)
summary(modelt)
brant(modelt)
#H0: Parallel Regression Assumption holds - can trust regression results
summary(modelt)
