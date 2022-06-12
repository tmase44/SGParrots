# Review script----

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
formattable(actions,
            align=c('r','c','c','c','c','c','c','c','c'),
            list(`Species` = formatter("span", style = ~ style(font.weight = "bold")),
                 'Neutral'=color_tile(customL,customH),'Displace'=color_tile(customL,customH),'Threat'=color_tile(customL,customH),'Swoop'=color_tile(customL,customH),'Chase'=color_tile(customL,customH),'Contact'=color_tile(customL,customH),'Fight'=color_tile(customL,customH),'Aggression score'=color_tile(customL,customH))) 
