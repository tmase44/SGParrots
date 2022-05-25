library(tiyverse)
library(Distance)

# based on df prepared in Composition

ts_rbp<-changiv %>% 
  filter(Object == "Red-breasted parakeet") %>% 
  select(Region.Label,Study.Area,Area,Sample.Label,Effort,Object,Distance)
view(ts_rbp)
