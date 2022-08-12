# LOAD PACKS----
library(pacman)
p_load(tidyverse,vegan,lubridate,gridExtra,circlize,stringr,readxl,kable,kableExtra)
#https://color.broadbrander.com/EDCB64 

# IMPORT DATA----
Chord <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Interactions")
Chord$interaction<-factor(Chord$interaction,
                             levels = c("Neutral","Displace","Threat","Swoop","Chase","Contact","Fight"))


# CHORD DATA PREP----
# prepare data frame
intchord<-Chord %>% 
  filter(recipsp!="NA") %>% 
  filter(interaction!='Neutral') %>% 
  # filter(initsp=="Monk parakeet"|
  #          initsp=="Tanimbar corella"|
  #          initsp=="Rose-ringed parakeet"|
  #          initsp=="Red-breasted parakeet") %>%  
  group_by(initsp,recipsp,isout) %>% 
  tally() %>% 
  filter(n>0) %>% 
  select(-isout)

reciplist<-Chord %>% select(recipsp)
reciplist$recipsp<-as.factor(reciplist$recipsp)
levels(reciplist$recipsp)
factor(intchord$initsp)


intchord<-intchord %>% 
  mutate(IS=case_when(
    initsp=="Asian glossy starling"~"AGS",
    initsp=="Asian koel"~"AK",
    initsp=="Black naped oriole"~"BnO",
    initsp=="Blue throated bee eater"~"BtB",
    initsp=="Brown throated sunbird"~"BtSb",
    initsp=="Cat"~"Mammal",
    initsp=="Changeable hawk eagle"~"CHE",
    initsp=="Collared kingfisher"~"CK",
    initsp=="Common flameback"~"CF",
    initsp=="Common hill myna"~"CHM",
    initsp=="Common iora"~"CI",
    initsp=="Common myna"~"CM",
    initsp=="Common tailorbird"~"CTb",
    initsp=="Grey headed fish eagle"~"GhFe",
    initsp=="Grey heron"~"GH",               
    initsp=="House crow"~"HC",               
    initsp=="House sparrow"~"HS",          
    initsp=="Javan myna"~"JM",              
    initsp=="Junglefowl"~"Jf",              
    initsp=="Large billed crow"~"LbC",        
    initsp=="Lesser green leafbird"~"LgLb",   
    initsp=="Lineated barbet"~"LB",       
    initsp=="Little tern"~"LT",             
    initsp=="Long-tailed macaque"~"Mammal",     
    initsp=="Long-tailed parakeet"~"LtP",     
    initsp=="Monk parakeet"~"MP",           
    initsp=="Olive backed sunbird"~"ObSb",    
    initsp=="Olive winged bulbul"~"OwB",     
    initsp=="Oriental dollarbird"~"OD",      
    initsp=="Oriental magpie robin"~"OmR",    
    initsp=="Oriental pied hornbill"~"OpH",  
    initsp=="Otter"~"Mammal",                    
    initsp=="Pied triller"~"PT",             
    initsp=="Pink necked green pigeon"~"PnGp", 
    initsp=="Purple heron"~"PH",             
    initsp=="Red-breasted parakeet"~"RbP",   
    initsp=="Rock dove"~"RD",               
    initsp=="Rose-ringed parakeet"~"RrP",     
    initsp=="Rufous woodpecker"~"RWp",        
    initsp=="Scaly breasted munia"~"SbM",     
    initsp=="Spotted dove"~"SD",            
    initsp=="Squirrel"~"Mammal",                 
    initsp=="Stork billed kingfisher"~"SbK",  
    initsp=="Sulphur crested cockatoo"~"ScC", 
    initsp=="Tanimbar corella"~"TC",         
    initsp=="White throated kingfisher"~"WtK",
    initsp=="Yellow crested cockatoo"~"YcC",  
    initsp=="Yellow vented bulbul"~"YvB",
    initsp=="Zebra dove"~"ZD"))
intchord<-intchord %>% 
  mutate(RS=case_when(
    recipsp=="Asian glossy starling"~"AGS",
    recipsp=="Asian koel"~"AK",
    recipsp=="Black naped oriole"~"BnO",
    recipsp=="Blue throated bee eater"~"BtB",
    recipsp=="Brown throated sunbird"~"BtSb",
    recipsp=="Cat"~"Mammal",
    recipsp=="Changeable hawk eagle"~"CHE",
    recipsp=="Collared kingfisher"~"CK",
    recipsp=="Common flameback"~"CF",
    recipsp=="Common hill myna"~"CHM",
    recipsp=="Common iora"~"CI",
    recipsp=="Common myna"~"CM",
    recipsp=="Common tailorbird"~"CTb",
    recipsp=="Grey headed fish eagle"~"GhFe",
    recipsp=="Grey heron"~"GH",               
    recipsp=="House crow"~"HC",               
    recipsp=="House sparrow"~"HS",          
    recipsp=="Javan myna"~"JM",              
    recipsp=="Junglefowl"~"Jf",              
    recipsp=="Large billed crow"~"LbC",        
    recipsp=="Lesser green leafbird"~"LgLb",   
    recipsp=="Lineated barbet"~"LB",       
    recipsp=="Little tern"~"LT",             
    recipsp=="Long-tailed macaque"~"Mammal",     
    recipsp=="Long-tailed parakeet"~"LtP",     
    recipsp=="Monk parakeet"~"MP",           
    recipsp=="Olive backed sunbird"~"ObSb",    
    recipsp=="Olive winged bulbul"~"OwB",     
    recipsp=="Oriental dollarbird"~"OD",      
    recipsp=="Oriental magpie robin"~"OmR",    
    recipsp=="Oriental pied hornbill"~"OpH",  
    recipsp=="Otter"~"Mammal",                    
    recipsp=="Pied triller"~"PT",             
    recipsp=="Pink necked green pigeon"~"PnGp", 
    recipsp=="Purple heron"~"PH",             
    recipsp=="Red-breasted parakeet"~"RbP",   
    recipsp=="Rock dove"~"RD",               
    recipsp=="Rose-ringed parakeet"~"RrP",     
    recipsp=="Rufous woodpecker"~"RWp",        
    recipsp=="Scaly breasted munia"~"SbM",     
    recipsp=="Spotted dove"~"SD",            
    recipsp=="Squirrel"~"Mammal",                 
    recipsp=="Stork billed kingfisher"~"SbK",  
    recipsp=="Sulphur crested cockatoo"~"ScC", 
    recipsp=="Tanimbar corella"~"TC",         
    recipsp=="White throated kingfisher"~"WtK",
    recipsp=="Yellow crested cockatoo"~"YcC",  
    recipsp=="Yellow vented bulbul"~"YvB",
    recipsp=="Zebra dove"~"ZD"))      

inchord2<-intchord %>% ungroup %>% 
  select(IS,RS,n) %>% 
  arrange(desc(n))

inchord2$IS<-factor(inchord2$IS,
                    levels = c('YcC','TC','ScC','RrP','MP','RbP','OpH','LtP',
                               'OD','SbK','LbC','HC','AK','JM','BnO'))
inchord2 %>%  
  select(RS,n) %>%
  ungroup() %>% 
  group_by(RS) %>%
  summarise(n=sum(n)) %>% 
  arrange(desc(n)) %>% 
  print(n=42)

inchord2$RS<-factor(inchord2$RS,
                    levels = c('JM','RbP','HC','TC','LtP','OpH','RD', 'AGS', 
                               'RrP', 'MP','ScC','YvB','BnO','CK','AK','OD','Mammal',
                               'SD','BtSb','LbC','CF','CHM','CM','YcC','HS',
                               'PnGp','Jf','LB','LgLb', 'LT','OmR','RWp','BtB',
                               'CHE','GH','GhFe',
                               'ObSb','OwB', 'PH', 'PT','SbK','WtK'))
levels(inchord2$IS)
levels(inchord2$RS)
inchord3<-inchord2 %>% 
  group_by(IS,RS) %>% 
  summarise(n=sum(n))

#ranking check
intchord %>% 
  ungroup %>% 
  select(initsp,n) %>% 
  group_by(initsp) %>% 
  summarise(ints=sum(n)) %>% 
  arrange(desc(ints))

levels(inchord3$IS)

# CHORD SPECIES----
#https://r-graph-gallery.com/chord-diagram.html
# Transform input data in a adjacency matrix
#adjacencyData <- with(intchord, table(initsp, recipsp))
# http://opencolor.tools/palettes/wesanderson/  

# Make the circular plot
#par(mfrow=c(1,1))
#chordDiagram(inchord2,grid.col=grid.col,annotationTrack = "grid",
          #   preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
#circos.track(track.index = 1, panel.fun = function(x, y) {
#  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
             # facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
#}, bg.border = NA)

# WHOLE NETWORK----
circos.clear()

# color scheme
grid.col=c("AGS"="#7BBCE7",
    "AK"="#4393C3",
    "BnO"="#6EA6CD",
    "BtB"="grey",
    "BtSb"="#A8D8DC",
    "CHE"="grey",
    "CK"="#8DCBE4",
    "CF"="#B5DDD8",
    "CHM"="#C2E3D2",
    "CI"="grey",
   "CM"="#C2E3D2",
   "CTb"="grey",
    "GhFe"="grey",
   "GH"="grey",               
    "HC"="#4A7BB7",               
   "HS"="#B5DDD8",          
   "JM"="#4393C3",              
   "Jf"="grey",              
    "LbC"="#2166AC",        
   "LgLb"="grey",   
   "LB"="grey",       
   "LT"="grey",             
    "LtP"="#6059A9",     
   "MP"="#F4A582",           
   "ObSb"="grey",    
   "OwB"="grey",     
  "OD"="#364B9A",      
   "OmR"="grey",    
  "OpH"="#6F4C9B",  
   "Mammal"="#9BD2E1",                    
"PT"="grey",             
   "PnGp"="#B5DDD8", 
    "PH"="grey",             
   "RbP"="#FDDBC7",   
    "RD"="#7EB2E4",               
   "RrP"="#F67E4B",     
   "RWp"="grey",        
   "SbM"="grey",     
"SD"="#A8D8DC",            
"SbK"="#5568B8",  
"ScC"="#D6604D", 
"TC"="#DD3D2D",         
"WtK"="grey",
 "YcC"="#A50026",  
"YvB"="#81C4E7",
"ZD"="grey")    
circos.clear()
circos.par(start.degree = 90)
chordDiagram(inchord3,
             grid.col=grid.col,
             annotationTrack = "grid",
             directional = 1,
             direction.type = c('arrows','diffHeight'),
             link.arr.type = "big.arrow",
             link.arr.length = 0.05,
             big.gap = 20,
             transparency = 0.5,
             annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, 
# below adds the labels at a 90 degree angle
circos.track(track.index = 1, 
             panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

title(main = "Interaction network",cex.main=1.5,line=-1)


# make a reference table----

x<-intchord %>% 
  ungroup %>% 
  select(initsp,IS) %>% 
  rename('Species'='initsp',
         'Code'='IS')
y<-intchord %>% 
  ungroup %>% 
  select(recipsp,RS) %>% 
  rename('Species'='recipsp',
         'Code'='RS')
z<-rbind(x,y) %>% 
  distinct(Code, .keep_all = TRUE) %>% 
  filter(Code!='Mammal') %>% 
  arrange(Species) %>% 
  add_row(Species='Mammals grouped',Code='Mammal')
dim(z)
42/3

z1<-z[1:14, ]
z2<-z[15:28, ]
z3<-z[29:42, ]
z4<-cbind(z1,z2,z3)

z4 %>% 
  kable(align = 'll') %>% 
  kable_styling(full_width = FALSE) %>% 
  column_spec(column = 2, width = "3cm") %>% 
  column_spec(column = 4, width = "3cm") %>% 
  column_spec(column = 6, width = "3cm") %>% 
  add_header_above(header=c("Network reference table: species codes"=6),
                   font_size = 16,
                   align='l') %>% 
  save_kable(file = "ref_table.html")
webshot::webshot("ref_table.html", "ref_table.pdf")


