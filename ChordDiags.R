# LOAD PACKS----
library(pacman)
p_load(tidyverse,vegan,lubridate,gridExtra,circlize,stringr,readxl)
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
  filter(initsp=="Monk parakeet"|
           initsp=="Tanimbar corella"|
           initsp=="Rose-ringed parakeet"|
           initsp=="Red-breasted parakeet") %>%  
  group_by(initsp,recipsp,isout) %>% 
  tally() %>% 
  filter(n>1) %>% 
  select(-isout)

reciplist<-Chord %>% select(recipsp)
reciplist$recipsp<-as.factor(reciplist$recipsp)
levels(reciplist$recipsp)

intchord<-intchord %>% 
  mutate(IS=case_when(
    initsp=="Monk parakeet"~"MP",
    initsp=="Tanimbar corella"~"TC",
    initsp=="Rose-ringed parakeet"~"RrP",
    initsp=="Red-breasted parakeet"~"RbP",
    initsp=="Long-tailed parakeet"~"LtP"))
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
    recipsp=="Lesser green leafbird"~"LgLB",   
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
    recipsp=="Pink necked green pigeon"~"PngP", 
    recipsp=="Purple heron"~"PH",             
    recipsp=="Red-breasted parakeet"~"RbP",   
    recipsp=="Rock dove"~"RD",               
    recipsp=="Rose-ringed parakeet"~"RrP",     
    recipsp=="Rufous woodpecker"~"RWp",        
    recipsp=="Scaly breasted munia"~"SbM",     
    recipsp=="Spotted dove"~"SD",            
    recipsp=="Squirrel"~"Mammal",                 
    recipsp=="Stork billed kingfisher"~"SbKf",  
    recipsp=="Sulphur crested cockatoo"~"ScC", 
    recipsp=="Tanimbar corella"~"TC",         
    recipsp=="White throated kingfisher"~"WtKf",
    recipsp=="Yellow crested cockatoo"~"YcC",  
    recipsp=="Yellow vented bulbul"~"YvB",
    recipsp=="Zebra dove"~"ZD"))      

inchord2<-intchord %>% ungroup %>% select(IS,RS,n)

inchord2$IS<-as.factor(inchord2$IS)
inchord2$RS<-as.factor(inchord2$RS)
levels(inchord2$IS)

# CHORD SPECIES----
#https://r-graph-gallery.com/chord-diagram.html
# Transform input data in a adjacency matrix
#adjacencyData <- with(intchord, table(initsp, recipsp))
# http://opencolor.tools/palettes/wesanderson/  

# Make the circular plot
par(mfrow=c(1,1))
chordDiagram(inchord2,grid.col=grid.col,annotationTrack = "grid",
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

circos.clear()

#... multi chord!!!!!----
circos.clear()
par(mfrow = c(2, 3))
grid.col=c("AGS"="grey",
    "AK"="grey",
    "BnO"="grey",
    "BtB"="grey",
    "BtSb"="grey",
    "CHE"="grey",
    "CK"="grey",
    "CF"="grey",
    "CHM"="grey",
    "CI"="grey",
   "CM"="grey",
   "CTb"="grey",
    "GhFe"="grey",
   "GH"="grey",               
    "HC"="grey",               
   "HS"="grey",          
   "JM"="grey",              
   "Jf"="grey",              
    "LbC"="grey",        
   "LgLB"="grey",   
   "LB"="grey",       
   "LT"="grey",             
    "LtP"="grey",     
   "MP"="#2E604A",           
   "ObSb"="grey",    
   "OwB"="grey",     
  "OD"="grey",      
   "OmR"="grey",    
  "OpH"="grey",  
   "Mammal"="grey",                    
"PT"="grey",             
   "PngP"="grey", 
    "PH"="grey",             
   "RbP"="#D1362F",   
    "RD"="grey",               
   "RrP"="#E6A2C5",     
   "RWp"="grey",        
   "SbM"="grey",     
"SD"="grey",            
"SbKf"="grey",  
"ScC"="grey", 
"TC"="#27223C",         
"WtKf"="grey",
 "YcC"="grey",  
"YvB"="grey",
"ZD"="grey")    


chordDiagram(inchord2,grid.col=grid.col,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Parrot interaction network")

# multi
rbp<-inchord2 %>% filter(IS=='RbP')
circos.clear()
par(mfrow=c(2,3))
par(cex = 0.5, mar = c(0, 0, 0, 0)) #adjust text size
circos.par(start.degree = 0)
#plot all----
chordDiagram(rbp,grid.col=grid.col,transparency = 0.8,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "RBP")




# CHORD INTERACT----
# DATA----
all.ch<-Chord %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose-ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
#view(all.ch)
all.ch<-all.ch %>% 
  mutate(RS=case_when(recipsp=="Asian glossy starling"~"AGS",
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
                          recipsp=="Lesser green leafbird"~"LgLB",   
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
                          recipsp=="Pink necked green pigeon"~"PngP", 
                          recipsp=="Purple heron"~"PH",             
                          recipsp=="Red-breasted parakeet"~"RbP",   
                          recipsp=="Rock dove"~"RD",               
                          recipsp=="Rose-ringed parakeet"~"RrP",     
                          recipsp=="Rufous woodpecker"~"RWp",        
                          recipsp=="Scaly breasted munia"~"SbM",     
                          recipsp=="Spotted dove"~"SD",            
                          recipsp=="Squirrel"~"Mammal",                 
                          recipsp=="Stork billed kingfisher"~"SbKf",  
                          recipsp=="Sulphur crested cockatoo"~"ScC", 
                          recipsp=="Tanimbar corella"~"TC",         
                          recipsp=="White throated kingfisher"~"WtKf",
                          recipsp=="Yellow crested cockatoo"~"YcC",  
                          recipsp=="Yellow vented bulbul"~"YvB",
                          recipsp=="Zebra dove"~"ZD"))      
all.ch<-all.ch %>% ungroup %>% select(interaction,RS,n)
# #...RBP----
rbp<-Chord %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Red-breasted parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
#view(rbp.ch)
rbp.ch<-rbp.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",
    recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Cat"~"C",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp=="Olive backed bulbul"~"BB",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",recipsp=="Otter"~"O",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Rufous woodpecker"~"WP",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Sunbird sp."~"SB",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
rbp.ch<-rbp.ch %>% ungroup %>% select(interaction,RS,n)

#...MP----
mp.ch<-Interact %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Monk parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
#view(mp.ch)
mp.ch<-mp.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",
    recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Cat"~"C",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp=="Olive backed bulbul"~"BB",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",recipsp=="Otter"~"O",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Rufous woodpecker"~"WP",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Sunbird sp."~"SB",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
mp.ch<-mp.ch %>% ungroup %>% select(interaction,RS,n)

#...TC----
tc.ch<-Interact %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Tanimbar corella") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
#view(tc.ch)
tc.ch<-tc.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",
    recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Cat"~"C",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp=="Olive backed bulbul"~"BB",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",recipsp=="Otter"~"O",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Rufous woodpecker"~"WP",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Sunbird sp."~"SB",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
tc.ch<-tc.ch %>% ungroup %>% select(interaction,RS,n)

#...RRP----
rrp.ch<-Interact %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Rose ringed parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
#view(rrp.ch)
rrp.ch<-rrp.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",
    recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Cat"~"C",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp=="Olive backed bulbul"~"BB",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",recipsp=="Otter"~"O",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Rufous woodpecker"~"WP",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Sunbird sp."~"SB",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
rrp.ch<-rrp.ch %>% ungroup %>% select(interaction,RS,n)

#...LTP----
ltp.ch<-Interact %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Long-tailed parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
#view(ltp.ch)
ltp.ch<-ltp.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",
    recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Cat"~"C",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp=="Olive backed bulbul"~"BB",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",recipsp=="Otter"~"O",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Rufous woodpecker"~"WP",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Sunbird sp."~"SB",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
ltp.ch<-ltp.ch %>% ungroup %>% select(interaction,RS,n)

#PLOTS----
#par(mfrow=c(1,1))
circos.clear()
par(mfrow=c(2,3))
par(cex = 0.5, mar = c(0, 0, 0, 0)) #adjust text size
circos.par(start.degree = 0)
#plot all----
grid.col.all=c("Neutral"="#f7f7f7","Displace"="#eaeccc","Threat"="#feda8b","Swoop"="#fdb366","Chase"="#f67e4b","Contact"="#dd3d2d","Fight"="#a50026",
               "AGS"="grey",
               "AK"="grey",
               "BnO"="grey",
               "BtB"="grey",
               "BtSb"="grey",
               "CHE"="grey",
               "CK"="grey",
               "CF"="grey",
               "CHM"="grey",
               "CI"="grey",
               "CM"="grey",
               "CTb"="grey",
               "GhFe"="grey",
               "GH"="grey",               
               "HC"="grey",               
               "HS"="grey",          
               "JM"="grey",              
               "Jf"="grey",              
               "LbC"="grey",        
               "LgLB"="grey",   
               "LB"="grey",       
               "LT"="grey",             
               "LtP"="grey",     
               "MP"="#2E604A",           
               "ObSb"="grey",    
               "OwB"="grey",     
               "OD"="grey",      
               "OmR"="grey",    
               "OpH"="grey",  
               "Mammal"="grey",                    
               "PT"="grey",             
               "PngP"="grey", 
               "PH"="grey",             
               "RbP"="#D1362F",   
               "RD"="grey",               
               "RrP"="#E6A2C5",     
               "RWp"="grey",        
               "SbM"="grey",     
               "SD"="grey",            
               "SbKf"="grey",  
               "ScC"="grey", 
               "TC"="#27223C",         
               "WtKf"="grey",
               "YcC"="grey",  
               "YvB"="grey",
               "ZD"="grey")
chordDiagram(all.ch,grid.col=grid.col.all,transparency = 0.1,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "All parrot initiated interactions")

#plot RBP----
chordDiagram(rbp,grid.col=grid.col,transparency = 0.1,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Red-breasted parakeet initiated interactions")

#plot MP----
chordDiagram(mp.ch,grid.col=grid.col.all,transparency = 0.1,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Monk parakeet initiated interactions")

#plot TC----
chordDiagram(tc.ch,grid.col=grid.col.all,transparency = 0.1,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Tanimbar corella initiated interactions")

#plot RRP----
chordDiagram(rrp.ch,grid.col=grid.col.all,transparency = 0.1,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Rose ringed parakeet initiated interactions")

#plot LTP----
chordDiagram(ltp.ch,grid.col=grid.col.all,transparency = 0.1,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Long-tailed parakeet initiated interactions")

