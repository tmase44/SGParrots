# LOAD PACKS----
library(pacman)
p_load(tidyverse,vegan,lubridate,gridExtra,circlize,stringr,readxl,wesanderson)
#https://color.broadbrander.com/EDCB64 

# IMPORT DATA----
Interact <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                       sheet = "Interactions")

# CHORD DATA PREP----
# prepare data frame
intchord<-Interact %>% 
  filter(recipsp!="NA") %>% 
  filter(initsp=="Monk parakeet"|
           initsp=="Tanimbar corella"|
           initsp=="Rose ringed parakeet"|
           initsp=="Red-breasted parakeet"|
           initsp=="Long-tailed parakeet") %>%  
  group_by(initsp,recipsp,isout) %>% 
  tally() %>% 
  filter(n>1) %>% 
  select(-isout)
view(intchord)

reciplist<-Interact %>% select(recipsp)
reciplist$recipsp<-as.factor(reciplist$recipsp)
levels(reciplist$recipsp)

intchord<-intchord %>% 
  mutate(IS=case_when(
    initsp=="Monk parakeet"~"MP",initsp=="Tanimbar corella"~"TC",initsp=="Rose ringed parakeet"~"RRP",
    initsp=="Red-breasted parakeet"~"RBP",initsp=="Long-tailed parakeet"~"LTP"))
intchord<-intchord %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",
    recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    

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
grid.col=c("MP"="#2E604A","TC"="#27223C","RRP"="#E6A2C5","RBP"="#D1362F","LTP"="#EDCB64",
           "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","CI"="grey","GFE"="grey","GH"="grey",        
           "HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey",
           "LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey",
           "DV"="grey","SW"="grey","BB"="grey","YCC"="grey","PH"="grey")
chordDiagram(inchord2,grid.col=grid.col,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Parrot initiated interactions")
# highlight RBP----
grid.col.rbp=c("MP"="#f6d7d5","TC"="#f6d7d5","RRP"="#f6d7d5","RBP"="#D1362F","LTP"="#f6d7d5",
               "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","CI"="grey","GFE"="grey","GH"="grey",        
               "HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey",
               "LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey",
               "DV"="grey","SW"="grey","BB"="grey","YCC"="grey","PH"="grey")
chordDiagram(inchord2,grid.col=grid.col.rbp,transparency = 0.4,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Red-breasted parakeet initiated interactions")

# highlight MP----
grid.col.mp=c("MP"="#2E604A","TC"="#d5dfdb","RRP"="#d5dfdb","RBP"="#d5dfdb","LTP"="#d5dfdb",
              "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","CI"="grey","GFE"="grey","GH"="grey",        
              "HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey",
              "LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey",
              "DV"="grey","SW"="grey","BB"="grey","YCC"="grey","PH"="grey")
chordDiagram(inchord2,grid.col=grid.col.mp,transparency = 0.4,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Monk parakeet initiated interactions")

# highlight TC----
grid.col.tc=c("MP"="#d4d3d8","TC"="#27223C","RRP"="#d4d3d8","RBP"="#d4d3d8","LTP"="#d4d3d8",
              "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","CI"="grey","GFE"="grey","GH"="grey",        
              "HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey",
              "LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey",
              "DV"="grey","SW"="grey","BB"="grey","YCC"="grey","PH"="grey")
chordDiagram(inchord2,grid.col=grid.col.tc,transparency = 0.4,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Tanimbar corella initiated interactions")
# highlight RRP----
grid.col.rrp=c("MP"="#faecf3","TC"="#faecf3","RRP"="#E6A2C5","RBP"="#faecf3","LTP"="#faecf3",
               "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","CI"="grey","GFE"="grey","GH"="grey",        
               "HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey",
               "LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey",
               "DV"="grey","SW"="grey","BB"="grey","YCC"="grey","PH"="grey")
chordDiagram(inchord2,grid.col=grid.col.rrp,transparency = 0.4,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Rose-ringed parakeet initiated interactions")
# highlight LTP----
grid.col.ltp=c("MP"="#fbf5e0","TC"="#fbf5e0","RRP"="#fbf5e0","RBP"="#fbf5e0","LTP"="#EDCB64",
               "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","CI"="grey","GFE"="grey","GH"="grey",        
               "HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey",
               "LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey",
               "DV"="grey","SW"="grey","BB"="grey","YCC"="grey","PH"="grey")
chordDiagram(inchord2,grid.col=grid.col.ltp,transparency = 0.4,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Long-tailed parakeet initiated interactions")


# CHORD INTERACT----
# DATA----
all.ch<-Interact %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
view(all.ch)
all.ch<-all.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
all.ch<-all.ch %>% ungroup %>% select(interaction,RS,n)
# #...RBP----
rbp.ch<-Interact %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Red-breasted parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
view(rbp.ch)
rbp.ch<-rbp.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
rbp.ch<-rbp.ch %>% ungroup %>% select(interaction,RS,n)

#...MP----
mp.ch<-Interact %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Monk parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
view(mp.ch)
mp.ch<-mp.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
mp.ch<-mp.ch %>% ungroup %>% select(interaction,RS,n)

#...TC----
tc.ch<-Interact %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Tanimbar corella") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
view(tc.ch)
tc.ch<-tc.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
tc.ch<-tc.ch %>% ungroup %>% select(interaction,RS,n)

#...RRP----
rrp.ch<-Interact %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Rose ringed parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
view(rrp.ch)
rrp.ch<-rrp.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
rrp.ch<-rrp.ch %>% ungroup %>% select(interaction,RS,n)

#...LTP----
ltp.ch<-Interact %>% 
  filter(recipsp!="NA") %>% filter(initsp=="Long-tailed parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
view(ltp.ch)
ltp.ch<-ltp.ch %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
ltp.ch<-ltp.ch %>% ungroup %>% select(interaction,RS,n)

#PLOTS----
circos.clear()
par(mfrow=c(2,3))
#plot all----
grid.col.all=c("Neutral"="#D3DDDC","Displace"="#DBB165","Swoop"="#1DACE8","Chase"="#2E604A","Threat"="#27223C","Contact"="#D1362F","Fight"="#F24D29",
               "MP"="grey","TC"="grey","RRP"="grey","RBP"="grey","LTP"="grey","AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey","CI"="grey","GFE"="grey","GH"="grey","PH"="grey")
chordDiagram(all.ch,grid.col=grid.col.all,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "All parrot initiated interactions")

#plot RBP----
chordDiagram(rbp.ch,grid.col=grid.col.all,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Red-breasted parakeet initiated interactions")

#plot MP----
chordDiagram(mp.ch,grid.col=grid.col.all,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Monk parakeet initiated interactions")

#plot TC----
chordDiagram(tc.ch,grid.col=grid.col.all,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Tanimbar corella initiated interactions")

#plot RRP----
chordDiagram(rrp.ch,grid.col=grid.col.all,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Rose ringed parakeet initiated interactions")

#plot LTP----
chordDiagram(ltp.ch,grid.col=grid.col.all,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Long-tailed parakeet initiated interactions")

# WINNING INTERACT----
# DATA----
all.ch.w<-Interact %>% 
  filter(recipsp!="NA" & isout=="W") %>% filter(initsp=="Monk parakeet"|initsp=="Tanimbar corella"|initsp=="Rose ringed parakeet"|initsp=="Red-breasted parakeet"|initsp=="Long-tailed parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
view(all.ch.w)
all.ch.w<-all.ch.w %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
all.ch.w<-all.ch.w %>% ungroup %>% select(interaction,RS,n)
# #...Win RBP----
rbp.ch.w<-Interact %>% 
  filter(recipsp!="NA" & isout=="W") %>% filter(initsp=="Red-breasted parakeet") %>%  
  group_by(interaction,recipsp,isout) %>% tally() %>% select(-isout)
view(rbp.ch.w)
rbp.ch.w<-rbp.ch.w %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",recipsp=="Common iora"~"CI",recipsp=="Grey headed fish eagle"~"GFE",recipsp=="Grey heron"~"GH",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Purple heron"~"PH",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Stork billed kingfisher"~"KF",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    
rbp.ch.w<-rbp.ch.w %>% ungroup %>% select(interaction,RS,n)

#PLOTS----
circos.clear()
par(mfrow=c(2,3))
#plot all----
chordDiagram(all.ch.w,grid.col=grid.col.all,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "All winning initiated interactions")

#plot RBP----
chordDiagram(rbp.ch.w,grid.col=grid.col.all,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Red-breasted parakeet winning initiations")

