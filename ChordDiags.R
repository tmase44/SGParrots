# LOAD PACKS----
library(pacman)
p_load(tidyverse,vegan,lubridate,gridExtra,circlize,stringr,readxl,wesanderson)

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

levels(intchord$recipsp)

intchord<-intchord %>% 
  mutate(IS=case_when(
    initsp=="Monk parakeet"~"MP",initsp=="Tanimbar corella"~"TC",initsp=="Rose ringed parakeet"~"RRP",
    initsp=="Red-breasted parakeet"~"RBP",initsp=="Long-tailed parakeet"~"LTP"))
intchord<-intchord %>% 
  mutate(RS=case_when(
    recipsp=="Monk parakeet"~"MP",recipsp=="Tanimbar corella"~"TC",recipsp=="Rose ringed parakeet"~"RRP",
    recipsp=="Red-breasted parakeet"~"RBP",recipsp=="Long-tailed parakeet"~"LTP",
    recipsp=="Asian glossy starling"~"AGS",recipsp=="Asian koel"~"AK",recipsp=="Black naped oriole"~"BNO",recipsp=="Blue tailed bee eater"~"BE",   
    recipsp=="Brown throated sunbird"~"SB",recipsp=="Collared kingfisher"~"KF",recipsp=="Common flameback"~"WP",recipsp=="Common hill myna"~"CHM",        
    recipsp=="House crow"~"HC",recipsp=="House sparrow"~"HS",recipsp=="Javan myna"~"JM",recipsp=="Large billed crow"~"LBC",recipsp=="Lesser green leafbird"~"LGL",recipsp=="Lineated barbet"~"LB",
    recipsp=="Little tern"~"LT",recipsp=="Long-tailed macaque"~"MQ",recipsp=="NA"~"NA",recipsp== "Oriental dollarbird"~"ODB",recipsp== "Oriental magpie robin"~"OMPR",recipsp=="Oriental pied hornbill"~"OPH",
    recipsp=="Pink necked green pigeon"~"DV",recipsp=="Rock dove"~"DV",recipsp=="Spotted dove"~"DV",recipsp=="Swift sp."~"SW",recipsp=="White-bellied kingfisher"~"KF",recipsp=="Yellow vented bulbul"~"BB",
    recipsp=="Zebra dove"~"DV",recipsp=="Yellow crested cockatoo"~"YCC"))    

inchord2<-intchord %>% ungroup %>% select(IS,RS,n)

inchord2$IS<-as.factor(inchord2$IS)
inchord2$RS<-as.factor(inchord2$RS)
levels(inchord2$IS)

# CHORD----
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
           "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey",        
           "HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey",
           "LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey",
           "DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Parrot initiated interactions")
# highlight MP
grid.col.mp=c("MP"="#2E604A","TC"="grey","RRP"="grey","RBP"="grey","LTP"="grey",
              "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col.mp,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Monk parakeet initiated interactions")
# highlight RBP
grid.col.rbp=c("MP"="grey","TC"="grey","RRP"="grey","RBP"="#D1362F","LTP"="grey",
               "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col.rbp,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Red-breasted parakeet initiated interactions")
# highlight TC
grid.col.tc=c("MP"="grey","TC"="#27223C","RRP"="grey","RBP"="grey","LTP"="grey",
              "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col.tc,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Tanimbar corella initiated interactions")
# highlight RRP
grid.col.rrp=c("MP"="grey","TC"="grey","RRP"="#E6A2C5","RBP"="grey","LTP"="grey",
               "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col.rrp,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Rose-ringed parakeet initiated interactions")
# highlight LTP
grid.col.ltp=c("MP"="grey","TC"="grey","RRP"="grey","RBP"="grey","LTP"="#EDCB64",
               "AGS"="grey","AK"="grey","BNO"="grey","BE"="grey","SB"="grey","KF"="grey","WP"="grey","CHM"="grey","HC"="grey","HS"="grey","JM"="grey","LBC"="grey","LGL"="grey","LB"="grey","LT"="grey","MQ"="grey","NA"="grey", "ODB"="grey", "OMPR"="grey","OPH"="grey","DV"="grey","SW"="grey","BB"="grey","YCC"="grey")
chordDiagram(inchord2,grid.col=grid.col.ltp,annotationTrack = "grid",annotationTrackHeight = c(0.03, 0.01),
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(inchord2))))))
# above removes the labels, below adds them in 90deg angle
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title(main = "Long-tailed parakeet initiated interactions")
