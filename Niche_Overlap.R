# LOAD PACKS----
library(pacman)
p_load(formattable,knitr,kableExtra,tidyverse,vegan,
       lubridate,gridExtra,circlize,stringr,readxl)


# IMPORT DATA----
NO <- read_excel("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/Survey/Actual/Survey_Data_Entry_Master.xlsx", 
                          sheet = "Composition")
#install.packages('Ostats')
#https://neon-biodiversity.github.io/Ostats/articles/Ostats-introduction.html

library(Ostats)

# Prepare data----
dat<-NO %>% 
  select(Study.Area,Species,Avg_size) %>% filter(Study.Area!='Palawan Beach')
dat
dat$log_size<-log10(dat$Avg_size)
dat

# Run function----
# takes 5 mins or so
Ostats_example <- Ostats(traits = as.matrix(dat[,'log_size', drop = FALSE]),
                         sp = factor(dat$Species),
                         plots = factor(dat$Study.Area),
                         random_seed = 517)
# Output summaries----
Ostats_example$overlaps_norm # does not take into account species abundance
Ostats_example$overlaps_unnorm # takes into account species abundance 
Ostats_example$overlaps_norm_ses
Ostats_example$overlaps_unnorm_ses
view(Ostats_example)
# If the ses (standard effect sizes) value is lower than the lower limit for 
# that community, it suggests that the community overlap value observed is lower
# than expected by chance. 
# Similarly, if the community overlap value is higher than the upper limit,
# the community has a higher overlap than expected by chance

#!!!!!!
# in this example for unorm (abundance taken into effect)
#$ses
#log_size
#Changi Village          -0.4139876 = in between
#Palawan Beach            5.7094837 = high overlap / low sample size
#Pasir Ris Town Park      3.7051646 = high overlap
#Sengkang Riverside Park  1.6777893 = slight upper overlap
#Springleaf               4.0015858 = high overlap
#Stirling Road            1.8219005 = high overlap

# high overlap indicates that large body-size distributions are
# higher than expected from a null model.

# Overlap plots----
#The graphing function Ostats_plot() depends on ggplot2 and can be used to
# visualize species trait overlaps of each community for multiple communities.

# The input dataset needs to have these information: 

# * plots community or site identity: a vector of names to indicate which 
## community or site the individual belongs to. 
# * sp taxon identification: a vector of species or taxa names. 
# * traits trait measurements: a vector of trait measurements for each 
## individual, or a matrix with rows representing individuals and columns
## representing traits. 
# * overlap_dat This input information is optional. 
## It is an object containing the output of Ostats for the same data. 
## If provided, it is used to label the plot panels with the community overlap value

siteID <- dat$Study.Area
taxonID <- dat$Species
trait <- dat$Avg_size

Ostats_plot(plots = siteID, 
            sp = taxonID, 
            traits = trait, 
            overlap_dat = Ostats_example, 
            name_x = 'log10 Body Size (cm)', 
            means = TRUE,
            alpha = 0.2)

is.recursive(Ostats_example)

# CAVITY Prepare data----
dat2<-NO %>% 
  select(Study.Area,Species,sp_lab,CavityYN)%>% filter(Study.Area!='Palawan Beach') %>% 
  filter(CavityYN=='4')
#dat$log_size<-log10(dat$Avg_size)
dat2

# Run function----
# takes 5 mins or so
Ostats_example <- Ostats(traits = as.matrix(dat2[,'CavityYN', drop = FALSE]),
                         sp = factor(dat2$Species),
                         plots = factor(dat2$Study.Area),
                         weight_type = 'hmean', #weights for abundance
                         random_seed = 517)
# Output summaries----
Ostats_example$overlaps_norm # does not take into account species abundance
Ostats_example$overlaps_unnorm # takes into account species abundance 
Ostats_example$overlaps_norm_ses
Ostats_example$overlaps_unnorm_ses
view(Ostats_example)
# If the ses (standard effect sizes) value is lower than the lower limit for 
# that community, it suggests that the community overlap value observed is lower
# than expected by chance. 
# Similarly, if the community overlap value is higher than the upper limit,
# the community has a higher overlap than expected by chance

# high overlap indicates that large body-size distributions are
# higher than expected from a null model.

# Overlap plots----
#The graphing function Ostats_plot() depends on ggplot2 and can be used to
# visualize species trait overlaps of each community for multiple communities.

# The input dataset needs to have these information: 

# * plots community or site identity: a vector of names to indicate which 
## community or site the individual belongs to. 
# * sp taxon identification: a vector of species or taxa names. 
# * traits trait measurements: a vector of trait measurements for each 
## individual, or a matrix with rows representing individuals and columns
## representing traits. 
# * overlap_dat This input information is optional. 
## It is an object containing the output of Ostats for the same data. 
## If provided, it is used to label the plot panels with the community overlap value

siteID <- dat2$Study.Area
taxonID <- dat2$Species
trait2 <- dat2$CavityYN

Ostats_plot(plots = siteID, 
            sp = taxonID, 
            traits = trait2, 
            overlap_dat = Ostats_example, 
            means = FALSE,
            name_x = 'Nesting category 4=cavity',
            alpha = 0.3,
            legend = TRUE)


# FORAGING Prepare data----
dat3<-NO %>% 
  select(Study.Area,Species,sp_lab,CavityYN,Foraginglab)%>% filter(Study.Area!='Palawan Beach') %>% 
  filter(CavityYN=='4')
#dat$log_size<-log10(dat$Avg_size)
dat3

# Run function----
# takes 5 mins or so
Ostats_example <- Ostats(traits = as.matrix(dat3[,'Foraginglab', drop = FALSE]),
                         sp = factor(dat3$Species),
                         plots = factor(dat3$Study.Area),
                         weight_type = 'hmean', #weights for abundance
                         random_seed = 517)
# Output summaries----
Ostats_example$overlaps_norm # does not take into account species abundance
Ostats_example$overlaps_unnorm # takes into account species abundance 
Ostats_example$overlaps_norm_ses
Ostats_example$overlaps_unnorm_ses
view(Ostats_example)
# If the ses (standard effect sizes) value is lower than the lower limit for 
# that community, it suggests that the community overlap value observed is lower
# than expected by chance. 
# Similarly, if the community overlap value is higher than the upper limit,
# the community has a higher overlap than expected by chance

# high overlap indicates that large body-size distributions are
# higher than expected from a null model.

# Overlap plots----
#The graphing function Ostats_plot() depends on ggplot2 and can be used to
# visualize species trait overlaps of each community for multiple communities.

# The input dataset needs to have these information: 

# * plots community or site identity: a vector of names to indicate which 
## community or site the individual belongs to. 
# * sp taxon identification: a vector of species or taxa names. 
# * traits trait measurements: a vector of trait measurements for each 
## individual, or a matrix with rows representing individuals and columns
## representing traits. 
# * overlap_dat This input information is optional. 
## It is an object containing the output of Ostats for the same data. 
## If provided, it is used to label the plot panels with the community overlap value

siteID <- dat3$Study.Area
taxonID <- dat3$Species
trait3 <- dat3$Foraginglab

Ostats_plot(plots = siteID, 
            sp = taxonID, 
            traits = trait3, 
            overlap_dat = Ostats_example, 
            means = TRUE,
            name_x = 'Foraging Niche',
            alpha = 0.3,
            legend = TRUE)
