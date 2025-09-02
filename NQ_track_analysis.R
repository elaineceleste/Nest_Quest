# nest quest track data and habitat survey proportions
# preliminary analysis and trying things out mostly 
#load packages
library(tidyverse)
library(lubridate)

#load data
hyland24_transects_cover <- read.csv("Hyland24_track_points_coverclasshabitat.csv")
elmo24_transects_cover <- read.csv("Elmo24_track_points_habitat.csv")
crow24_transects_cover <- read.csv("Crow24_track_points_habitat.csv")

table(hyland_cover$hyland2024) #just checking values
table(elmo24_transects_cover$elmoraster)
table(crow24_transects_cover$crowhassen)
# 1 = Grass/Shrub, 4 = Road/Paved Surface, 6 = Deciduous Tree Canopy, 8 = Agriculture, 9 = Emergent Wetland
# the amount of agriculture present in crow and elmo is not truly agriculture, and should probably be lumped with prairie. 

hyland_trasects_summary <- hyland24_transects_cover %>% summarize(total_points = count(hyland24_transects_cover),
                                                      total_grass = length(which(hyland24_transects_cover$hyland2024 == 1)),
                                                      total_paved = length(which(hyland24_transects_cover$hyland2024 == 4)),
                                                      total_decidous = length(which(hyland24_transects_cover$hyland2024 == 6)),
                                                      prop_grass = round(total_grass / total_points, digits = 3),
                                                      prop_paved = round(total_paved / total_points, digits = 3),
                                                      prop_decidous = round(total_decidous / total_points, digits = 3)) %>% 
  mutate(site = c("hyland24"),
         .before=total_points)

                                                      