# Nest Quest '24 + '25 habitat analysis # SD Oct 1 2025

# Variables of interest 
  # Leaf litter
  # percent cover of grass forb, bare ground, at the nest away from the nest. 
  # Soil density data too.


#### SET WORKING DIRECTORY
# getwd() # this is used to see what your current working directory is. 
# setwd() # this is the commmand you use to set a new working directory. 
#load packages
library(tidyverse) #dplyr package
library(lubridate)

#load data
habitat_all <- read.csv("nq_habitat2425.csv") #reading in the data from github

#classifying variables
#site column name needs fixed in spreadsheet? 
habitat_all$nest_ID <- as.factor(habitat_all$nest_ID) 

#first look at data
print(unique(habitat_all$nest_ID)) # 18 unique nests. no_nest is not a nest value.


#cleaning 
table(habitat_all$quadrant_no) # values that are "about_x" should just be lumped with the closest quadrat distance


#### Creating data set that includes quadrat distances
nest_quadrats <- habitat_all %>% 
  group_by(nest_ID, quadrant_no) %>% 
  mutate(quadrant_distance = case_when(quadrant_no == "1N" ~ "1m",
                                      quadrant_no == "1S" ~ "1m",
                                      quadrant_no == "1E" ~ "1m",
                                      quadrant_no == "1W" ~ "1m",
                                      quadrant_no == "10N" ~ "10m",
                                      quadrant_no == "10S" ~ "10m",
                                      quadrant_no == "10E" ~ "10m",
                                      quadrant_no == "10W" ~ "10m",
                                      startsWith(quadrant_no, "50") ~ "50m",
                                      startsWith(quadrant_no, "0") ~ "0m",
                                      startsWith(quadrant_no, "100M") ~ "100m",
                                      startsWith(quadrant_no, "25M") ~ "25m",
                                      startsWith(quadrant_no, "75M") ~ "75m")) # lumping quadrat distances together
