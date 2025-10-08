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

### Classifying Variables ###
#site column name needs fixed in spreadsheet? 
habitat_all$nest_ID <- as.factor(habitat_all$nest_ID) 
#habitat_all$per_bare <- as.factor(habitat_all$per_bare)
#habitat_all$per_forb <- as.factor(habitat_all$per_forb)
#habitat$per_grass <- as.factor(habitat_all$per_grass)

#first look at data
print(unique(habitat_all$nest_ID)) # 18 unique nests. no_nest is not a nest value.

print(unique(habitat_all$per_bare)) # looks good

print(unique(habitat_all$per_forb)) # looks good

print(unique(habitat_all$per_grass)) # there is a level in here "" that is just a blank space
habitat_all$per_grass[habitat_all$per_grass == "" ] <- NA # changing to NA
print(unique(habitat_all$per_grass)) #check. if "" is gone, then we are good. 

### Data Cleaning ###
#quadrat_no
table(habitat_all$quadrant_no) # values that are "about_x" should just be lumped with the closest quadrat distance
habitat_all$quadrant_no[habitat_all$quadrant_no == 'about_45S'] <- '50S' # replacing values
habitat_all$quadrant_no[habitat_all$quadrant_no == 'about_40S'] <- '50S'
habitat_all$quadrant_no[habitat_all$quadrant_no == 'about_40N'] <- '50N'
habitat_all$quadrant_no[habitat_all$quadrant_no == 'about_45N'] <- '50N'
habitat_all$quadrant_no[habitat_all$quadrant_no == 'about_55E'] <- '50E'
habitat_all$quadrant_no[habitat_all$quadrant_no == 'about_95M'] <- '100M'
table(habitat_all$quadrant_no) # check. do all values look right?

#percent cover data


#### Creating data set that includes quadrant distances
nest_quadrats <- habitat_all %>% 
  group_by(nest_ID, quadrant_no, year) %>% 
  mutate(quadrant_distance = case_when(quadrant_no == "1N" ~ "1m", # column with groupings of quadrat distance
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
                                      startsWith(quadrant_no, "75M") ~ "75m"),
         proximity = case_when(quadrant_no == "1N" ~ "near", # lumps quadrats into near (1m) nest and far from nest (10m and 50m) 
                              quadrant_no == "1S" ~ "near",
                              quadrant_no == "1E" ~ "near",
                              quadrant_no == "1W" ~ "near",
                              quadrant_no == "10N" ~ "far",
                              quadrant_no == "10S" ~ "far",
                              quadrant_no == "10E" ~ "far",
                              quadrant_no == "10W" ~ "far", 
                              quadrant_no == "50N" ~ "far",
                              quadrant_no == "50S" ~ "far",
                              quadrant_no == "50E" ~ "far",
                              quadrant_no == "50W" ~ "far", 
                              startsWith(quadrant_no, "50") ~ "transect",
                              startsWith(quadrant_no, "0") ~ "transect",
                              startsWith(quadrant_no, "100M") ~ "transect",
                              startsWith(quadrant_no, "25M") ~ "transect",
                              startsWith(quadrant_no, "75M") ~ "transect")) # lumping quadrant distances together

##### habitat analysis. Cover  and litter depth data here
nest_cover <- nest_quadrats %>% select(.,c("nest_ID",
                                           "year",
                                           "per_bare", 
                                           "per_forb", 
                                           "per_grass", 
                                           "leaf_lit_cm_1", 
                                           "leaf_lit_cm_2", 
                                           "leaf_lit_cm_3",
                                           "quadrant_distance")) #simplifying data set for cover and litter analysis. Only selecting certain columns here.

# I think having a number 1-5 for to represent % ranges of cover in each quadrant will be good to average. 
# Goal is to average each distance.

nest_cover2 <- nest_cover %>% 
  group_by(nest_ID, quadrant_distance) %>% 
  mutate(per_bare = recode(per_bare, '0' = 0, '1_to_5' = 1, '25_to_50' = 3, '5_to_25' = 2, '50_to_75' = 4, '75_to_100' = 5),
         per_forb = recode(per_forb, '0' = 0, '1_to_5' = 1, '25_to_50' = 3, '5_to_25' = 2, '50_to_75' = 4, '75_to_100' = 5),
         per_grass = recode(per_grass, '0' = 0, '1_to_5' = 1, '25_to_50' = 3, '5_to_25' = 2, '50_to_75' = 4, '75_to_100' = 5))
     # assigned a numeric value to each of the percent cover categories. Now they should be able to be averaged
# 0 = 0
# 1_to_5 = 1
# 5_to_25 = 2
# 25_to_50 = 3
# 50_to_75 = 4
# 75_to_100 = 5

mutate(nest_cover, litter_mean = rowMeans(select(nest_cover, starts_with("leaf_lit")), na.rm = TRUE))

nest_cover2 <- nest_cover2 %>% 
  group_by(nest_ID, quadrant_distance, year) %>% 
  summarize(avg_bare = mean(per_bare),
            avg_forb = mean(per_forb),
            avg_grass = mean(per_grass))
  
# hi sami

### Habitat analaysis for soil density data. EB 10/7/25 ###

nest_cover_psi <- nest_quadrats %>% select(.,c("nest_ID", 
                                              "year", 
                                              "psi_avg", 
                                              "quadrant_distance")) #create new data frame to isolate soil density data

nest_cover_psi <- nest_cover_psi %>%
  group_by(nest_ID, quadrant_distance, year) %>%
  summarize(mean_psi = mean(psi_avg))


#barplot(mean_psi ~ nest_ID, data = nest_cover_psi)

nest_cover_psi %>% 
  filter(nest_ID == "ah_lowerupper_imp1") %>% 
  ggplot(aes(y = mean_psi, x = quadrant_distance)) +
  geom_bar(stat = "identity") +
  #facet_wrap(vars(quadrat_distance), ncol = 3) +
  labs(x = "quadrat distance", y = "mean psi :)") + 
  ggtitle("Arden hills lower upper impatiens nest 1")

