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

habitat_all <- mutate(habitat_all, mean_litter_depth = rowMeans(select(habitat_all, starts_with("leaf_lit")), na.rm = TRUE))

#### Creating data set that includes quadrant distances, as well as categories for values near nest and in the surronding area. 
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
         proximity = case_when(quadrant_no == "1N" ~ "near", # lumps quadrats into a column "near" (1m) nest and "far" from nest (10m and 50m) 
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
                              startsWith(quadrant_no, "50") ~ "transect", # These are values to represent data at sites where no nest was found. 
                              startsWith(quadrant_no, "0") ~ "transect",
                              startsWith(quadrant_no, "100M") ~ "transect",
                              startsWith(quadrant_no, "25M") ~ "transect",
                              startsWith(quadrant_no, "75M") ~ "transect")) # lumping quadrant distances together

##### Habitat Analysis. Cover  and litter depth data here ####
nest_cover <- nest_quadrats %>% select(.,c("nest_ID",
                                           "year",
                                           "per_bare", 
                                           "per_forb", 
                                           "per_grass", 
                                           "mean_litter_depth",
                                           "quadrant_distance",
                                           "proximity")) #simplifying data set for cover and litter analysis. Only selecting certain columns here.


# I think having a number 1-5 for to represent % ranges of cover in each quadrant will be good to average. 
# Goal is to average each distance.

nest_cover2 <- nest_cover %>% 
  group_by(nest_ID, quadrant_distance, proximity) %>% 
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

# data frame for average cover for nest ID and year. 
nest_cover3 <- nest_cover2 %>% 
  group_by(nest_ID, year, proximity) %>% 
  summarize(avg_bare = mean(per_bare),
            avg_forb = mean(per_forb),
            avg_grass = mean(per_grass),
            mean_of_means_litter_cm = mean(mean_litter_depth))
hist(nest_cover3$avg_grass)
qqnorm(nest_cover3$avg_grass)
#visualizing cover data
### plotting bare ground for each nest
nest_cover3 %>% 
  ggplot(aes(y = avg_bare, x = nest_ID, fill = proximity)) + # defining x and y values. 'fill' is how to get the groupings based on a column. 
  geom_bar(stat = "identity", position = 'dodge', color = 'black')+ 
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5), #rotate x-axis text
        panel.grid.major = element_blank()) + 
  facet_wrap(vars(year), ncol = 2, scale = "free") + # separate data by year
  ylim(0,5)+ # set scale manually
  labs(x = "nest_ID", y = "mean bare ground cover") + #labels
  scale_fill_manual(values = c("purple", "pink", "blue")) # changing color
  
### plotting forb cover for each nest
nest_cover3 %>% 
  ggplot(aes(y = avg_forb, x = nest_ID, fill = proximity)) +
  geom_bar(stat = "identity", position = 'dodge', color = 'black')+ 
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5), 
        panel.grid.major = element_blank()) +
  facet_wrap(vars(year), ncol = 2, scale = "free") +
  ylim(0,5)+
  labs(x = "nest_ID", y = "mean forb cover") +
  scale_fill_manual(values = c("purple", "pink", "blue"))

### plotting grass cover for each nest
nest_cover3 %>% 
  ggplot(aes(y = avg_grass, x = nest_ID, fill = proximity)) +
  geom_bar(stat = "identity", position = 'dodge', color = 'black')+ 
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5), 
        panel.grid.major = element_blank()) +
  facet_wrap(vars(year), ncol = 2, scale = "free") +
  ylim(0,5)+
  labs(x = "nest_ID", y = "mean grass cover") +
  scale_fill_manual(values = c("purple", "pink", "blue"))

### plotting mean of means litter depth
nest_cover3 %>% 
  ggplot(aes(y = mean_of_means_litter_cm, x = nest_ID, fill = proximity)) +
  geom_bar(stat = "identity", position = 'dodge', color = 'black')+ 
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5), 
        panel.grid.major = element_blank()) +
  facet_wrap(vars(year), ncol = 2, scale = "free") +
  ylim(0,5)+
  labs(x = "nest_ID", y = "mean of means litter (cm)") +
  scale_fill_manual(values = c("purple", "pink", "blue"))

###
### I want to see the cover data averaged across ALL nests###
nest_cover_all <- nest_cover2 %>% 
  filter(!is.na(c(per_bare))) %>% 
  filter(!is.na(c(per_forb))) %>%
  #filter(!is.na(c(per_grass))) %>%
  group_by(proximity, year) %>% 
  summarize(avg_bare = mean(per_bare),
            avg_forb = mean(per_forb),
            avg_grass = mean(per_grass))
  
### plotting grass cover for each nest
nest_cover_all %>% 
  # filter(nest_ID == "ah_lowerupper_imp1") %>% 
  ggplot(aes(y = avg_bare, x = proximity)) +
  geom_bar(stat = "identity", position = 'dodge', color = 'black')+ 
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5), 
        panel.grid.major = element_blank()) +
  facet_wrap(vars(year), ncol = 2, scale = "free") +
  ylim(0,5)+
  labs(x = "nest_ID", y = "mean grass cover") 

## box and whisker plot for grass cover for each nest 
ggplot(nest_cover2, aes(x = nest_ID, y = per_grass)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5), 
        panel.grid.major = element_blank()) +
  ylim(0,5)+
  labs(title = "Grass Cover Variability", 
       x = "nest_ID",
       y = "Percent grass cover")


#### Habitat Cover Statistical Analysis ####

## Testing with values averaged for each nest. These are data where each nest has their ranked choices averaged.

#habitat_t_test <- nest_cover3 %>% 
#                    filter(!is.na(avg_grass)) %>% 
#                    filter(!proximity == "transect") %>% 
#                    filter(!nest_ID == "no_nest")
#hist(habitat_t_test$avg_grass)
#qqnorm(habitat_t_test$avg_grass)
#qqline(habitat_t_test$avg_grass)


#result <- t.test(avg_grass ~ proximity, data = habitat_t_test, conf.level = 0.95, paired = FALSE)
#print(result)
#if (result$p.value < 0.05) {
#  print("There is a significant difference in the amount of grass cover in far plots and near plots.")
#} else {
#  print("There is no significant difference in the amount of grass cover in far plots and near plots.")
#}


### Test for data in it's ordinal state, where each value is representative of our percent cover ranking.
  ##  Basically, 1 = no cover and 5 = the highest cover 
## The question: Is there a difference between habitat cover near the nest, versus far from the nest? 
  ## Something to note, is that the "far" category includes 10 and 50 meter measurements, 
    # while the "near" category represents only the 1 meter measurements.
    # Therefore, the sample size for the far category is bigger.

habitat_t_test_standard <- nest_cover2 %>% 
  filter(!is.na(per_grass)) %>% 
  filter(!proximity == "transect") %>% 
  filter(!nest_ID == "no_nest") # removing NA's. Also only interested in habitat data at nests for this test. 
## these data are only representative for 2025 for the moment, as 2024 is missing grass data. 

hist(habitat_t_test_standard$per_grass) # the data is skewed right. it is NOT NORMAL!!
#qqplot(habitat_t_test_standard$per_grass)
#qqline(habitat_t_test_standard$per_grass)

## I was running a t-test at first, before realizing the data was not normal.

#result_2 <- t.test(per_grass ~ proximity, data = habitat_t_test_standard, conf.level = 0.95, paired = FALSE)
#print(result_2)
#if (result_2$p.value < 0.05) {
#  print("There is a significant difference in the amount of grass cover in far plots and near plots.")
#} else {
#  print("There is no significant difference in the amount of grass cover in far plots and near plots.")
#}

## Because the data is not normal, I decided to try a Wilcox Rank Sum test for these data. 
  ## Our data is ordinal, meaning there are rankings. For us it is 1 to 5, to represent our percent cover classes. 

# Performing the Wilxocon Rank Sum Test
# null hypothesis = there is no difference between habitat cover measurements near and far from the nest. 
# alternative = There is a difference between habitat cover measurements near and far from the nest. 
wilcox.test(per_grass~proximity, data = habitat_t_test_standard) # p = 0.008668 < 0.05 | There is difference between grass measurements far and near the nest.
wilcox.test(per_forb~proximity, data = habitat_t_test_standard)  # p = 0.07669  > 0.05 | There is no difference between forb measurements far and near the nest.
wilcox.test(per_bare~proximity, data = habitat_t_test_standard)  # p = 0.3274   > 0.05 | There is no difference between bare ground measurements far and near the nest.

## I also decided to run a Chi-Square test to see if the variables are independent of each other. 
  ## I am still learning about interpreting this, and am not sure if it is relevant. 

# Running Chi Square Test
cont_table = table(habitat_t_test_standard$per_grass,habitat_t_test_standard$proximity) 
print(cont_table)
print(chisq.test(cont_table))

####
##### soil density analysis #####
# hi sami
# hi elise!!!! If you see this, I'm proud of you for pulling code :) 10/20/2025

### Habitat analaysis for soil density data. EB 10/7/25 ###

nest_cover_psi <- nest_quadrats %>% select(.,c("nest_ID", 
                                              "year", 
                                              "psi_avg", 
                                              "quadrant_distance", 
                                              "proximity")) #create new data frame to isolate soil density data

nest_cover_psi <- nest_cover_psi %>%
  group_by(nest_ID, proximity, year) %>%
  summarize(mean_psi = mean(psi_avg))


#barplot(mean_psi ~ nest_ID, data = nest_cover_psi)

## to create a barplot for one individual nest
#nest_cover_psi %>%
  #filter(nest_ID == "ah_lowerupper_imp1") %>% 
  #ggplot(aes(y = mean_psi, x = quadrant_distance)) +
  #geom_bar(stat = "identity") +
  #facet_wrap(vars(quadrat_distance), ncol = 3) +
  #labs(x = "quadrat distance", y = "mean psi :)") + 
 #ggtitle("Arden hills lower upper impatiens nest 1")

nest_cover_psi %>%
ggplot(aes(y = mean_psi, x = nest_ID, fill = proximity)) +
  geom_bar(stat = "identity", position = 'dodge', color = 'black')+ 
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5), 
        panel.grid.major = element_blank()) +
  facet_wrap(vars(year), ncol = 2, scale = "free") +
  ylim(0,400)+
  labs(x = "nest_ID", y = "mean soil compaction (psi)") +
  scale_fill_manual(values = c("purple", "pink", "blue"))

#Create a box and whisker plot for soil compaction variability 

ggplot(habitat_all, aes(x = nest_ID, y = psi_avg)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5), 
        panel.grid.major = element_blank()) +
  ylim(0,400)+
  labs(title = "Soil Compaction Variability", 
       x = "nest_ID",
       y = "soil density (psi)")

