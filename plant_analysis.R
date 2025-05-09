# Nest Quest Plant Preliminary Data # SD 24/4/2025

#load packages
library(tidyverse)
library(lubridate)

#load data
plants <- read.csv("NQ_plants_data.csv")

#classifying variables
plants$site <- as.factor(plants$site)
plants$nest_ID <- as.factor(plants$nest_ID)
plants$plant_name <- as.factor(plants$plant_name)
plants$date <- as.Date(plants$date, format = "%m_%d_%Y") #convert to date
#plants = separate(data = plants, col = "date", into = c("day","month","year"),  sep = "_", remove = FALSE)
plants$stem_count <- as.integer(plants$stem_count) #error where "no count" is converted to NA
plants$stem_count <- plants$stem_count %>% replace(is.na(.), 0) #turning those NA's into 0's


#removing unessecary columns for simplicity
plants <- plants %>% select(-c('field_name', 'needs_id', 'Identifer'))


plants$plant_name[plants$plant_name==""] <- NA #convert blank values for to NA. These are plants with unknown identity. 


summary(plants) #check


ggplot(plants, aes(y = plant_name, x = stem_count)) +
  geom_bar(stat = "identity", position = "dodge") # just looking at all plants frequency

occurrence <- plants %>% count(plant_name, sort =TRUE) #occurrence of all plants in quadrats


# Looking at plant communities by nest
 nest_plants_summary <- plants %>% group_by(nest_ID) %>% summarize(total_species = n_distinct(plant_name),
                                            total_stems = sum(stem_count)) 
 

 ##
 nest_plant_comm <- plants %>% group_by(nest_ID, plant_name) %>% summarize(total_stems = sum(stem_count)) 
 
 occur_nest <- plants %>% count(plant_name, nest_ID, sort =TRUE)
 
 nest_plant_comm <- left_join(nest_plant_comm, occur_nest) %>% rename(plant.n = n) #joining number of occurrences of plant species at nest
 
 
 nest_plant_comm %>% 
   filter(nest_ID == "Crow_imp_gc") %>% 
 ggplot(aes(x = reorder(plant_name, plant.n), y = plant.n)) +
   geom_bar(stat = "identity") + coord_flip 
  # labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")
 
 
 #### Creating data set that includes quadrat distances
 nest_quadrats <- plants %>% 
   group_by(nest_ID, plant_name, quadrat_no) %>% 
   summarize(total_stems = sum(stem_count)) %>% 
   mutate(quadrat_distance = case_when(quadrat_no == "1N" ~ "1m",
                                       quadrat_no == "1S" ~ "1m",
                                       quadrat_no == "1E" ~ "1m",
                                       quadrat_no == "1W" ~ "1m",
                                       startsWith(quadrat_no, "10") ~ "10m",
                                       startsWith(quadrat_no, "50") ~ "50m")) # lumping quadrat distances together
   
 
 nest_quadrats$quadrat_distance <- as.factor(nest_quadrats$quadrat_distance)
 
 nest_quad_plant_n<- nest_quadrats %>% count(plant_name, nest_ID, quadrat_distance) %>% rename(plant.n = n) #getting number of plants for each species
 
 nest_quadrats <- full_join(nest_quadrats, nest_quad_plant_n) %>% mutate(stem_density = plant.n/total_stems, round, 2)# joining to main dataframe
 
 #plotting by plant abundance and quadrat distance, one plot per nest
 nest_quadrats %>% 
   filter(nest_ID == "Crow_imp_gc") %>% 
   ggplot(aes(y = reorder(plant_name, plant.n), x = plant.n)) +
   geom_bar(stat = "identity") +
   facet_wrap(vars(quadrat_distance), ncol = 3) +
 labs(x = "plant abundance", y = "plant species") + 
   ggtitle("Crow_imp_gc")
 
 nest_quadrats %>% 
   filter(nest_ID == "Crow2_imp_1") %>% 
   ggplot(aes(y = reorder(plant_name, plant.n), x = plant.n)) +
   geom_bar(stat = "identity") +
   facet_wrap(vars(quadrat_distance), ncol = 3) +
   labs(x = "plant abundance", y = "plant species") + 
   ggtitle("Crow2_imp_1")
 
 nest_quadrats %>% 
   filter(nest_ID == "HP_gris1") %>% 
   ggplot(aes(y = reorder(plant_name, plant.n), x = plant.n)) +
   geom_bar(stat = "identity") +
   facet_wrap(vars(quadrat_distance), ncol = 3) +
   labs(x = "plant abundance", y = "plant species") + 
   ggtitle("HP_gris1")
 
 nest_quadrats %>% 
   filter(nest_ID == "HP_imp1") %>% 
   ggplot(aes(y = reorder(plant_name, plant.n), x = plant.n)) +
   geom_bar(stat = "identity") +
   facet_wrap(vars(quadrat_distance), ncol = 3) +
   labs(x = "plant abundance", y = "plant species") + 
   ggtitle("HP_imp1")
 
 nest_quadrats %>% 
   filter(nest_ID == "HP_imp2") %>% 
   ggplot(aes(y = reorder(plant_name, plant.n), x = plant.n)) +
   geom_bar(stat = "identity") +
   facet_wrap(vars(quadrat_distance), ncol = 3) +
   labs(x = "plant abundance", y = "plant species") + 
   ggtitle("HP_imp2")
 
 nest_quadrats %>% 
   filter(nest_ID == "LH_Eq2_imp1") %>% 
   ggplot(aes(y = reorder(plant_name, plant.n), x = plant.n)) +
   geom_bar(stat = "identity") +
   facet_wrap(vars(quadrat_distance), ncol = 3) +
   labs(x = "plant abundance", y = "plant species") + 
   ggtitle("LH_Eq2_imp1")
 
 nq_habitat <- read.csv("nq_habitat.csv")
  