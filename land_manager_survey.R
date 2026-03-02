#Responses from land manager survey before field season

land.managers.march.1.refine <- read.csv("G:/Shared drives/Bumble Bee Nest Quest/planning/Land_Management/land-managers-march-1-refine.csv", comment.char="#")
management_data=land.managers.march.1.refine


#getting survey data into countable form by each column
library(tidyverse)
library(janitor)

#land use
management_tidy_land_type <- management_data %>% 
  separate_rows(land,
                sep = ", ") 


management_tidy_land_type <- management_tidy_land_type %>% 
  mutate(land = case_when(
    land %in% c("Wetlands","wetlands", "seasonal wetlands") ~ "wetlands"
    ,TRUE ~ land
  )
  )

management_tidy_land_type <- management_tidy_land_type %>% 
  mutate(land = case_when(
    land %in% c("streams","lakes/ponds", "connected and disconnected backwaters") ~ "open water"
    ,TRUE ~ land
  )
  )


#plot land use

management_tidy_land_type %>% 
  count(land) %>% 
  ggplot(aes(x = n,
             y = land)) +
  geom_col()

####management

management_tidy_management <- management_data %>% 
  separate_rows(management,
                sep = ", ") 

#plot management

management_tidy_management %>% 
  count(management) %>% 
  ggplot(aes(x = n,
             y = management)) +
  geom_col()

####additional manage

management_tidy_additionalmanage <- management_data %>% 
  separate_rows(additionalmanage,
                sep = ", ") 

management_tidy_additionalmanage <- management_tidy_additionalmanage %>% 
  mutate(additionalmanage = case_when(
    additionalmanage %in% c("disking","discing", "Disking to set back succesion") ~ "disking"
    ,TRUE ~ additionalmanage
  )
  )

management_tidy_additionalmanage <- management_tidy_additionalmanage %>% 
  mutate(additionalmanage = case_when(
    additionalmanage %in% c("prescribed haying","haying") ~ "haying"
    ,TRUE ~ additionalmanage
  )
  )
management_tidy_additionalmanage <- management_tidy_additionalmanage %>% 
  mutate(additionalmanage = case_when(
    additionalmanage %in% c("portable biochor kiln for woody disposal","biochar") ~ "biochar"
    ,TRUE ~ additionalmanage
  )
  )


management_tidy_additionalmanage %>% na.omit()

#plot additional management

management_tidy_additionalmanage %>% 
  count(additionalmanage) %>% 
  ggplot(aes(x = n,
             y = additionalmanage)) +
  geom_col()

####habitat

management_tidy_habitat <- management_data %>% 
  separate_rows(habitat,
                sep = ", ") 

management_tidy_habitat <- management_tidy_habitat %>% 
  mutate(habitat = case_when(
    habitat %in% c("We manage very different habitats so our willingness to do these things depends on what habitat/area the action is in and what our objectives are. Some of these actions directly conflict with our goals for some of the habitats/areas we manage.
") ~ "NA"
    ,TRUE ~ habitat
  )
  )

#plot habitat 

management_tidy_habitat %>% 
  count(habitat) %>% 
  ggplot(aes(x = n,
             y = habitat)) +
  geom_col()

####linear

management_tidy_linear <- management_data %>% 
  separate_rows(linear,
                sep = ", ") 




#plot linear 

management_tidy_linear %>% 
  count(linear) %>% 
  ggplot(aes(x = n,
             y = linear)) +
  geom_col()



