###NEST QUEST 2024-2025 SOIL SAMPLE ANALYSIS#### LOOKING AT SOIL COMPOSITION/TEXTURE

#Load packages
library(tidyverse) #dplyr package
library(lubridate) #dealing with time and dates
library(readr) # creating .csv files
library(lme4) 
library(colorBlindness)
library(forcats)
library(gridExtra) # for creating plot grids
library(dplyr)
library(ggtern)
library(glmmTMB)
library(brms)

#Load data
soil_results <- read.csv('soil_results_2425.csv')
sample_data24 <- read.csv('soil_sample_data24.csv')
sample_data25 <- read.csv('soil_sample_data25.csv')

#Data cleaning
soil_results <- soil_results %>%
  select(Sample.ID, quadrat_no, Weight..g., Sand..mm., Silt..mm., Clay..mm., Sand...., Silt...., Clay...., soil_type)

soil_results <- soil_results %>%
  rename(.,
         sample_ID = Sample.ID,
         weight_g = Weight..g.,
         sand_mm = Sand..mm.,
         silt_mm = Silt..mm.,
         clay_mm = Clay..mm.,
         sand_perc = Sand....,
         silt_perc = Silt....,
         clay_perc = Clay....)

soil_results <- soil_results %>%
  mutate(sample_ID = case_when(
    startsWith(sample_ID, "AH_LU_imp1") ~ "ah_lowerupper_imp1",
    startsWith(sample_ID, "AH_LU_imp2") ~ "ah_lowerupper_imp2",
    startsWith(sample_ID, "AH_LU_imp3") ~ "ah_lowerupper_imp3",
    startsWith(sample_ID, "AH_North") ~ "ah_north",
    startsWith(sample_ID, "AH_Upper") ~ "ah_upper",
    startsWith(sample_ID, "crow_3") ~ "crow3",
    startsWith(sample_ID, "crow1_imp1") ~"crow1_imp1",
    startsWith(sample_ID, "crow_2") ~ "crow_2",
    startsWith(sample_ID, "HP_gris") ~ "24hp_gris1",
    startsWith(sample_ID, "HP_imp1") ~ "24hp_imp1",
    startsWith(sample_ID, "HP_Sled") ~ "hp_sled",
    startsWith(sample_ID, "HP1_imp1") ~ "25hp1_imp1",
    startsWith(sample_ID, "HP2_imp1") ~ "25hp2_imp1",
    #startsWith(sample_ID, "LE_Barn") ~ "le_barn",
    startsWith(sample_ID, "LE_Barn_Ferv") ~ "le_barn_ferv",
    startsWith(sample_ID, "LE_GC") ~ "le_gc",
    startsWith(sample_ID, "LE_Ski") ~ "le_ski",
    startsWith(sample_ID, "LH_EQ1") ~ "25lh_eq1",
    startsWith(sample_ID, "LH_Schultz_imp1") ~ "lh_schultz_imp1",
    startsWith(sample_ID, "LH_Schultz_imp2") ~ "lh_schultz_imp2",
    startsWith(sample_ID, "LH_Schultz_rufo") ~ "lh_schultz_rufo1",
    startsWith(sample_ID, "LH_SL") ~ "lh_schultz_gris1",
    startsWith(sample_ID, "Side_2") ~ "24lh_eq1",
    startsWith(sample_ID, "AU_LU") ~ "ah_lowerupper_imp3",
    startsWith(sample_ID, "crow2") ~ "crow2",
    startsWith(sample_ID, "crow3") ~ "crow3",
    .default = as.character(sample_ID)
    )) #renaming nest IDs to remove the distance measure 

table(soil_results$sample_ID) #checking that all names are correct. LE_Barn are not updated, but going to remove those values. 

soil_results <- soil_results %>%
  rename(.,
         nest_ID = sample_ID) 

soil_results <- soil_results %>% mutate(., nest_ID = tolower(nest_ID)) #actually renaming this column to nest ID

soil_results <- soil_results %>%
  mutate(survey_plot = case_when(
    startsWith(nest_ID, "ah_lowerupper") ~ "ah_lowerupper",
    startsWith(nest_ID, "lh_schultz") ~ "lh_schultz",
    startsWith(nest_ID, "crow1") ~ "crow1",
    startsWith(nest_ID, "24hp") ~ "24hp",
    startsWith(nest_ID, "24lh_eq1") ~ "24lh_eq1",
    startsWith(nest_ID, "25hp1") ~ "25hp1",
    startsWith(nest_ID, "25hp2") ~ "25hp2",
    startsWith(nest_ID, "25lh_eq1") ~ "25lh_eq1",
    startsWith(nest_ID, "ah_north") ~ "ah_north",
    startsWith(nest_ID, "ah_upper") ~ "ah_upper",
    startsWith(nest_ID, "crow2") ~ "crow2",
    startsWith(nest_ID, "crow3") ~ "crow3",
    startsWith(nest_ID, "hp_sled") ~ "hp_sled",
    startsWith(nest_ID, "le_barn") ~ "le_barn",
    startsWith(nest_ID, "le_gc") ~ "le_gc",
    startsWith(nest_ID, "le_ski") ~ "le_ski")
    ) #cerating survey_plot column 

soil_results <- soil_results %>%
  mutate(nest_ID = case_when(
    nest_ID == "24lh_eq1" ~ "no_nest",
    nest_ID == "25lh_eq1" ~ "no_nest",
    nest_ID == "ah_north" ~ "no_nest",
    nest_ID == "ah_upper" ~ "no_nest",
    nest_ID == "crow2" ~ "no_nest",
    nest_ID == "crow3" ~ "no_nest",
    nest_ID == "hp_sled" ~ "no_nest",
    nest_ID == "le_gc" ~ "no_nest",
    nest_ID == "le_ski" ~ "no_nest",
    TRUE ~ as.character(nest_ID) #changes the nest_ID to no nest in plots without a detected nest
  ))

soil_results <- soil_results %>%
  mutate(quadrat_distance = case_when(quadrat_no == "1N" ~ "1m", # column with groupings of quadrat distance
                                       quadrat_no == "1S" ~ "1m",
                                       quadrat_no == "1E" ~ "1m",
                                       quadrat_no == "1W" ~ "1m",
                                       quadrat_no == "10N" ~ "10m",
                                       quadrat_no == "10S" ~ "10m",
                                       quadrat_no == "10E" ~ "10m",
                                       quadrat_no == "10W" ~ "10m",
                                       startsWith(quadrat_no, "50") ~ "50m",
                                       startsWith(quadrat_no, "0") ~ "0m",
                                       startsWith(quadrat_no, "100M") ~ "100m",
                                       startsWith(quadrat_no, "25M") ~ "25m",
                                       startsWith(quadrat_no, "75M") ~ "75m"),
         proximity = case_when(quadrat_no == "1N" ~ "near", # lumps quadrats into a column "near" (1m) nest and "far" from nest (10m and 50m) 
                               quadrat_no == "1S" ~ "near",
                               quadrat_no == "1E" ~ "near",
                               quadrat_no == "1W" ~ "near",
                               quadrat_no == "10N" ~ "far",
                               quadrat_no == "10S" ~ "far",
                               quadrat_no == "10E" ~ "far",
                               quadrat_no == "10W" ~ "far", 
                               quadrat_no == "50N" ~ "far",
                               quadrat_no == "50S" ~ "far",
                               quadrat_no == "50E" ~ "far",
                               quadrat_no == "50W" ~ "far", 
                               startsWith(quadrat_no, "50") ~ "transect", # These are values to represent data at sites where no nest was found. 
                               startsWith(quadrat_no, "0") ~ "transect",
                               startsWith(quadrat_no, "100M") ~ "transect",
                               startsWith(quadrat_no, "25M") ~ "transect",
                               startsWith(quadrat_no, "75M") ~ "transect"))

soil_results <- soil_results[-c(65,66,84,85,86,87,88,142,143,144,145,146), ] #Removing rows with 2024 data and the le_barn transect data since there is soil data for le_barn_ferv. 
 ##Data is cleaned and ready to merge with the sample data. But first...

##Cleaning sample_data25 to make sure names match to join these data sets 
sample_data25 <- sample_data25 %>%
  rename(.,
         survey_plot = site,
         quadrat_no = quadrant_no)

sample_data25 <- sample_data25 %>%
  select(survey_plot, nest_ID, quadrat_no, year, date_collected, date_dried, date_weighed, dry_tempC, dry_tempF, wt_gms, wt_pebbles) 
           
sample_data25 <- sample_data25 %>%
  filter(year != "2024") #removing 2024 data

sample_data25 <- sample_data25[-c(106,107,108,109,110), ] #removes le_barn no_nest using base r
           
sample_data25 <- sample_data25 %>% mutate(., survey_plot = tolower(survey_plot),
                                          nest_ID = tolower(nest_ID))

sample_data25 <- sample_data25 %>%
  mutate(survey_plot = case_when(
    survey_plot == "hp1" ~ "25hp1",
    survey_plot == "hp2" ~ "25hp2",
    TRUE ~ as.character(survey_plot)
  )) #renaming survey plots from hyland to 25hp1 and 25hp2

sample_data25 <- sample_data25 %>%
  mutate(nest_ID = case_when(
    nest_ID == "hp1_imp1" ~ "25hp1_imp1",
    nest_ID == "hp2_imp1" ~ "25hp2_imp1",
    TRUE ~ as.character(nest_ID)
  )) #renaming nest IDs from hyland to have 25 in the nest ID
#Data is ready to join!!!

all_soil <- full_join(sample_data25, soil_results) 

all_soil_nests <- all_soil %>%
  filter(., nest_ID != 'no_nest')

all_soil_nests$quadrat_distance <- factor(all_soil_nests$quadrat_distance, 
                                          levels = c('1m', '10m', '50m')) #relevel so reference value is 1 meter quadrats and make all distances factors 

all_soil_nests$soil_type <- factor(all_soil_nests$soil_type)

#Creating some visuals to look at the data before running models. 

ggplot(all_soil_nests, aes(x = quadrat_distance, fill = soil_type)) +
  geom_bar() +
  labs(
    x = "Distance from nest",
    y = "Number of observations",
    fill = "Soil type"
  ) +
  theme_classic()

#Plotting proportions
ggplot(all_soil_nests, aes(x = quadrat_distance, fill = soil_type)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Distance from nest",
    y = "Proportion of observations",
    fill = "Soil type"
  ) +
  theme_classic()

#By nest
ggplot(
  all_soil_nests,
  aes(x = quadrat_distance, fill = soil_type)
) +
  geom_bar(position = "fill") +
  facet_wrap(~ nest_ID) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Distance from nest",
    y = "Proportion",
    fill = "Soil type"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) 

#Soil type frequency by distance
ggplot(
  all_soil_nests,
  aes(x = soil_type, fill = quadrat_distance)
) +
  geom_bar(position = "dodge") +
  labs(
    x = "Soil type",
    y = "Number of observations",
    fill = "Distance"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


###Now data is ready for modelling. Using same GLMM with soil_type as the predictor variable and quadrat_distance as the dependent variable. 
##Running models with 2025 data only. 




           