# NQ Habitat analysis 17 Dec 2025

#load packages
library(tidyverse) #dplyr package
library(lubridate)
library(lme4) # GLM models
library(gridExtra) # for creating plot grids
library(grid)

#load data
sites <- read.csv("nest_survey_site_data.csv") 
penetrometer <- read.csv("habitat_data/2025_penetrometer.csv")
habitat25 <- read.csv("habitat_data/25_nest_habitat.csv")
habitat24 <- read.csv("habitat_data/24_nest_habitat.csv")
habitat_site_transect24 <- read.csv("habitat_data/24_habitat_site_transect.csv")

#change values to lowercase for consistency
penetrometer <- penetrometer %>% mutate(., site = tolower(site), nest_ID = tolower(nest_ID))
habitat25 <- habitat25 %>% mutate(., site = tolower(site), nest_ID = tolower(nest_ID))
habitat24 <- habitat24 %>% mutate(., site = tolower(site), nest_ID = tolower(nest_ID))
habitat_site_transect24 <- habitat_site_transect24 %>% mutate(., site = tolower(site))


#clean
habitat25 <- subset(habitat25, year == '2025') # remove blank rows
habitat25 <- habitat25 %>% mutate(soil_temp_C = as.numeric(soil_temp_C))

# join data sets together
habitat24 <- left_join(habitat24, habitat_site_transect24)
all_habitat <- full_join(habitat25, habitat24)

# change values to NA
all_habitat[all_habitat == ''] <- NA
all_habitat[all_habitat == 'na'] <- NA

# remove unnecessary columns

all_habitat <- all_habitat %>% mutate(coord_Lat = NULL, 
                                      coord_Lon = NULL, 
                                      coor_N = NULL,
                                      coor_W = NULL,
                                      date_grass_collected = NULL)

#cleaning quadrat column
table(all_habitat$quadrat_no) # values that are "about_x" should just be lumped with the closest quadrat distance
all_habitat$quadrat_no[all_habitat$quadrat_no == 'about_45S'] <- '50S' # replacing values
all_habitat$quadrat_no[all_habitat$quadrat_no == 'about_40S'] <- '50S'
all_habitat$quadrat_no[all_habitat$quadrat_no == 'about_40N'] <- '50N'
all_habitat$quadrat_no[all_habitat$quadrat_no == 'about_45N'] <- '50N'
all_habitat$quadrat_no[all_habitat$quadrat_no == 'about_55E'] <- '50E'
all_habitat$quadrat_no[all_habitat$quadrat_no == 'about_95M'] <- '100M'
all_habitat$quadrat_no[all_habitat$quadrat_no == 'about_37E'] <- '50E'
table(all_habitat$quadrat_no) # check. do all values look right?

#percent cover data

all_habitat <- mutate(all_habitat, mean_litter_depth = rowMeans(select(all_habitat, starts_with("leaf_lit")), na.rm = TRUE))
#note litter depths between 2024 and 2025 seem very different

# Creating quadrat distances column and distance/proximity column
all_habitat <- all_habitat %>%  
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
                               startsWith(quadrat_no, "75M") ~ "transect")) # lumping quadrat distances together

# creating mid points for habitat analysis
all_habitat <- all_habitat %>% 
  mutate(per_bare_midpoint = recode(per_bare, '0' = 0, '1_to_5' = 2.5, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5),
         per_forb_midpoint = recode(per_forb, '0' = 0, '1_to_5' = 2.5, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5),
         per_grass_midpoint = recode(per_grass, '0' = 0, '1_to_5' = 2.5, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5))


# scatter plot looking at each nest's habitat plots
ggplot(all_habitat, aes(x = nest_ID, y = per_grass, color = quadrat_distance)) +
  geom_point(position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0)) +
  facet_grid(vars(year))

# Look at cover class values across different quadrat distances
grass_boxplot <- all_habitat %>% filter(., nest_ID != 'no_nest') %>% 
ggplot(., aes(x = factor(quadrat_distance, levels = c('1m', '10m', '50m')), y = per_grass_midpoint)) +
  geom_boxplot() +
  xlab('quadrat distance') +
  facet_grid(vars(year))

bare_boxplot <- all_habitat %>% filter(., nest_ID != 'no_nest') %>% 
  ggplot(., aes(x = factor(quadrat_distance, levels = c('1m', '10m', '50m')), y = per_bare_midpoint)) +
  geom_boxplot() +
  xlab('quadrat distance') +
  facet_grid(vars(year))

forb_boxplot <- all_habitat %>% filter(., nest_ID != 'no_nest') %>% 
  ggplot(., aes(x = factor(quadrat_distance, levels = c('1m', '10m', '50m')), y = per_forb_midpoint)) +
  geom_boxplot() +
  xlab('quadrat distance') +
  facet_grid(vars(year))

grid.arrange(grass_boxplot, bare_boxplot, forb_boxplot, ncol = 3)


# data frame for average cover for nest ID and year. 
nest_habitat_summarized <- all_habitat %>% 
  filter(., nest_ID != 'no_nest') %>%  # removing no_nest counts for now
  group_by(nest_ID, year, quadrat_distance) %>% 
  summarize(avg_bare = mean(per_bare_midpoint), #using midpoints to create numeric values from categories
            avg_forb = mean(per_forb_midpoint),
            avg_grass = mean(per_grass_midpoint),
            mean_of_means_litter_cm = mean(mean_litter_depth))

ggplot(nest_habitat_summarized, aes(x=avg_grass, y=avg_forb, color = quadrat_distance)) +
  geom_point(size = 3) 

ggplot(nest_habitat_summarized, aes(x = nest_ID, 
                                    y = avg_grass, 
                                    fill = factor(quadrat_distance, levels = c('1m', '10m', '50m')))) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  theme(#axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 0  , vjust = 0, size = 8.5, color = 'black'),#rotate x-axis text
    axis.text.y = element_text(size = 8, color = 'black'),
    axis.title = element_text(size = 8),
   # panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
   # panel.background = element_blank(),
    axis.ticks.x = element_blank()) +
  labs(x = "nest_ID", y = "mean grass midpoint") + #labels
  scale_fill_manual(values = c("#FFA500", "#56B4E9", '#ff6db6'),
                    labels = c("1M", "10M", '50M'),
                    guide = guide_legend(title = NULL)) 
                                      

hist(nest_cover3$avg_grass)
qqnorm(nest_cover3$avg_grass)

