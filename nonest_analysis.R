## Nest Quest habitat analysis 13 March 2026. Specifically looking at sites with no nest to compare with nests. 

#load packages
library(tidyverse) #dplyr package
library(lubridate)
library(lme4) # GLM models
library(gridExtra) # for creating plot grids
library(grid)
library(glmmTMB) #beta distribution model
library(ggeffects) # getting predicted values from model
library(emmeans)
library(RColorBrewer) #to use colorblind friendly color palette for figures

#load data
sites <- read.csv("nest_survey_site_data.csv")
penetrometer <- read.csv("habitat_data/2025_penetrometer.csv")
habitat25 <- read.csv("habitat_data/25_nest_habitat.csv")
habitat24 <- read.csv("habitat_data/24_nest_habitat.csv")
habitat_site_transect24 <- read.csv("habitat_data/24_habitat_site_transect.csv")

##### Data Cleaning #####
#change values to lowercase for consistency
penetrometer <- penetrometer %>% mutate(., site = tolower(site), nest_ID = tolower(nest_ID))
habitat25 <- habitat25 %>% mutate(., site = tolower(site), nest_ID = tolower(nest_ID))
habitat24 <- habitat24 %>% mutate(., site = tolower(site), nest_ID = tolower(nest_ID))
habitat_site_transect24 <- habitat_site_transect24 %>% mutate(., site = tolower(site))

 
#clean
habitat25 <- subset(habitat25, year == '2025') # remove blank rows
habitat25 <- habitat25 %>% mutate(soil_temp_C = as.numeric(soil_temp_C))

penetrometer <- penetrometer %>% mutate(year = NULL, 
                                        month = NULL, 
                                        day = NULL) # dropping columns before joining

# join data sets together
habitat24 <- left_join(habitat24, habitat_site_transect24)
all_habitat <- full_join(habitat25, habitat24)
all_habitat <- left_join(all_habitat, penetrometer)
all_habitat <- merge(all_habitat, sites[, c('location', 'site')], by= "site") # adding locations

# change values to NA
all_habitat[all_habitat == ''] <- NA
all_habitat[all_habitat == 'na'] <- NA

# remove unnecessary columns

all_habitat <- all_habitat %>% mutate(coord_Lat = NULL, 
                                      coord_Lon = NULL, 
                                      coor_N = NULL,
                                      coor_W = NULL,
                                      date_grass_collected = NULL,
                                      X3in_psi = NULL,
                                      X6in._psi = NULL,
                                      X9in_psi = NULL,
                                      X12in_psi = NULL,
                                      X15in_psi = NULL,
                                      X18in_psi = NULL,
                                      long_N = NULL,
                                      lat_W = NULL)

all_habitat <- all_habitat %>% rename(survey_plot = site,
                                      site = location) #change site and location to survery_plot and site respectively. 
all_habitat <- all_habitat %>%
  mutate(year = as.factor(year))

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
                               startsWith(quadrat_no, "50") ~ "transect", # These are values to represent data at survey_plots where no nest was found. 
                               startsWith(quadrat_no, "0") ~ "transect",
                               startsWith(quadrat_no, "100M") ~ "transect",
                               startsWith(quadrat_no, "25M") ~ "transect",
                               startsWith(quadrat_no, "75M") ~ "transect")) # lumping quadrat distances together

# changing some penetrometer variable classes
all_habitat <- all_habitat %>% 
  mutate(., tip_size_in = as.factor(tip_size_in),#tip size to factor
         quadrat_distance = as.factor(quadrat_distance)) %>% #quadrat_distance
  drop_na(quadrat_distance) # remove NA's

# creating mid points for habitat analysis
all_habitat <- all_habitat %>% 
  mutate(per_bare_midpoint = recode(per_bare, '0' = 0, '1_to_5' = 3, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5),
         per_forb_midpoint = recode(per_forb, '0' = 0, '1_to_5' = 3, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5),
         per_grass_midpoint = recode(per_grass, '0' = 0, '1_to_5' = 3, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5))

##### Intital look at data and exploration #####

# scatter plot looking at each nest's habitat plots
ggplot(all_habitat, aes(x = nest_ID, y = per_grass, color = quadrat_distance)) +
  geom_point(position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0)) +
  facet_grid(vars(year))

# Visualizing cover class values along different transect quadrat distances (plots with no nest)
grass_boxplot <- all_habitat %>% filter(., nest_ID == 'no_nest') %>% 
  ggplot(., aes(x = factor(quadrat_distance, levels = c('0m', '25m', '50m', '75m', '100m')), y = per_grass_midpoint)) +
  geom_boxplot(outliers = FALSE, fill ='lightblue') +
  geom_point(position = position_jitterdodge(jitter.height = 0.3, jitter.width = 0.8), size = 2) +
  xlab('quadrat distance') +
  ylab('Percent cover (midpoint)') +
  ggtitle('Grass cover')
grass_boxplot 

bare_boxplot <- all_habitat %>% filter(., nest_ID == 'no_nest') %>% 
  ggplot(., aes(x = factor(quadrat_distance, levels = c('0m', '25m', '50m', '75m','100m')), y = per_bare_midpoint)) +
  geom_boxplot(outliers = FALSE, fill ='lightblue')  +
  geom_point(position = position_jitterdodge(jitter.height = 0.3, jitter.width = 0.8), size = 2) +
  #theme(axis.title.y = NULL) +
  xlab('quadrat distance') +
  ylab('') +
  ggtitle('Bare ground cover')
bare_boxplot

forb_boxplot <- all_habitat %>% filter(., nest_ID == 'no_nest') %>% 
  ggplot(., aes(x = factor(quadrat_distance, levels = c('0m', '25m', '50m', '75m', '100m')), y = per_forb_midpoint)) +
  geom_boxplot(outliers = FALSE, fill ='lightblue')  +
  geom_point(position = position_jitterdodge(jitter.height = 0.3, jitter.width = 0.8), size = 2) +
  #theme(axis.title.y = NULL) +
  xlab('quadrat distance') +
  ylab('') +
  ggtitle('Forb cover')
forb_boxplot

grid.arrange(grass_boxplot, bare_boxplot, forb_boxplot, ncol = 3)

# Viszualizing penetrometer data 

ggplot(all_habitat, aes(x = nest_ID, y = psi_avg)) +
  geom_point(position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0)) +
  theme(axis.text.x = element_text(angle =90))

ggplot(all_habitat, aes(x = nest_ID, y = psi_avg)) +
  geom_boxplot(  ) +
  theme(axis.text.x = element_text(angle =90))

ggplot(all_habitat, aes(x = site, y = psi_avg, fill = nest_ID)) +
  geom_boxplot(  ) +
  theme(axis.text.x = element_text(angle =90))

penetrometer_boxplot <- all_habitat %>% filter(., nest_ID == 'no_nest') %>% 
  ggplot(., aes(x = factor(quadrat_distance, levels = c('0m', '25m', '50m', '75m', '100m')), y = psi_avg)) +
  geom_boxplot(outliers = FALSE, fill ='lightblue')  +
  geom_point(position = position_jitterdodge(jitter.height = 0.3, jitter.width = 0.8), size = 2) +
  #theme(axis.title.y = NULL) +
  xlab('quadrat distance') +
  ylab('average psi')  +
  ggtitle('Soil Compaction (penetrometer)') 
penetrometer_boxplot
summary(penetrometer$psi_avg)
grid.arrange(grass_boxplot, bare_boxplot, forb_boxplot, penetrometer_boxplot, ncol = 2)

##### Looking at habitat by nest vs no nest #####
# Creating data frame for plots with no nest
no_nest_summarized <- all_habitat %>% 
  filter(., nest_ID == 'no_nest') %>%  
  group_by(survey_plot, site, nest_ID, year, quadrat_distance) %>% 
  summarize(avg_bare = mean(per_bare_midpoint), #using midpoints to create numeric values from categories
            avg_forb = mean(per_forb_midpoint),
            avg_grass = mean(per_grass_midpoint),
            mean_of_means_litter_cm = mean(mean_litter_depth),
            compaction = mean(psi_avg)) %>% 
  mutate(., quadrat_distance = as.factor(quadrat_distance),
         site = as.factor(site),
         year = as.factor(year),
         survey_plot = as.factor(survey_plot))

ggplot(no_nest_summarized, aes(x=avg_grass, y=avg_forb, color = quadrat_distance)) +
  geom_point(size = 3) 

#plotting grass measurements at plots with no nest
ggplot(no_nest_summarized, aes(x = survey_plot, 
                                    y = avg_grass, 
                                    fill = factor(quadrat_distance, levels = c('0m', '25m', '50m', '75m', '100m')))) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  theme(#axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 0  , vjust = 0, size = 8.5, color = 'black'),#rotate x-axis text
    axis.text.y = element_text(size = 8, color = 'black'),
    axis.title = element_text(size = 8),
    # panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    # panel.background = element_blank(),
    axis.ticks.x = element_blank()) +
  labs(x = "plot", y = "mean grass midpoint") + #labels
  scale_fill_manual(values = c('#FFA500', '#56B4E9', '#ff6db6', '#999999', '#66c2a5'), 
                    labels = c("0m", "25m", '50m', '75m', '100m'),
                    guide = guide_legend(title = NULL)) 

#plotting bare ground at plots with no nest
ggplot(no_nest_summarized, aes(x = survey_plot, 
                                    y = avg_bare, 
                                    fill = factor(quadrat_distance, levels = c('0m', '25m', '50m', '75m', '100m')))) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  theme(#axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 0  , vjust = 0, size = 8.5, color = 'black'),#rotate x-axis text
    axis.text.y = element_text(size = 8, color = 'black'),
    axis.title = element_text(size = 8),
    # panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    # panel.background = element_blank(),
    axis.ticks.x = element_blank()) +
  labs(x = "plot", y = "mean bare midpoint") + #labels
  scale_fill_manual(values = c('#FFA500', '#56B4E9', '#ff6db6', '#999999', '#66c2a5'),
                    labels = c('0m', '25m', '50m', '75m', '100m'),
                    guide = guide_legend(title = NULL))  +
  facet_grid('year')

#plotting soil compaction at plots with no nest
ggplot(no_nest_summarized, aes(x = survey_plot, 
                                    y = compaction, 
                                    fill = factor(quadrat_distance, levels = c('0m', '25m', '50m', '75m', '100m')))) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  theme(#axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 0  , vjust = 0, size = 8.5, color = 'black'),#rotate x-axis text
    axis.text.y = element_text(size = 8, color = 'black'),
    axis.title = element_text(size = 8),
    # panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    # panel.background = element_blank(),
    axis.ticks.x = element_blank()) +
  labs(x = "plot", y = "mean soil compaction") + #labels
  scale_fill_manual(values = c('#FFA500', '#56B4E9', '#ff6db6', '#999999', '#66c2a5'),
                    labels = c('0m', '25m', '50m', '75m', '100m'),
                    guide = guide_legend(title = NULL)) 

#### Is habitat different at the nest vs. in the absence of a nest? Or: What habitat characteristics predict where nests occur?
##What habitat characteristics are associated with the presence of a nest?? How does habitat near nests differ from habitat in plots without nests?

#Creating dataframe with necessary categories to compare nest and no nest habitat
all_habitat2 <- all_habitat %>% 
  select(survey_plot, year, site, nest_ID, leaf_lit_cm_1, leaf_lit_cm_2, leaf_lit_cm_3, psi_avg, mean_litter_depth, quadrat_distance, proximity, per_grass, per_forb, per_bare, per_bare_midpoint, per_forb_midpoint, per_grass_midpoint) %>%
  mutate(
    site = as.factor(site),
    year = as.factor(year),
    survey_plot = as.factor(survey_plot)
  ) %>%
  mutate(nest_present = if_else(nest_ID == "no_nest", "no", "yes")) %>%
  mutate(nest_present = as.factor(nest_present))

#Converting "no" to 0 and "yes" to 1
all_habitat2$nest_present_num <- ifelse(all_habitat2$nest_present == "yes", 1, 0)

## Modelling 
#Fitting a logistic regression for percent cover data (grass, forbs, bare ground)

##Grass model

grass_model <- glmmTMB(
  nest_present_num ~ per_grass_midpoint + (1 | site:survey_plot),
  data = all_habitat2,
  family = binomial()
)
summary(grass_model)
summary(aov(grass_model))

#without the random effect
grass_model2 <- glm(
  nest_present_num ~ per_grass_midpoint, 
  data = all_habitat2,
  family=binomial()
)

summary(grass_model2)

#predicted values

preds_grass <- data.frame(
  per_grass_midpoint = seq(
    min(all_habitat2$per_grass_midpoint, na.rm = TRUE),
    max(all_habitat2$per_grass_midpoint, na.rm = TRUE)
  )
)

preds_grass$prob <- predict(grass_model, preds_grass, type = "response", re.form = NA)

#plotting it

ggplot() +
  geom_jitter(data = all_habitat2,
             aes(x = per_grass_midpoint, y = nest_present_num),
             alpha = 0.4, height = 0.1) +
  geom_line(data = preds_grass,
            aes(x = per_grass_midpoint, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Grass cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()

ggplot() +
  geom_point(data = preds_grass, 
             aes(x = per_grass_midpoint, y = prob))

#Plotting proportions of grass cover by nest presence for a better visual

#making data frame with proportions

grass_prop_data <- all_habitat2 %>%
  group_by(nest_present, per_grass) %>%
  summarise(count = n()) %>%
  group_by(nest_present) %>%
  mutate(prop = n / sum(n)) %>%
  drop_na()

#Display colorblind friendly color palettes 
display.brewer.all(colorblindFriendly = TRUE)

ggplot(grass_prop_data, aes(x = nest_present, y = prop, fill = per_grass)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Nest presence",
    y = "Count",
    fill = "Grass cover (%)"
  ) +
  scale_fill_brewer(palette = "YlGn") +
  theme_minimal()
            

##Forb model

forb_model <- glmmTMB(
  nest_present_num ~ per_forb_midpoint + (1 | site:survey_plot),
  data = all_habitat2,
  family = binomial()
)
summary(forb_model)
summary(aov(forb_model))

#predicted values

preds_forb <- data.frame(
  per_forb_midpoint = seq(
    min(all_habitat2$per_forb_midpoint, na.rm = TRUE),
    max(all_habitat2$per_forb_midpoint, na.rm = TRUE),
    length.out = 100
  )
)

preds_forb$prob <- predict(forb_model, preds_forb, type = "response", re.form = NA)

#plotting it

ggplot() +
  geom_jitter(data = all_habitat2,
             aes(x = per_forb_midpoint, y = nest_present_num),
             alpha = 0.4, height = 0.1) +
  geom_smooth() +
  geom_line(data = preds_forb,
            aes(x = per_forb_midpoint, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Forb cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()

#Plotting proportions of forb cover by nest presence for a better visual

#making data frame with proportions
forb_prop_data <- all_habitat2 %>%
  group_by(nest_present, per_forb_midpoint) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(nest_present) %>%
  mutate(prop = n / sum(n))

ggplot(forb_prop_data, aes(x = nest_present, y = prop, fill = per_forb_midpoint)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Nest presence",
    y = "Proportion of quadrats",
    fill = "Forb cover (%)"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradientn(colors = brewer.pal(6, "YlGn")) +
  theme_minimal()

#Bare ground model

bare_model <- glmmTMB(
  nest_present_num ~ per_bare_midpoint + (1 | site:survey_plot),
  data = all_habitat2,
  family = binomial()
)
summary(bare_model)
summary(aov(bare_model))

#predicted values

preds_bare <- data.frame(
  per_bare_midpoint = seq(
    min(all_habitat2$per_bare_midpoint, na.rm = TRUE),
    max(all_habitat2$per_bare_midpoint, na.rm = TRUE),
    length.out = 100
  )
)

preds_bare$prob <- predict(bare_model, preds_bare, type = "response", re.form = NA)

#plotting it

ggplot() +
  geom_jitter(data = all_habitat2,
             aes(x = per_bare_midpoint, y = nest_present_num),
             alpha = 0.4, height = 0.1) +
  geom_smooth() +
  geom_line(data = preds_bare,
            aes(x = per_bare_midpoint, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Bare ground cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()

ggplot(data = all_habitat2, aes(x = per_bare_midpoint, y = nest_present_num)) +
  geom_smooth(stat = 'smooth', method = 'glm') + 
  geom_jitter(alpha = 0.4, height = 0.1) +
  ylim (0,1)


ggplot() +
  geom_point(data = preds_bare, 
             aes(x = per_bare_midpoint, y = prob))

#Plotting proportions of grass cover by nest presence for a better visual

#making data frame with proportions
bare_prop_data <- all_habitat2 %>%
  group_by(nest_present, per_bare_midpoint) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(nest_present) %>%
  mutate(prop = n / sum(n))

ggplot(bare_prop_data, aes(x = nest_present, y = prop, fill = per_bare_midpoint)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Nest presence",
    y = "Proportion of quadrats",
    fill = "Bare ground cover (%)"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradientn(colors = brewer.pal(6, "YlGn")) +
  theme_minimal()

#Soil compaction

soil_model <- glmmTMB(
  nest_present_num ~ psi_avg + (1 | site:survey_plot), 
  data = all_habitat2,
  family = binomial()
)
summary(soil_model)
summary(aov(soil_model))

#predicted values

preds_soil <- data.frame(
  psi_avg = seq(
    min(all_habitat2$psi_avg, na.rm = TRUE),
    max(all_habitat2$psi_avg, na.rm = TRUE),
    length.out = 100
  )
)

preds_soil$prob <- predict(soil_model, preds_soil, type = "response", re.form = NA)

#plotting it

ggplot() +
  geom_jitter(data = all_habitat2,
             aes(x = psi_avg, y = nest_present_num),
             alpha = 0.4) +
  geom_line(data = preds_soil,
            aes(x = psi_avg, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Soil compaction (psi)",
    y = "Probability of nest presence"
  ) +
  theme_minimal() 

#Plotting proportions of grass cover by nest presence for a better visual

#making data frame with proportions
soil_prop_data <- all_habitat2 %>%
  group_by(nest_present, psi_avg) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(nest_present) %>%
  mutate(prop = n / sum(n))

ggplot(soil_prop_data, aes(x = nest_present, y = prop, fill = psi_avg)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Nest presence",
    y = "Proportion of quadrats",
    fill = "Soil compaction (psi)"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradientn(colors = brewer.pal(6, "YlGn")) +
  theme_minimal()



