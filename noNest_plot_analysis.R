#Final take at nest vs. no nest analysis

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

all_habitat <- all_habitat %>%
  mutate(nest_present = if_else(nest_ID == "no_nest", "no", "yes")) %>%
  mutate(nest_present = as.factor(nest_present))
  
all_habitat$nest_present_num <- ifelse(all_habitat$nest_present == "yes", 1, 0)

#Does habitat cover differ between plots where a nest is present and plots without a detected nest? 

plot_habitat_summarized <- all_habitat %>% 
  group_by(survey_plot, site, year, nest_present, nest_present_num) %>% 
  summarize(
    avg_bare  = mean(mean_litter_depth, na.rm = TRUE),
    avg_forb  = mean(per_forb_midpoint, na.rm = TRUE),
    avg_grass = mean(per_grass_midpoint, na.rm = TRUE),
    compaction = mean(psi_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    site = as.factor(site),
    survey_plot = as.factor(survey_plot),
    year = as.factor(year)
  )

#exclude crow_gc (no survey plot) and le_barn (no nest because nest found later on)
plot_habitat_summarized <- plot_habitat_summarized %>%
  filter(!survey_plot %in% c("crow_gc"))

plot_habitat_summarized <- plot_habitat_summarized %>%
  filter (!(survey_plot == "le_barn" & nest_present == "no"))

plot_habitat25 <- plot_habitat_summarized %>%
  filter (!year == 2024)


## Modelling 
#Fitting a logistic regression for percent cover data (grass, forbs, bare ground)

###GRASS 

##Grass model including year as an effect

grass_model <- glmmTMB(
  nest_present_num ~ avg_grass + year + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(grass_model)
drop1(grass_model, test = "Chisq")# Asking what happens if predictor variables are removed from the model #No clear effect of grass cover on nest presence probability, no evidence that nest probability differs from 2024 to 2025
summary(aov(grass_model))
plot(grass_model)
library(s)
plot_model(grass_model, type = "pred", terms = c("nest_present_num"))

#Grass predicted values

preds_grass <- expand.grid(
  avg_grass = seq(min(plot_habitat_summarized$avg_grass),
                  max(plot_habitat_summarized$avg_grass),
                  length.out = 50),
  year = levels(plot_habitat_summarized$year))

preds_grass$prob <- predict(
  grass_model,
  preds_grass,
  type = "response",
  re.form = NA)

#plotting it

ggplot(preds_grass, aes(x = avg_grass, y = prob, color = year)) +
  geom_jitter(data = plot_habitat_summarized,
              aes(x = avg_grass, y = nest_present_num),
              alpha = 0.4, height = 0.1) +
  geom_line(data = preds_grass,
            aes(x = avg_grass, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Grass cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()
 
#Visualizing the data to try and find a better way of plotting it
ggplot(plot_habitat_summarized, aes(x = nest_present, y = avg_grass, fill = nest_present)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~year) +  
  labs(x = "Nest Present", y = "Average Grass (%)", fill = "Nest Present") +
  theme_minimal()

#Grass model without year as an effect
grass_model2 <- glmmTMB(
  nest_present_num ~ avg_grass + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(grass_model2)
drop1(grass_model2, test = "Chisq") #After removing year, grass becomes significant. 

preds_grass2 <- data.frame(
  avg_grass = seq(
    min(plot_habitat_summarized$avg_grass, na.rm = TRUE),
    max(plot_habitat_summarized$avg_grass, na.rm = TRUE),
    length.out = 100
  )
)

preds_grass2$prob <- predict(grass_model2, preds_grass2, type = "response", re.form = NA)

ggplot() +
  geom_jitter(data = plot_habitat_summarized,
              aes(x = avg_grass, y = nest_present_num),
              alpha = 0.4, height = 0.1) +
  geom_line(data = preds_grass2,
            aes(x = avg_grass, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Grass cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()

#Violin plot for visualizing distribution of data
ggplot(plot_habitat_summarized, aes(x = nest_present, y = avg_grass, fill = nest_present)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  #geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.8) +
  labs(
    x = "Nest Presence",
    y = "Average Grass (%)"
  ) +
  facet_wrap(~year) +
  theme_minimal()

#Forb model

forb_model <- glmmTMB(
  nest_present_num ~ avg_forb + year + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(forb_model)
drop1(forb_model, test = "Chisq")

#Forb predicted values

preds_forb <- expand.grid(
  avg_forb = seq(min(plot_habitat_summarized$avg_forb),
                  max(plot_habitat_summarized$avg_forb),
                  length.out = 50),
  year = levels(plot_habitat_summarized$year))

preds_forb$prob <- predict(
  forb_model,
  preds_forb,
  type = "response",
  re.form = NA)

#plotting it

ggplot(preds_forb, aes(x = avg_forb, y = prob, color = year)) +
  geom_jitter(data = plot_habitat_summarized,
              aes(x = avg_forb, y = nest_present_num),
              alpha = 0.4, height = 0.1) +
  geom_line(data = preds_forb,
            aes(x = avg_forb, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Forb cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()

#Bare ground model

bare_model <- glmmTMB(
  nest_present_num ~ avg_bare + year + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(bare_model)
drop1(bare_model, test = "Chisq")

#Bare predicted values

preds_bare <- expand.grid(
  avg_bare = seq(min(plot_habitat_summarized$avg_bare),
                 max(plot_habitat_summarized$avg_bare),
                 length.out = 50),
  year = levels(plot_habitat_summarized$year))

preds_bare$prob <- predict(
  bare_model,
  preds_bare,
  type = "response",
  re.form = NA)

#plotting it

ggplot(preds_bare, aes(x = avg_bare, y = prob, color = year)) +
  geom_jitter(data = plot_habitat_summarized,
              aes(x = avg_bare, y = nest_present_num),
              alpha = 0.4, height = 0.1) +
  geom_line(data = preds_bare,
            aes(x = avg_bare, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Bare ground cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()

#Bare ground model without year as an effect

bare_model2 <- glmmTMB(
  nest_present_num ~ avg_bare + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(bare_model2)
drop1(bare_model2, test = "Chisq") #After removing year, grass becomes significant. 

preds_bare2 <- data.frame(
  avg_bare = seq(
    min(plot_habitat_summarized$avg_bare, na.rm = TRUE),
    max(plot_habitat_summarized$avg_bare, na.rm = TRUE),
    length.out = 100
  )
)

preds_bare2$prob <- predict(bare_model2, preds_bare2, type = "response", re.form = NA)

ggplot(plot_habitat_summarized, aes(x = nest_present, y = avg_bare, fill = nest_present)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  #geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.8) +
  labs(
    x = "Nest Presence",
    y = "Average Bare Ground (%)"
  ) +
  facet_wrap(~year) +
  theme_minimal()

#Soil compaction model

compaction_model <- glmmTMB(
  nest_present_num ~ compaction + (1 | site:survey_plot), #had to remove year as an effect or got all NaN values
  data = plot_habitat_summarized,
  family = binomial()
)

summary(compaction_model)
drop1(compaction_model, test = "Chisq")

#Grass predicted values

preds_compaction <- data.frame(
  compaction = seq(
    min(plot_habitat_summarized$compaction, na.rm = TRUE),
    max(plot_habitat_summarized$compaction, na.rm = TRUE),
    length.out = 100
  )
)

preds_compaction$prob <- predict(compaction_model, preds_compaction, type = "response", re.form = NA)

#plotting it

ggplot() +
  geom_jitter(data = plot_habitat_summarized,
              aes(x = compaction, y = nest_present_num),
              alpha = 0.4) +
  geom_line(data = preds_compaction,
            aes(x = compaction, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Soil compaction (psi)",
    y = "Probability of nest presence"
  ) +
  theme_minimal() 

#Models using ONLY 2025 DATA 

#Grass model 

grass_model25 <- glmmTMB(
  nest_present_num ~ avg_grass + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(grass_model25)


preds_grass25 <- data.frame(
  avg_grass = seq(
    min(plot_habitat25$avg_grass, na.rm = TRUE),
    max(plot_habitat25$avg_grass, na.rm = TRUE),
    length.out = 100
  )
)

preds_grass25$prob <- predict(grass_model25, preds_grass25, type = "response", re.form = NA)

ggplot() + 
  geom_point(data = plot_habitat25,
              aes(x = avg_grass, y = nest_present_num),
              #alpha = 0.4, height = 0.1, #Is this height thing right? 
              size = 3) + 
  geom_line(data = preds_grass25,
            aes(x = avg_grass, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Grass cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()


ggplot(plot_habitat_summarized, aes(x = nest_present, y = avg_grass, fill = nest_present)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  #geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.8) +
  labs(
    x = "Nest Presence",
    y = "Average Grass (%)"
  ) +
  scale_fill_manual(values = c('#56B4E9','#FFA500'),
                    labels = c('no', 'yes')) + 
  theme_minimal()

#Grass model diagnostics (not sure if this is right)

preds_grass25$logit <- log(
  preds_grass25$prob /
    (1 - preds_grass25$prob)
)

ggplot(preds_grass25, aes(x = avg_grass, y = logit)) +
  geom_jitter(size = 3, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    x = "Average Grass (%)",
    y = "Logit (log-odds of nest presence)"
  ) +
  theme_minimal()


#Forb model

forb_model25 <- glmmTMB(
  nest_present_num ~ avg_forb + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(forb_model25)


preds_forb25 <- data.frame(
  avg_forb = seq(
    min(plot_habitat25$avg_forb, na.rm = TRUE),
    max(plot_habitat25$avg_forb, na.rm = TRUE),
    length.out = 100
  )
)

preds_forb25$prob <- predict(forb_model25, preds_forb25, type = "response", re.form = NA)

ggplot() +
  geom_point(data = plot_habitat25,
             aes(x = avg_forb, y = nest_present_num),
             #alpha = 0.4, height = 0.1, #Is this height thing right? 
             size = 3) +
  geom_line(data = preds_forb25,
            aes(x = avg_forb, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Forb cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()


#Bare ground model 

bare_model25 <- glmmTMB(
  nest_present_num ~ avg_bare + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(bare_model25)


preds_bare25 <- data.frame(
  avg_bare = seq(
    min(plot_habitat25$avg_bare, na.rm = TRUE),
    max(plot_habitat25$avg_bare, na.rm = TRUE),
    length.out = 100
  )
)

preds_bare25$prob <- predict(bare_model25, preds_bare25, type = "response", re.form = NA)

ggplot() +
  geom_point(data = plot_habitat25,
              aes(x = avg_bare, y = nest_present_num),
              #alpha = 0.4, height = 0.1, #Is this height thing right? 
              size = 3) +
  geom_line(data = preds_bare25,
            aes(x = avg_bare, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Bare ground cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()

