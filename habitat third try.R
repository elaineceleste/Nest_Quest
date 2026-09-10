# NQ Habitat analysis 17 Dec 2025

#load packages
library(tidyverse) #dplyr package
library(lubridate)
library(lme4) # GLM models
library(gridExtra) # for creating plot grids
library(grid) 
library(glmmTMB) #beta distribution model
library(ggeffects) # getting predicted values from model
library(emmeans)
library(AICcmodavg)

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
  mutate(., tip_size_in = as.factor(tip_size_in)) %>%#tip size to factor
      #   quadrat_distance = as.factor(quadrat_distance))
 #quadrat_distance
  drop_na(quadrat_distance) # remove NA's


# creating mid points for habitat analysis
all_habitat <- all_habitat %>% 
  mutate(per_bare_midpoint = recode(per_bare, '0' = 0, '1_to_5' = 3, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5),
         per_forb_midpoint = recode(per_forb, '0' = 0, '1_to_5' = 3, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5),
         per_grass_midpoint = recode(per_grass, '0' = 0, '1_to_5' = 3, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5))


all_habitat_nests <- all_habitat %>% 
  filter(., nest_ID != 'no_nest') %>% 
  #%>% # removing no_nest row from dataset 
  mutate(., quadrat_distance = as.factor(quadrat_distance),
         site = as.factor(site),
         per_bare = as.factor(per_bare),
         per_grass = as.factor(per_grass),
         per_forb = as.factor(per_forb)) 


# changing some penetrometer variable classes
all_habitat_nests <- all_habitat_nests %>% 
  mutate(., tip_size_in = as.factor(tip_size_in),#tip size to factor
         quadrat_distance = as.factor(quadrat_distance)) %>% #quadrat_distance
  drop_na(quadrat_distance) # remove NA's



all_habitat_nests <- all_habitat_nests %>% 
  mutate(., grass_prop = per_grass_midpoint / 100, 
         bare_prop = per_bare_midpoint / 100, 
         forb_prop = per_forb_midpoint/ 100) # creating proportion columns (0-1) to meet assumptions for beta tests

all_habitat_nests$quadrat_distance <- relevel(all_habitat_nests$quadrat_distance, ref = '1m') #relevel so reference value is 1 meter quadrats



# Regression with random effects
# Values on not averaged, and survey plot will be nested within site 

# Grass data 


grass_model <- glmmTMB(grass_prop ~ quadrat_distance, 
                        data = all_habitat_nests, 
                        family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(grass_model) #
summary(aov(grass_model)) #

grass_model_site <- glmmTMB(grass_prop ~ quadrat_distance + (1 |site) , 
                       data = all_habitat_nests, 
                       family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(grass_model_site) #
summary(aov(grass_model_site)) #


grass_model_site_plot <- glmmTMB(grass_prop ~ quadrat_distance + (1 |site) + (1 | survey_plot), 
                       data = all_habitat_nests, 
                       family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(grass_model_site_plot) #
summary(aov(grass_model_site_plot)) #


grass_model_plot <- glmmTMB(grass_prop ~ quadrat_distance + (1 | survey_plot), 
                                 data = all_habitat_nests, 
                                 family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(grass_model_plot) #
summary(aov(grass_model_plot)) #


grass_model_site_plot_nested <- glmmTMB(grass_prop ~ quadrat_distance + (1 | site:survey_plot), 
                            data = all_habitat_nests, 
                            family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(grass_model_site_plot_nested) #
summary(aov(grass_model_site_plot_nested)) #


#Compile AIC values
#define list of models
grass_models <- list(grass_model, 
                     grass_model_site,
                     grass_model_site_plot,
                     grass_model_plot,
                     grass_model_site_plot_nested)

#specify model names
grass_mod_names <- c('grass_model', 
                     'grass_model_site', 
                     'grass_model_site_plot',
                     'grass_model_plot',
                     'grass_model_site_plot_nested')

#calculate AIC of each model
aictab(cand.set = grass_models, modnames = grass_mod_names)


grass_res <- resid(grass_model_site_plot_nested)
plot(fitted(grass_model_site_plot_nested), grass_res) 
abline(0,0)


# bare ground data 

bare_model <- glmmTMB(bare_prop ~ quadrat_distance, 
                       data = all_habitat_nests, 
                       family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(bare_model) #
summary(aov(bare_model)) #

bare_model_site <- glmmTMB(bare_prop ~ quadrat_distance + (1 |site) , 
                            data = all_habitat_nests, 
                            family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(bare_model_site) #
summary(aov(bare_model_site)) #


bare_model_site_plot <- glmmTMB(bare_prop ~ quadrat_distance + (1 |site) + (1 | survey_plot), 
                                 data = all_habitat_nests, 
                                 family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(bare_model_site_plot) #
summary(aov(bare_model_site_plot)) #


bare_model_plot <- glmmTMB(bare_prop ~ quadrat_distance + (1 | survey_plot), 
                            data = all_habitat_nests, 
                            family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(bare_model_plot) #
summary(aov(bare_model_plot)) #


bare_model_site_plot_nested <- glmmTMB(bare_prop ~ quadrat_distance + (1 | site:survey_plot), 
                                        data = all_habitat_nests, 
                                        family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(bare_model_site_plot_nested) #
summary(aov(bare_model_site_plot_nested)) #

#Compile AIC values
#define list of models
bare_models <- list(bare_model, 
                     bare_model_site,
                     bare_model_site_plot,
                     bare_model_plot,
                     bare_model_site_plot_nested)

#specify model names
bare_mod_names <- c('bare_model', 
                     'bare_model_site', 
                     'bare_model_site_plot',
                     'bare_model_plot',
                     'bare_model_site_plot_nested')

#calculate AIC of each model
aictab(cand.set = bare_models, modnames = bare_mod_names)



# forb data 

forb_model <- glmmTMB(forb_prop ~ quadrat_distance, 
                       data = all_habitat_nests, 
                       family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(forb_model) #
summary(aov(forb_model)) #

forb_model_site <- glmmTMB(forb_prop ~ quadrat_distance + (1 |site) , 
                            data = all_habitat_nests, 
                            family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(forb_model_site) #
summary(aov(forb_model_site)) #


forb_model_site_plot <- glmmTMB(forb_prop ~ quadrat_distance + (1 |site) + (1 | survey_plot), 
                                 data = all_habitat_nests, 
                                 family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(forb_model_site_plot) #
summary(aov(forb_model_site_plot)) #


forb_model_plot <- glmmTMB(forb_prop ~ quadrat_distance + (1 | survey_plot), 
                            data = all_habitat_nests, 
                            family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(forb_model_plot) #
summary(aov(forb_model_plot)) #


forb_model_site_plot_nested <- glmmTMB(forb_prop ~ quadrat_distance + (1 | site:survey_plot), 
                                        data = all_habitat_nests, 
                                        family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(forb_model_site_plot_nested) #
summary(aov(forb_model_site_plot_nested)) #

#Compile AIC values
#define list of models
forb_models <- list(forb_model, 
                     forb_model_site,
                     forb_model_site_plot,
                     forb_model_plot,
                     forb_model_site_plot_nested)

#specify model names
forb_mod_names <- c('forb_model', 
                     'forb_model_site', 
                     'forb_model_site_plot',
                     'forb_model_plot',
                     'forb_model_site_plot_nested')

#calculate AIC of each model
aictab(cand.set = forb_models, modnames = forb_mod_names)


predict(grass_model_site_plot_nested, type = "response", se.fit = TRUE)
ggpredict(grass_model_site_plot_nested, terms = c("quadrat_distance")) |> plot()

grass_pred_means <- ggpredict(grass_model_site_plot_nested, terms = c("quadrat_distance")) |> as.data.frame()
head(grass_pred_means)
grass_pred_means <- grass_pred_means %>% mutate(cover = 'grass')


predict(bare_model_site_plot_nested, type = "response", se.fit = TRUE)
ggpredict(bare_model_site_plot_nested, terms = c("quadrat_distance")) |> plot()
bare_pred_means <- ggpredict(bare_model_site_plot_nested, terms = c("quadrat_distance")) |> as.data.frame()
head(bare_pred_means)
bare_pred_means <- bare_pred_means %>% mutate(cover = 'bare')


predict(forb_model_site_plot_nested, type = "response", se.fit = TRUE)
ggpredict(forb_model_site_plot_nested, terms = c("quadrat_distance")) |> plot()
forb_pred_means <- ggpredict(forb_model_site_plot_nested, terms = c("quadrat_distance")) |> as.data.frame()
head(forb_pred_means)
forb_pred_means <- forb_pred_means %>% mutate(cover = 'forb')

all_cover_preds <- full_join(grass_pred_means, bare_pred_means) 
all_cover_preds <- full_join(all_cover_preds, forb_pred_means)
all_cover_preds <- all_cover_preds %>% rename(., quadrat_distance = x)


ggplot(all_cover_preds, aes(x = cover, y = predicted, fill= quadrat_distance)) +
  geom_bar(stat = 'identity',  position=position_dodge(), color = 'black') +
  geom_errorbar(aes(ymin= conf.low, ymax= conf.high), 
                width = .3,
                position=position_dodge(.9),
                size = 1) +
#geom_point(position=position_dodge(.9), size = 2) +
  labs(x = "Habitat cover", y = "Predicted proportion") +
  theme(axis.text.x = element_text(vjust = 0.7, angle = 0, size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(values = c("#838B8B", "#C1CDCD", "#F0FFFF"), 
                    labels = c("1m", "10m", "50m"),
                    guide = guide_legend(title = NULL)) 


emmeans(grass_model_site_plot_nested, pairwise ~ quadrat_distance)
emmeans(bare_model_site_plot_nested, pairwise ~ quadrat_distance)
emmeans(forb_model_site_plot_nested, pairwise ~ quadrat_distance)


### Soil compaction modeling quadrat distance###
shapiro.test(all_habitat_nests$psi_avg)
hist(all_habitat_nests$psi_avg)
# Data is not normal

kruskal.test(psi_avg ~ quadrat_distance, data = all_habitat_nests) 
# soil compaction is not affected by quadrat distance from the nest





##### Running models with only 2025 data #####

all_habitat_nests2025 <- subset(all_habitat_nests, year == 2025)



# Grass 2025 only
grass_model_site_plot_nested2025 <- glmmTMB(grass_prop ~ quadrat_distance + (1 | site:survey_plot), 
                                        data = all_habitat_nests2025, 
                                        family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(grass_model_site_plot_nested2025) #

grass_res2025 <- resid(grass_model_site_plot_nested2025)
plot(fitted(grass_model_site_plot_nested2025), grass_res2025) 
abline(0,0)


# Bare 2025 only
bare_model_site_plot_nested2025 <- glmmTMB(bare_prop ~ quadrat_distance + (1 | site:survey_plot), 
                                       data = all_habitat_nests2025, 
                                       family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(bare_model_site_plot_nested2025) #



# Forb  2025 only
forb_model_site_plot_nested2025 <- glmmTMB(forb_prop ~ quadrat_distance + (1 | site:survey_plot), 
                                       data = all_habitat_nests2025, 
                                       family = ordbeta(link = 'logit')) # using means of each quadrat at each nest
summary(forb_model_site_plot_nested2025) #


# predicted values for 2025 models 
# Grass
predict(grass_model_site_plot_nested2025, type = "response", se.fit = TRUE)
ggpredict(grass_model_site_plot_nested2025, terms = c("quadrat_distance")) |> plot()

grass_pred_means2025 <- ggpredict(grass_model_site_plot_nested2025, terms = c("quadrat_distance")) |> as.data.frame()
head(grass_pred_means2025)
grass_pred_means2025 <- grass_pred_means2025 %>% mutate(cover = 'grass')

#Bare ground
predict(bare_model_site_plot_nested2025, type = "response", se.fit = TRUE)
ggpredict(bare_model_site_plot_nested2025, terms = c("quadrat_distance")) |> plot()

bare_pred_means2025 <- ggpredict(bare_model_site_plot_nested2025, terms = c("quadrat_distance")) |> as.data.frame()
head(bare_pred_means2025)
bare_pred_means2025 <- bare_pred_means2025 %>% mutate(cover = 'bare')

#Forb model
predict(forb_model_site_plot_nested2025, type = "response", se.fit = TRUE)
ggpredict(forb_model_site_plot_nested2025, terms = c("quadrat_distance")) |> plot()

forb_pred_means2025 <- ggpredict(forb_model_site_plot_nested2025, terms = c("quadrat_distance")) |> as.data.frame()
head(forb_pred_means2025)
forb_pred_means2025 <- forb_pred_means2025 %>% mutate(cover = 'forb')

all_cover_preds2025 <- full_join(grass_pred_means2025, bare_pred_means2025) 
all_cover_preds2025 <- full_join(all_cover_preds2025, forb_pred_means2025)
all_cover_preds2025 <- all_cover_preds2025 %>% rename(., quadrat_distance = x)



ggplot(all_cover_preds2025, aes(x = cover, y = predicted, fill= quadrat_distance)) +
  geom_bar(stat = 'identity',  position=position_dodge(), color = 'black') +
  geom_errorbar(aes(ymin= conf.low, ymax= conf.high), 
                width = .3,
                position=position_dodge(.9),
                size = 1) +
  #geom_point(position=position_dodge(.9), size = 2) +
  labs(x = "Habitat cover", y = "Predicted proportion") +
  theme(axis.text.x = element_text(vjust = 0.7, angle = 0, size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(values = c("#838B8B", "#C1CDCD", "#F0FFFF"), 
                    labels = c("1m", "10m", "50m"),
                    guide = guide_legend(title = NULL)) 



emmeans(grass_model_site_plot_nested2025, pairwise ~ quadrat_distance)
emmeans(bare_model_site_plot_nested2025, pairwise ~ quadrat_distance)
emmeans(forb_model_site_plot_nested2025, pairwise ~ quadrat_distance)


filter(all_cover_preds2025, cover == 'grass') %>% 
ggplot(., aes(x = cover, y = predicted, fill= quadrat_distance)) +
  geom_bar(stat = 'identity',  position=position_dodge(), color = 'black') +
  geom_errorbar(aes(ymin= conf.low, ymax= conf.high), 
                width = .3,
                position=position_dodge(.9),
                size = 1) +
  #geom_point(position=position_dodge(.9), size = 2) +
  labs(x = "Habitat cover", y = "Predicted proportion") +
  theme(axis.text.x = element_text(vjust = 0.7, angle = 0, size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(values = c("#838B8B", "#C1CDCD", "#F0FFFF"), 
                    labels = c("1m", "10m", "50m"),
                    guide = guide_legend(title = NULL)) 






# Catergorical tests for differences between quadrat distances. 
# Is there a difference in habitat between the 1 meter, 10 meter and 50 meter quadrats? 
# In grass? bare ground? forbs? 
# 1. Create contingency table for each category. How many times does each category occur, at each distance? 
# 2. Run contingency table in fisher test. 
# Null hypothesis: There is no significant difference in habitat between quadrat distances. 
# Alt hypothesis: There is a significant difference in habitat between quadrat distances.



bare_cont_table <- print(table(all_habitat_nests$quadrat_distance, all_habitat_nests$per_bare)) # create contingency table
grass_cont_table <- print(table(all_habitat_nests$quadrat_distance, all_habitat_nests$per_grass)) # create contingency table
forb_cont_table <- print(table(all_habitat_nests$quadrat_distance, all_habitat_nests$per_forb)) # create contingency table

# fisher's exact test : better suited for small datasets like ours. 
fisher_bare <- fisher.test(bare_cont_table, simulate.p.value = TRUE)
fisher_bare # p-value = 0.001499
bare_cont_df <- data.frame(bare_cont_table)

fisher_grass <- fisher.test(grass_cont_table, simulate.p.value = TRUE)
fisher_grass # p-value = 0.0004998

fisher_forb <- fisher.test(forb_cont_table, simulate.p.value = TRUE)
fisher_forb # p-value = 0.3958


####UNUSED CODE
compaction_model <- glmmTMB(psi_avg ~ quadrat_distance, 
                            data = all_habitat_nests, 
                            family = gaussian(link = "identity")) # using means of each quadrat at each nest
summary(compaction_model) #

compaction_model_site <- glmmTMB(psi_avg ~ quadrat_distance + (1 | site) , 
                                 data = all_habitat_nests, 
                                 family = gaussian(link = "identity")) # using means of each quadrat at each nest
summary(compaction_model_site) #
summary(aov(compaction_model_site)) #


compaction_model_site_plot <- glmmTMB(psi_avg ~ quadrat_distance + (1 |site)  + (1 | survey_plot) , 
                                      data = all_habitat_nests, 
                                      family = gaussian(link = "identity")) # using means of each quadrat at each nest
summary(compaction_model_site_plot) #
summary(aov(compaction_model_site_plot)) #


compaction_model_plot <- glmmTMB(psi_avg ~ quadrat_distance + (1 | survey_plot) , 
                                 data = all_habitat_nests, 
                                 family = gaussian(link = "identity")) # using means of each quadrat at each nest
summary(compaction_model_plot) #
summary(aov(compaction_model_plot)) #


compaction_model_site_plot_nested <- glmmTMB(psi_avg ~ quadrat_distance + (1 | site:survey_plot), 
                                             data = all_habitat_nests, 
                                             family = gaussian(link = "identity")) # using means of each quadrat at each nest
summary(compaction_model_site_plot_nested) #
summary(aov(compaction_model_site_plot_nested)) #

compaction_models <- list(compaction_model, 
                          compaction_model_site,
                          compaction_model_site_plot,
                          compaction_model_plot,
                          compaction_model_site_plot_nested)

#specify model names
compaction_mod_names <- c('compaction_model', 
                          'compaction_model_site', 
                          'compaction_model_site_plot',
                          'compaction_model_plot',
                          'compaction_model_site_plot_nested')

#calculate AIC of each model
aictab(cand.set = compaction_models, modnames = compaction_mod_names)

