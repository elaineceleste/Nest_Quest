# NQ Habitat analysis 17 Dec 2025

#load packages
library(tidyverse) #dplyr package
library(lubridate)
library(lme4) # GLM models
library(gridExtra) # for creating plot grids
library(grid) 
library(glmmTMB) #beta distribution model
library(ggeffects) # getting predicted values from model

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
all_habitat <- merge(all_habitat, sites[, c('location', 'site')], by= "site") # adding locations


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
  mutate(per_bare_midpoint = recode(per_bare, '0' = 0, '1_to_5' = 3, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5),
         per_forb_midpoint = recode(per_forb, '0' = 0, '1_to_5' = 3, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5),
         per_grass_midpoint = recode(per_grass, '0' = 0, '1_to_5' = 3, '25_to_50' = 37.5, '5_to_25' = 15, '50_to_75' = 62.5, '75_to_100' = 87.5))


# scatter plot looking at each nest's habitat plots
ggplot(all_habitat, aes(x = nest_ID, y = per_grass, color = quadrat_distance)) +
  geom_point(position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0)) +
  facet_grid(vars(year))

# Look at cover class values across different quadrat distances
grass_boxplot <- all_habitat %>% filter(., nest_ID != 'no_nest') %>% 
ggplot(., aes(x = factor(quadrat_distance, levels = c('1m', '10m', '50m')), y = per_grass_midpoint)) +
  geom_boxplot(outliers = FALSE, fill ='lightblue') +
  geom_point(position = position_jitterdodge(jitter.height = 0.3, jitter.width = 0.8), size = 2) +
  xlab('quadrat distance') +
  ylab('Percent cover (midpoint)') +
  facet_grid(vars(year)) +
  ggtitle('Grass cover')

bare_boxplot <- all_habitat %>% filter(., nest_ID != 'no_nest') %>% 
  ggplot(., aes(x = factor(quadrat_distance, levels = c('1m', '10m', '50m')), y = per_bare_midpoint)) +
  geom_boxplot(outliers = FALSE, fill ='lightblue')  +
  geom_point(position = position_jitterdodge(jitter.height = 0.3, jitter.width = 0.8), size = 2) +
  #theme(axis.title.y = NULL) +
  xlab('quadrat distance') +
  ylab('') +
  facet_grid(vars(year)) +
  ggtitle('Bare ground cover')

forb_boxplot <- all_habitat %>% filter(., nest_ID != 'no_nest') %>% 
  ggplot(., aes(x = factor(quadrat_distance, levels = c('1m', '10m', '50m')), y = per_forb_midpoint)) +
  geom_boxplot(outliers = FALSE, fill ='lightblue')  +
  geom_point(position = position_jitterdodge(jitter.height = 0.3, jitter.width = 0.8), size = 2) +
  #theme(axis.title.y = NULL) +
  xlab('quadrat distance') +
  ylab('') +
  facet_grid(vars(year)) +
  ggtitle('Forb cover')

grid.arrange(grass_boxplot, bare_boxplot, forb_boxplot, ncol = 3)


# data frame for average cover for nest ID and year. 
nest_habitat_summarized <- all_habitat %>% 
  filter(., nest_ID != 'no_nest') %>%  # removing no_nest counts for now
  group_by(nest_ID, year, quadrat_distance, location, site) %>% 
  summarize(avg_bare = mean(per_bare_midpoint), #using midpoints to create numeric values from categories
            avg_forb = mean(per_forb_midpoint),
            avg_grass = mean(per_grass_midpoint),
            mean_of_means_litter_cm = mean(mean_litter_depth)) %>% 
  mutate(., quadrat_distance = as.factor(quadrat_distance),
         location = as.factor(location),
         year = as.factor(year),
         site = as.factor(site))



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

ggplot(nest_habitat_summarized, aes(x = site, 
                                    y = avg_bare, 
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
  labs(x = "nest_ID", y = "mean bare midpoint") + #labels
  scale_fill_manual(values = c("#FFA500", "#56B4E9", '#ff6db6'),
                    labels = c("1M", "10M", '50M'),
                    guide = guide_legend(title = NULL))  +
  facet_grid('year')

##### are habitat measures different in the different quadrat distances? Which habitat measures are different? 

all_habitat_nonest <- all_habitat %>% 
  filter(., nest_ID != 'no_nest') %>% # removing no_nest row from dataset for modeling
  mutate(., quadrat_distance = as.factor(quadrat_distance),
         location = as.factor(location)) 

nest_habitat_summarized <- nest_habitat_summarized %>% 
  mutate(., avg_grass_prop = avg_grass / 100, 
            avg_bare_prop = avg_bare / 100, 
            avg_forb_prop = avg_forb/ 100) # creating proportion columns (0-1) to meet assumptions for beta tests

nest_habitat_summarized2025 <- nest_habitat_summarized %>% 
  filter(., year == 2025) 

nest_habitat_summarized$quadrat_distance <- relevel(nest_habitat_summarized$quadrat_distance, ref = '1m') #relevel so reference value is 1 meter quadrats
all_habitat_nonest$quadrat_distance <- relevel(all_habitat_nonest$quadrat_distance, ref = '1m')
nest_habitat_summarized2025$quadrat_distance <- relevel(nest_habitat_summarized2025$quadrat_distance, ref = '1m') #relevel so reference value is 1 meter quadrats



## grass  vs quadrat distances - Data NOT normal. Using only 2025 data, as 2024 data is lacking
shapiro.test(nest_habitat_summarized$avg_grass)
shapiro.test(log(nest_habitat_summarized$avg_grass))
hist(nest_habitat_summarized$avg_grass)

grass_model2 <- glmmTMB(avg_grass_prop ~ quadrat_distance + (1|location), 
                      data = nest_habitat_summarized2025, 
                      family = beta_family) # using means of each quadrat at each nest
summary(grass_model2) # AIC = -82.3
summary(aov(grass_model2)) # quadrat distance has a strong effect on the amount of grass, and there is a minor effect from location.

## forb vs quadrat distances - data is normal when log transformed
shapiro.test(nest_habitat_summarized$avg_forb)
shapiro.test(log1p(nest_habitat_summarized$avg_forb))
hist(nest_habitat_summarized$avg_forb)
hist(log1p(nest_habitat_summarized$avg_forb))

forb_model1 <- glmmTMB(log1p(avg_forb) ~ quadrat_distance + (1|location) + year, #log transform data to make normal
                      data = nest_habitat_summarized, 
                      family = gaussian(link = 'identity'))
                     # start = list(theta = 0)) # using means of each quadrat at each nest
summary(forb_model1) # AIC = 114.2

ggpredict(forb_model1, terms = c("quadrat_distance")) |> plot()
summary(aov(forb_model1)) #location is affecting the outcome of this model.

# model using only 2025 data
shapiro.test(nest_habitat_summarized2025$avg_forb)
shapiro.test(log1p(nest_habitat_summarized2025$avg_forb))
hist(nest_habitat_summarized2025$avg_forb)
hist(log1p(nest_habitat_summarized2025$avg_forb))

forb_model1_2025 <- glmmTMB(log1p(avg_forb) ~ quadrat_distance + (1|location), #log transform data to make normal
                       data = nest_habitat_summarized2025, 
                       family = gaussian(link = 'identity'))
# start = list(theta = 0)) # using means of each quadrat at each nest
summary(forb_model1_2025) # AIC = 52.4

ggpredict(forb_model1_2025, terms = c("quadrat_distance")) |> plot()
summary(aov(forb_model1_2025)) #location is affecting the outcome of this model.

# bare vs quadrat distances - data NOT normal
shapiro.test(nest_habitat_summarized$avg_bare)
hist(nest_habitat_summarized$avg_bare)

bare_model1 <- glmmTMB(avg_bare_prop ~ quadrat_distance + (1|location), 
                       data = nest_habitat_summarized, 
                       family = ordbeta(link = 'logit'))

bare_model2 <- glmmTMB(avg_bare_prop ~ quadrat_distance + (1|location) + year, 
                        data = nest_habitat_summarized, 
                        family = ordbeta(link = 'logit'))

summary(bare_model1) # AIC = 13.0
summary(bare_model2) # AIC = 54.6
summary(aov(bare_model1)) # location is affecting the outcome of this model. 
summary(aov(bare_model2))  # large effect of year and location, small effect with quadrat distance
ggpredict(bare_model1, terms = c("quadrat_distance")) |> plot()
#ggpredict(bare_model2, terms = c("year")) |> plot()

## 2025 data
shapiro.test(nest_habitat_summarized2025$avg_bare)
hist(nest_habitat_summarized2025$avg_bare)

bare_model1_2025 <- glmmTMB(avg_bare_prop ~ quadrat_distance + (1|location), 
                       data = nest_habitat_summarized2025, 
                       family = ordbeta(link = 'logit'))

summary(bare_model1_2025) # AIC = 13.0
summary(aov(bare_model1_2025)) # location is affecting the outcome of this model. 
ggpredict(bare_model1_2025, terms = c("quadrat_distance")) |> plot()

# calculating Emmeans and obtaining predicted values for plotting

library(emmeans)
## grass model
grass_emm<-emmeans(grass_model2, "quadrat_distance", type = "response")
grass_emm

plot(grass_emm, type="response")   # Link scale by default
pairs(grass_emm, reverse = TRUE) # pairwise comparison

# predicted values
preds_grass <- predict(grass_model2, type = "response", se.fit = TRUE)
print(preds_grass)

## forb model
forb_emm<-emmeans(forb_model1, "quadrat_distance", type = "response")
forb_emm

plot(forb_emm, type="response")   # Link scale by default
pairs(forb_emm, reverse = TRUE) # pairwise comparison

# predicted values
preds_forb <- predict(forb_model1, type = "response", se.fit = TRUE)
print(preds_forb)

## bare model
bare_emm<-emmeans(bare_model1, "quadrat_distance", type = "response")
bare_emm

plot(bare_emm, type="response")   # Link scale by default
pairs(bare_emm, reverse = TRUE) # comparison

# predicted values
preds_bare <- predict(bare_model1, type = "response", se.fit = TRUE)
print(preds_bare)

# add predictions to a new dataframe 
nest_habitat_preds <- nest_habitat_summarized
nest_habitat_preds <- nest_habitat_preds[-c(26), ]

nest_habitat_preds2025 <- nest_habitat_summarized2025
nest_habitat_preds2025 <- nest_habitat_preds2025[-c(9), ]

nest_habitat_preds2025$grass_fit <- preds_grass$fit 
nest_habitat_preds2025$grass_se <- preds_grass$se.fit

nest_habitat_preds$forb_fit <- preds_forb$fit 
nest_habitat_preds$forb_se <- preds_forb$se.fit

nest_habitat_preds$bare_fit <- preds_bare$fit 
nest_habitat_preds$bare_se <- preds_bare$se.fit

nest_habitat_preds <- left_join(nest_habitat_preds, nest_habitat_preds2025)

distance_predictions <- nest_habitat_preds %>% select(.,-c(nest_ID, location, year)) # create dataframe to be grouped by habitat only
distance_predictions <- distance_predictions %>%
  group_by(quadrat_distance) %>% 
  summarize_if(is.numeric, mean, na.rm = TRUE, .groups = "drop") # condense data and find mean value of predictions
distance_predictions <- distance_predictions %>% # calculate confidence intervals
  mutate(grass_LCI = grass_fit - grass_se,
         grass_UCI = grass_fit + grass_se,
         forb_LCI = forb_fit - forb_se,
         forb_UCI = forb_fit + forb_se,
         bare_LCI = bare_fit - bare_se,
         bare_UCI = bare_fit + bare_se)

distance_predictions <- probs %>% group_by(habitat) %>% summarize_if(is.numeric, mean, na.rm = TRUE, .groups = "drop")

# Shape data for plotting
fit <- distance_predictions %>% 
  select(., 
         quadrat_distance,
         grass_fit,
         forb_fit,
         bare_fit) %>% 
  pivot_longer(., 
               cols = c(grass_fit, forb_fit, bare_fit), 
               names_to = "cover", 
               values_to = "fit") %>% 
  mutate(., cover = case_when(startsWith(cover, 'grass') ~ 'grass',
                              startsWith(cover, 'forb') ~ 'forb' ,
                              startsWith(cover, 'bare') ~ 'bare'))

LCI <- distance_predictions %>% 
  select(., 
         quadrat_distance,
         grass_LCI,
         forb_LCI,
         bare_LCI) %>% 
  pivot_longer(., 
                cols = c(grass_LCI, forb_LCI, bare_LCI), 
                names_to = "cover", 
                values_to = "LCI") %>% 
  mutate(., cover = case_when(startsWith(cover, 'grass') ~ 'grass',
                              startsWith(cover, 'forb') ~ 'forb' ,
                              startsWith(cover, 'bare') ~ 'bare'))

UCI<- distance_predictions %>% 
  select(., 
         quadrat_distance,
         grass_UCI,
         forb_UCI,
         bare_UCI) %>% 
  pivot_longer(., 
               cols = c(grass_UCI, forb_UCI, bare_UCI), 
               names_to = "cover", 
               values_to = "UCI") %>% 
  mutate(., cover = case_when(startsWith(cover, 'grass') ~ 'grass',
                              startsWith(cover, 'forb') ~ 'forb' ,
                              startsWith(cover, 'bare') ~ 'bare'))

fits_plot <- full_join(fit, LCI)
fits_plot <- full_join(fits_plot, UCI)
fits_plot # data frame for plotting


# plotting predicted values
ggplot(data = fits_plot, aes(x = quadrat_distance, y = fit)) +
  geom_point(size = 8, shape = 20
            # colour= c("olivedrab3", "forestgreen", "goldenrod2")
             ) +
 # ylim(0,1) +
#  scale_x_discrete(limits = c("Forest", "Edge", "Prairie")) +
 # scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))+
  geom_errorbar(aes(ymin = LCI, ymax = UCI), size = 2, width = 0) +
  labs(x = 'Quadrat distance', y = "emmeans") +
  facet_wrap(vars(cover), ncol = 3, nrow = 1, scales = 'free')
