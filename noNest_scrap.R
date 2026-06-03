##Scrap code from no nest analysis
#all_habitat is dataframe in nonest_analysis.R
##### Are habitat measures different at the plot level (plots with vs without a nest)? But determined that we don't have enough data to look at this data at the plot level. 

#Create data frame with one row per nest or one row per plot -> Does this need to be simplified even further? Averaging or summarizing all of the nests within one plot? 
plot_habitat_summarized <- all_habitat %>% 
  group_by(nest_ID, survey_plot, site, year) %>% 
  summarize(
    avg_bare  = mean(mean_litter_depth, na.rm = TRUE),
    avg_forb  = mean(per_forb_midpoint, na.rm = TRUE),
    avg_grass = mean(per_grass_midpoint, na.rm = TRUE),
    compaction = mean(psi_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    site = as.factor(site),
    year = as.factor(year),
    survey_plot = as.factor(survey_plot)
  )

plot_habitat_summarized <- plot_habitat_summarized %>%
  mutate(nest_present = if_else(nest_ID == "no_nest", "no", "yes")) %>%
  mutate(nest_present = as.factor(nest_present))  #adds column to identify whether row represents a nest or not


plot_habitat_summarized <- plot_habitat_summarized %>% 
  mutate(., avg_grass_prop = avg_grass / 100, 
         avg_bare_prop = avg_bare / 100, 
         avg_forb_prop = avg_forb/ 100) # creating proportion columns (0-1) to meet assumptions for beta tests

plot_habitat_summarized2025 <- plot_habitat_summarized %>% 
  filter(., year == 2025) 

#Modeling all habitat measures together -> Model is asking: how do these habitat variables influence nest presence? 
plot_model <- glmmTMB(
  nest_present ~ avg_grass + avg_forb + avg_bare + compaction + (1 | survey_plot), #This code will run the model until I add "+ (1 | survey_plot). This prevents pseudoreplication. 
  data = plot_habitat_summarized,
  family = binomial()
)

summary(plot_model)

##### Creating models for each habitat cover measure, still at plot level - is this necessary ? 
## Soil Compaction Model 
## Testing normality - data NOT normal 
shapiro.test(plot_habitat_summarized$compaction)
shapiro.test(log(plot_habitat_summarized$compaction))
hist(plot_habitat_summarized$compaction)

#Creating models 
compaction_model2 <- glmmTMB(log(compaction) ~ nest_present + (1|site), 
                             data = plot_habitat_summarized, 
                             family = Gamma(link = "log"))  
summary(compaction_model2) # AIC = 
summary(aov(compaction_model2)) 

compaction_model3 <- glmmTMB(compaction ~ nest_present + (1 + survey_plot|site), 
                             data = plot_habitat_summarized, 
                             family = Gamma(link = "log")) # using means of each quadrat at each nest
summary(compaction_model3) #
summary(aov(compaction_model3)) #

#Grass model

#Forb model

#Bare ground model 

#Creating dataframe with necessary categories to compare nest and no nest habitat
all_habitat2 <- all_habitat %>% 
  select(survey_plot, year, site, nest_ID, leaf_lit_cm_1, leaf_lit_cm_2, leaf_lit_cm_3, psi_avg, mean_litter_depth, quadrat_distance, proximity, per_bare, per_forb, per_grass) %>%
  mutate(
    site = as.factor(site),
    year = as.factor(year),
    survey_plot = as.factor(survey_plot)
  ) %>%
  mutate(nest_present = if_else(nest_ID == "no_nest", "no", "yes")) %>%
  mutate(nest_present = as.factor(nest_present))

## Creating models 
## Testing normality
#Grass first 
shapiro.test(all_habitat2$prop_grass)
shapiro.test(log(all_habitat2$prop_grass))
hist(all_habitat2$prop_grass)

grass_model2 <- glmmTMB(prop_grass ~ nest_present + (1|survey_plot), 
                        data = all_habitat2, 
                        family = beta_family) 


##Then, can use both nest_present and quadrat_distance to dest the differences between nest and non-nest plots and if distance from the nest impacts that relationship

full_model <- glmmTMB(
  prop_forb ~ nest_present * quadrat_distance + (1 | survey_plot),
  data = all_habitat2,
  family = beta_family()
)