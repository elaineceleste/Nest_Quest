# Source - https://stackoverflow.com/a
# Posted by JasonAizkalns, modified by community. See post 'Timeline' for change history
# Retrieved 2025-11-19, License - CC BY-SA 4.0

library(tidyverse)
library(broom)

df <- data.frame(
  stringsAsFactors = FALSE,
  department = c("x", "y", "y", "y", "x", "z", "z", "x", "z"),
  gender = c("Male","Female","Male",
             "Male","Female","Female","Male","Male","Female"),
  pass = c(1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 0L)
)


df %>%
  group_by(department) %>%
  nest() %>% 
  mutate(
    chi_test = map(data, ~ chisq.test(.$gender, .$pass)),
    tidied = map(chi_test, tidy)
  ) %>% 
  unnest(tidied)

#> # A tibble: 3 x 7
#> # Groups:   department [3]
#>   department data      chi_test statistic p.value parameter method              
#>   <chr>      <list>    <list>       <dbl>   <dbl>     <int> <chr>               
#> 1 x          <tibble ~ <htest>   4.62e-32   1.00          1 Pearson's Chi-squar~
#> 2 y          <tibble ~ <htest>   4.62e-32   1.00          1 Pearson's Chi-squar~
#> 3 z          <tibble ~ <htest>   1.88e- 1   0.665         1 Pearson's Chi-squar~
#> 
chi_bombus_results <- data.frame(chi_bombus_nest_found$observed)
chi_bombus_expected <- data.frame(chi_bombus_nest_found$expected)
chi_bombus_expected <- chi_bombus_expected %>% mutate(nest_found = c(0,1))

chi_bombus_expected <- chi_bombus_expected %>% pivot_longer(!nest_found, 
                                                            names_to = 'bombus_seen', 
                                                            values_to = 'Freq')
str_replace(chi_bombus_expected$bombus_seen, '.', ' ')

chi_bombus_results <- chi_bombus_results %>% rename(nest_found = Var1,
                                                    bombus_seen = Var2,
                                                    observed = Freq)


## Code I was trying to do to get labels above bars. Did not work. 
#plot_labels <- site_summary2 %>% distinct(site) 

#plot_labels %>% mutate(nest = if_else(site_summary2$total_nests > 1, 'yes', 'no'))

#df <- data.frame(site = distinct(site_summary2, site),
#     nest = c('yes', 'yes', 'yes','yes', ' yes','yes', 'yes', 'yes' ,'no', 'no', 'no', 'no', 'no', 'no', 'no', 'no', 'no'),
#    x = c())

#Hi 9/10/2026
