#### Basic reformatting of results for later analysis and graphing

library(tidyverse)
library(forcats)

load("results/dat.RData")

options(dplyr.print_max = 1e4)


# normalize data so that Opt = 100, Random = 0
dat <- dat %>% 
  filter(normalized == "Absolute") %>%
  group_by(X, data) %>% 
  mutate(normvalue = (value - value[variable == "Random"])/(value[variable == "Optimal"] - value[variable == "Random"]))

# remember to ungroup 
dat <- dat %>% ungroup()

# rename some variable labels for plots
dat <- filter(dat, my_alpha == "No Interactions" | 
                my_gamma == "No Multiplicative Interactions") %>% 
  mutate(my_alpha = fct_recode(my_alpha,
                               "No Interactions" = "No Interactions",
                               "Small Interactions" = "Low Interactions",
                               "Large Interactions" = "High Interactions")) %>% 
  mutate(my_gamma = fct_recode(my_gamma,
                               "No Interactions" = "No Multiplicative Interactions",
                               "Small Interactions" = "Low Multiplicative Interactions",
                               "Large Interactions" = "High Multiplicative Interactions")) %>%
  mutate(any_ints = ifelse((my_alpha == "No Interactions") & (my_gamma == "No Interactions"), 
                           "No Interactions","With Interactions")) %>%
  mutate(my_selprob = fct_recode(my_selprob,
                                 "Random" = "Neutral",
                                 "Good projects" = "Goodish",
                                 "Poor projects" = "Poorish"))

dat$variable <- factor(dat$variable,
                       levels=c("greedynet","mvp","lvp","rvp","Greedy","greedyvalue",
                                "greedycost","Heuristic","Random","Optimal","Nadir"))

dat <- mutate(dat, heuristic = fct_recode(variable,
                                          "AtB" = "greedynet", 
                                          "AtB-mv" = "mvp",
                                          "AtB-lv" = "lvp",
                                          "AtB-rv" = "rvp",
                                          "AtB-myo" = "Greedy",
                                          "AtV" = "greedyvalue",
                                          "AtC" = "greedycost",
                                          "ND" = "Heuristic",
                                          "Random" = "Random",
                                          "Opt" = "Optimal",
                                          "Nadir" = "Nadir"))

save(dat, file = "results/processed_dat.RData")
