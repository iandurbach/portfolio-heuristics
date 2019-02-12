# infers consistency with heuristic use from data created with "make_simulated_data.R"
# see section 6.2 of paper

library(tidyverse)

# load data
random_actions_t1 <- readRDS("behavioural_lab/output/simulated_data_task1.Rds")
random_actions_t2 <- readRDS("behavioural_lab/output/simulated_data_task2.Rds")
# combine tasks
random_actions <- rbind(random_actions_t1, random_actions_t2)

# binary indicator of whether project chosen is consistent with each heuristic
# note does NOT mean they ARE using the heuristic, often heuristics overlap
additions_with_heuristics <- random_actions %>% 
  filter(Action == "addition") %>%
  rowwise() %>%
  mutate(is_unitv = Projects %in% unitv,
         is_value = Projects %in% value,
         is_cost = Projects %in% cost,
         is_addedv = Projects %in% addedv,
         is_addedv_mvp = FALSE,
         #is_addedv_mvp = Projects %in% addedv_mvp,
         is_other = !any(is_unitv, is_value, is_cost, is_addedv, is_addedv_mvp)) %>%
  #is_other = TRUE) %>%
  ungroup()

# in task 1 unitv and addedv are equivalent, so remove addedv from task 1
additions_with_heuristics$is_addedv <- ifelse(additions_with_heuristics$task == 1, FALSE,
                                               additions_with_heuristics$is_addedv)

# where heuristics overlap, assign a weight of 1 / (number of overlapping h's) to each one
additions_with_heuristics <- additions_with_heuristics %>% 
  mutate(nposs_heurs = is_unitv + is_value + is_cost + is_addedv + is_addedv_mvp + is_other,
         w_unitv = ifelse(nposs_heurs == 0, 0, is_unitv / nposs_heurs),
         w_value = ifelse(nposs_heurs == 0, 0, is_value / nposs_heurs),
         w_cost = ifelse(nposs_heurs == 0, 0, is_cost / nposs_heurs),
         w_addedv = ifelse(nposs_heurs == 0, 0, is_addedv / nposs_heurs),
         w_addedv_mvp = ifelse(nposs_heurs == 0, 0, is_addedv_mvp / nposs_heurs),
         w_other = ifelse(nposs_heurs == 0, 0, is_other / nposs_heurs))

saveRDS(additions_with_heuristics, file = "behavioural_lab/output/results_simulated_random_selections.Rds")

