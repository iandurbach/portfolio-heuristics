library(tidyverse)

source("behavioural_lab/simulate_random_sample.R")
source("behavioural_lab/simulate_random_portfolio_with_removals.R")
source("behavioural_lab/find_heuristic_selections.R")
source("behavioural_lab/utils.R")

# number of simulated samples to generate

nsim <- 2000

#### task 1

# get task data
actions <- readRDS("behavioural_lab/output/task1_selection_data.Rds")
task_data <- readRDS("behavioural_lab/data/task1_project_data.Rds")

# see how many people, and how many removals per person, we want to simulate
# I do this to match the behavioural experiment here, but that's not necessary
desired_removals <- actions %>% group_by(RespID) %>% 
  summarize(total = n(), additions = sum(Action == "addition")) %>%
  ungroup() %>%
  mutate(removals = total - additions)
# length of vector is the number of simulated people, entries are n removals per person
removals <- desired_removals$removals

# simulate data
sim_data <- map(1:nsim, simulate_random_sample, removals = removals, task_data = task_data)
# add an index
for(i in 1:nsim){sim_data[[i]]$Sim_ID <- i}
# put into data frame
sim_data <- sim_data %>% reduce(rbind) %>% data.frame() %>% select(Sim_ID, everything())
# add task id
sim_data$task <- 1
# save
saveRDS(sim_data, file = "../exp_data/output/simulated_data_task1.Rds")

#### task 2

# get task data
actions <- readRDS("behavioural_lab/output/task2_selection_data.Rds")
task_data <- readRDS("behavioural_lab/data/task2_project_data.Rds")

# see how many people, and how many removals per person, we want to simulate
# I do this to match the behavioural experiment here, but that's not necessary
desired_removals <- actions %>% group_by(RespID) %>% 
  summarize(total = n(), additions = sum(Action == "addition")) %>%
  ungroup() %>%
  mutate(removals = total - additions)
# length of vector is the number of simulated people, entries are n removals per person
removals <- desired_removals$removals

# simulate data
sim_data <- map(1:nsim, simulate_random_sample, removals = removals, task_data = task_data)
# add an index
for(i in 1:nsim){sim_data[[i]]$Sim_ID <- i}
# put into data frame
sim_data <- sim_data %>% reduce(rbind) %>% data.frame() %>% select(Sim_ID, everything())
# add task id
sim_data$task <- 2
# save
saveRDS(sim_data, file = "behavioural_lab/output/simulated_data_task2.Rds")

