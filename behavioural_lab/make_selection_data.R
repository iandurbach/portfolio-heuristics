library(tidyverse)
library(readxl)

source("behavioural_lab/find_heuristic_selections.R")
source("behavioural_lab/utils.R")

########################################################
### For "Task 1": 5 projects with no project interactions
########################################################

task_data <- readRDS("behavioural_lab/data/task1_project_data.Rds")
responses_detail = as.data.frame(readxl::read_xlsx("behavioural_lab/data/Task1and2_responses.xlsx", sheet = "NoInt", range = "A2:G425", col_names = T))

all_actions <- data.frame(RespID = as.integer(),
                          ActionID = as.integer(),
                          Action = as.character(),
                          Projects = as.integer(),
                          Current_Portfolio = list())

respondent_IDs <- unique(responses_detail$RespID)

for(this_RespID in respondent_IDs){
  
  selections <- responses_detail %>% filter(RespID == this_RespID)
  
  for(i in 0:(nrow(selections) - 1)){
    current_portfolio <- as.numeric(selections[i, -c(1,2)])
    current_portfolio <- current_portfolio[!is.na(current_portfolio)]
    next_portfolio <- as.numeric(selections[i+1, -c(1,2)])
    next_portfolio <- next_portfolio[!is.na(next_portfolio)]
    current_portfolio_binary <- rep(0, 5) 
    next_portfolio_binary <- rep(0, 5) 
    current_portfolio_binary[current_portfolio] <- 1
    next_portfolio_binary[next_portfolio] <- 1   
    
    removal <- setdiff(current_portfolio, next_portfolio)
    addition <- setdiff(next_portfolio, current_portfolio)
    
    this_addition <- c()
    if(length(addition) > 0){
      this_addition <- expand.grid(RespID = this_RespID, 
                                   ActionID = i,
                                   Action = "addition",
                                   Projects = addition,
                                   Current_Portfolio = I(list(current_portfolio_binary)),
                                   Next_Portfolio = I(list(next_portfolio_binary)))
    }
    this_removal <- c()
    if(length(removal) > 0){
      this_removal <- expand.grid(RespID = this_RespID, 
                                  ActionID = i,
                                  Action = "removal",
                                  Projects = removal,
                                  Current_Portfolio = I(list(current_portfolio_binary)),
                                  Next_Portfolio = I(list(next_portfolio_binary)))
    }
    this_action <- rbind(this_addition, this_removal)
    all_actions <- rbind(all_actions, this_action)
  }
  
}

# check if current and next portfolios are in budget
# bit ugly: don't know how to do this with mutate so calculate first then add into df
all_actions$current_in_budget <- map(all_actions$Current_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()
all_actions$next_in_budget <- map(all_actions$Next_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()

# sometimes projects are added that violate budget constraints. This is an error in the data processing. 
# Projects can only be selected if there was enough budget (i.e. after suitable removals). The following 
# code checks for this and corrects the ordering where violations have occurred.
for(i in seq(from = 1, to = nrow(all_actions), by = 2)){
  
  this_action <- all_actions[i, ]
  next_action <- all_actions[i + 1, ]
  
  if((this_action$RespID == next_action$RespID) &    # must be same respondent
     (this_action$Action == "addition") & (this_action$next_in_budget == 0) &  # adds an infeasible project
     (next_action$Action == "removal") & # then frees up budget by removing one
     (this_action$Projects != next_action$Projects)){ # and its not the same project
    
    # swap around the order of removal and addition
    all_actions$Action[i] <- next_action$Action
    all_actions$Projects[i] <- next_action$Projects
    all_actions$Next_Portfolio[[i]] <- all_actions$Current_Portfolio[[i]]
    all_actions$Next_Portfolio[[i]][next_action$Projects] <- 0
    
    all_actions$Action[i+1] <- this_action$Action
    all_actions$Projects[i+1] <- this_action$Projects
    all_actions$Current_Portfolio[[i+1]] <- all_actions$Next_Portfolio[[i]]
    all_actions$Next_Portfolio[[i+1]] <- all_actions$Current_Portfolio[[i+1]]
    all_actions$Next_Portfolio[[i+1]][this_action$Projects] <- 1
    
  }
}
# redo budget checks
all_actions$current_in_budget <- map(all_actions$Current_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()
all_actions$next_in_budget <- map(all_actions$Next_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()

# repeat the procedure, because sometimes a project is followed by TWO removals before it is within budget
# could repeat this several times but two should cover 99% of cases
for(i in 1:nrow(all_actions)){
  
  this_action <- all_actions[i, ]
  next_action <- all_actions[i + 1, ]
  
  if((this_action$RespID == next_action$RespID) & 
     (this_action$Action == "addition") & (this_action$next_in_budget == 0) & 
     (next_action$Action == "removal") & 
     (this_action$Projects != next_action$Projects)){
    
    # swap around the order of removal and addition
    all_actions$Action[i] <- next_action$Action
    all_actions$Projects[i] <- next_action$Projects
    all_actions$Next_Portfolio[[i]] <- all_actions$Current_Portfolio[[i]]
    all_actions$Next_Portfolio[[i]][next_action$Projects] <- 0
    
    all_actions$Action[i+1] <- this_action$Action
    all_actions$Projects[i+1] <- this_action$Projects
    all_actions$Current_Portfolio[[i+1]] <- all_actions$Next_Portfolio[[i]]
    all_actions$Next_Portfolio[[i+1]] <- all_actions$Current_Portfolio[[i+1]]
    all_actions$Next_Portfolio[[i+1]][this_action$Projects] <- 1
    
  }
}
# redo budget checks
all_actions$current_in_budget <- map(all_actions$Current_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()
all_actions$next_in_budget <- map(all_actions$Next_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()

##

get_heuristics <- map(all_actions$Current_Portfolio, find_heuristic_selections, task_data = task_data) %>% 
  reduce(rbind) %>% 
  data.frame()

all_actions <- all_actions %>% cbind(get_heuristics)
all_actions$task <- 1

saveRDS(all_actions, "behavioural_lab/output/task1_selection_data.Rds")

########################################################
### For "Task 2": 5 projects with project interactions
########################################################

task_data <- readRDS("behavioural_lab/data/task2_project_data.Rds")
responses_detail = as.data.frame(readxl::read_xlsx("behavioural_lab/data/Task1and2_responses.xlsx", sheet = "WithInt", range = "A2:G559", col_names = T))

all_actions <- data.frame(RespID = as.integer(),
                          ActionID = as.integer(),
                          Action = as.character(),
                          Projects = as.integer(),
                          Current_Portfolio = list())

respondent_IDs <- unique(responses_detail$RespID)

for(this_RespID in respondent_IDs){

  selections <- responses_detail %>% filter(RespID == this_RespID)
  
  for(i in 0:(nrow(selections) - 1)){
    current_portfolio <- as.numeric(selections[i, -c(1,2)])
    current_portfolio <- current_portfolio[!is.na(current_portfolio)]
    next_portfolio <- as.numeric(selections[i+1, -c(1,2)])
    next_portfolio <- next_portfolio[!is.na(next_portfolio)]
    current_portfolio_binary <- rep(0, 5) 
    next_portfolio_binary <- rep(0, 5) 
    current_portfolio_binary[current_portfolio] <- 1
    next_portfolio_binary[next_portfolio] <- 1   
    
    removal <- setdiff(current_portfolio, next_portfolio)
    addition <- setdiff(next_portfolio, current_portfolio)
    
    this_addition <- c()
    if(length(addition) > 0){
      this_addition <- expand.grid(RespID = this_RespID, 
                                  ActionID = i,
                                  Action = "addition",
                                  Projects = addition,
                                  Current_Portfolio = I(list(current_portfolio_binary)),
                                  Next_Portfolio = I(list(next_portfolio_binary)))
    }
    this_removal <- c()
    if(length(removal) > 0){
      this_removal <- expand.grid(RespID = this_RespID, 
                                 ActionID = i,
                                 Action = "removal",
                                 Projects = removal,
                                 Current_Portfolio = I(list(current_portfolio_binary)),
                                 Next_Portfolio = I(list(next_portfolio_binary)))
    }
    this_action <- rbind(this_addition, this_removal)
    all_actions <- rbind(all_actions, this_action)
  }
  
}

# check if current and next portfolios are in budget
# bit ugly: don't know how to do this with mutate so calculate first then add into df
all_actions$current_in_budget <- map(all_actions$Current_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()
all_actions$next_in_budget <- map(all_actions$Next_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()

# sometimes projects are added that violate budget constraints. This is an error in the data processing. 
# Projects can only be selected if there was enough budget (i.e. after suitable removals). The following 
# code checks for this and corrects the ordering where violations have occurred.
for(i in seq(from = 1, to = nrow(all_actions), by = 2)){
  
  this_action <- all_actions[i, ]
  next_action <- all_actions[i + 1, ]
  
  if((this_action$RespID == next_action$RespID) &    # must be same respondent
     (this_action$Action == "addition") & (this_action$next_in_budget == 0) &  # adds an infeasible project
     (next_action$Action == "removal") & # then frees up budget by removing one
     (this_action$Projects != next_action$Projects)){ # and its not the same project
    
    # swap around the order of removal and addition
    all_actions$Action[i] <- next_action$Action
    all_actions$Projects[i] <- next_action$Projects
    all_actions$Next_Portfolio[[i]] <- all_actions$Current_Portfolio[[i]]
    all_actions$Next_Portfolio[[i]][next_action$Projects] <- 0
    
    all_actions$Action[i+1] <- this_action$Action
    all_actions$Projects[i+1] <- this_action$Projects
    all_actions$Current_Portfolio[[i+1]] <- all_actions$Next_Portfolio[[i]]
    all_actions$Next_Portfolio[[i+1]] <- all_actions$Current_Portfolio[[i+1]]
    all_actions$Next_Portfolio[[i+1]][this_action$Projects] <- 1
    
  }
}
# redo budget checks
all_actions$current_in_budget <- map(all_actions$Current_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()
all_actions$next_in_budget <- map(all_actions$Next_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()

# repeat the procedure, because sometimes a project is followed by TWO removals before it is within budget
# could repeat this several times but two should cover 99% of cases
for(i in 1:nrow(all_actions)){
  
  this_action <- all_actions[i, ]
  next_action <- all_actions[i + 1, ]
  
  if((this_action$RespID == next_action$RespID) & 
     (this_action$Action == "addition") & (this_action$next_in_budget == 0) & 
     (next_action$Action == "removal") & 
     (this_action$Projects != next_action$Projects)){
    
    # swap around the order of removal and addition
    all_actions$Action[i] <- next_action$Action
    all_actions$Projects[i] <- next_action$Projects
    all_actions$Next_Portfolio[[i]] <- all_actions$Current_Portfolio[[i]]
    all_actions$Next_Portfolio[[i]][next_action$Projects] <- 0
    
    all_actions$Action[i+1] <- this_action$Action
    all_actions$Projects[i+1] <- this_action$Projects
    all_actions$Current_Portfolio[[i+1]] <- all_actions$Next_Portfolio[[i]]
    all_actions$Next_Portfolio[[i+1]] <- all_actions$Current_Portfolio[[i+1]]
    all_actions$Next_Portfolio[[i+1]][this_action$Projects] <- 1
    
  }
}
# redo budget checks
all_actions$current_in_budget <- map(all_actions$Current_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()
all_actions$next_in_budget <- map(all_actions$Next_Portfolio, in_budget, costs = task_data$costs, task_data$budget) %>% unlist()

##

get_heuristics <- map(all_actions$Current_Portfolio, find_heuristic_selections, task_data = task_data) %>% 
  reduce(rbind) %>% 
  data.frame()

all_actions <- all_actions %>% cbind(get_heuristics)

all_actions$task <- 2

saveRDS(all_actions, "behavioural_lab/output/task2_selection_data.Rds")

