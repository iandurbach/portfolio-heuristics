find_heuristic_selections <- function(current_portfolio, task_data){
  
  # load data
  benefits <- task_data$benefits
  costs <- task_data$costs
  budget <- task_data$budget
  int_subsets <- task_data$int_subsets
  val_int_subsets <- task_data$val_int_subsets
  
  maxcost <- max(costs)
  
  projects_in <- 1:(length(current_portfolio)) * current_portfolio
  projects_in <- projects_in[projects_in > 0]
  
  # current portfolio marginal value
  current_portfolio_marginal_value <- sum(benefits * current_portfolio)
  
  # current portfolio interaction value
  current_portfolio_interactions <- unlist(map2(rep(list(projects_in), length(int_subsets)), int_subsets, ~prod(.y %in% .x)))
  current_portfolio_interaction_value <- sum(current_portfolio_interactions * val_int_subsets)
  
  # current portfolio total value
  current_portfolio_full_value <- current_portfolio_marginal_value + current_portfolio_interaction_value
  
  # current portfolio interaction values (mvp only)
  
  # identify most valuable project by computing the decrease in value if dropped each project
  decrease_portfolio_full_value <- c()
  for(i in 1:5){
    # drop a project
    dropped_portfolio <- current_portfolio
    dropped_portfolio[i] <- 0
    dropped_projects_in <- 1:5 * dropped_portfolio
    dropped_projects_in <- dropped_projects_in[dropped_projects_in > 0]
    # new portfolio value
    dropped_portfolio_marginal_value <- sum(benefits * dropped_portfolio)
    dropped_portfolio_interactions <- unlist(map2(rep(list(dropped_projects_in),4), int_subsets, ~prod(.y %in% .x)))
    dropped_portfolio_interaction_value <- sum(dropped_portfolio_interactions * val_int_subsets)
    dropped_portfolio_full_value <- dropped_portfolio_marginal_value + dropped_portfolio_interaction_value
    decrease_portfolio_full_value[i] <- current_portfolio_full_value - dropped_portfolio_full_value
  }
  mvp <- sort(decrease_portfolio_full_value, decreasing = TRUE, index.return = TRUE)$ix[1]
  
  # current portfolio cost
  current_portfolio_cost <- sum(costs * current_portfolio)
  
  unit_value <- c()
  value_only <- c()
  cost_only <- c()
  added_value <- c()
  added_value_mvp <- c()
  for(i in 1:5){
    
    # already in?
    already_in <- (current_portfolio[i] == 1)
    
    # add project i
    proposed_portfolio <- current_portfolio
    proposed_portfolio[i] <- 1
    
    # new portfolio value
    proposed_portfolio_marginal_value <- sum(benefits * proposed_portfolio)
    
    # new portfolio interaction value
    proposed_projects_in <- 1:5 * proposed_portfolio
    proposed_projects_in <- proposed_projects_in[proposed_projects_in > 0]
    proposed_portfolio_interactions <- unlist(map2(rep(list(proposed_projects_in),4), int_subsets, ~prod(.y %in% .x)))
    proposed_portfolio_interaction_value <- sum(proposed_portfolio_interactions * val_int_subsets)
    
    # new portfolio interaction value (mvp only)
    new_int_subsets <-  proposed_portfolio_interactions - current_portfolio_interactions
    mvp_in_subset <- unlist(map2(rep(list(mvp),4), int_subsets, ~prod(.x %in% .y)))
    proposed_portfolio_interactions_mvp <- current_portfolio_interactions + (new_int_subsets * mvp_in_subset)
    proposed_portfolio_interaction_value_mvp <- sum(proposed_portfolio_interactions_mvp * val_int_subsets)
    
    # new portfolio cost
    proposed_portfolio_cost <- sum(costs * proposed_portfolio)
    
    # would exceed budget to include?
    not_feasible <- (proposed_portfolio_cost > budget)
    
    # unit value
    unit_value[i] <- ifelse(not_feasible, 0, (proposed_portfolio_marginal_value - current_portfolio_marginal_value) / costs[i])
    
    # highest value
    value_only[i] <- ifelse(not_feasible, 0, (proposed_portfolio_marginal_value - current_portfolio_marginal_value))
    
    # lowest cost
    cost_only[i] <- ifelse(not_feasible|already_in, 1000 * maxcost, costs[i])
    
    # added value
    proposed_portfolio_full_value <- proposed_portfolio_marginal_value + proposed_portfolio_interaction_value - current_portfolio_full_value
    added_value[i] <- ifelse(not_feasible, 0, proposed_portfolio_full_value / costs[i])
    
    # added value (mvp)
    proposed_portfolio_full_value_mvp <- proposed_portfolio_marginal_value + proposed_portfolio_interaction_value_mvp - current_portfolio_full_value
    added_value_mvp[i] <- ifelse(not_feasible, 0, proposed_portfolio_full_value_mvp / costs[i])
    
  }
  
  pick_unit_value <- which(unit_value == max(unit_value))
  pick_value_only <- which(value_only == max(value_only))
  pick_cost_only <- which(cost_only == min(cost_only))
  pick_added_value <- which(added_value == max(added_value))
  pick_added_value_mvp <- which(added_value_mvp == max(added_value_mvp))
  
  return(list(unitv = pick_unit_value, value = pick_value_only, cost = pick_cost_only,
              addedv = pick_added_value, addedv_mvp = pick_added_value_mvp))
}