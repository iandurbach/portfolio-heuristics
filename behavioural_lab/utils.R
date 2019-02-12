# helper functions for lab experiment

# function to see if portfolio is in budget
in_budget <- function(portfolio, costs, budget){
  return(ifelse(sum(portfolio * costs) > budget, 0, 1))
}

# function to compute portfolio value
portfolio_benefit <- function(current_portfolio, task_data){
  
  benefits <- task_data$benefits
  int_subsets <- task_data$int_subsets
  val_int_subsets <- task_data$val_int_subsets
  
  projects_in <- 1:(length(current_portfolio)) * current_portfolio
  projects_in <- projects_in[projects_in > 0]
  
  # current portfolio marginal value
  current_portfolio_marginal_value <- sum(benefits * current_portfolio)
  
  # current portfolio interaction value
  current_portfolio_interactions <- unlist(map2(rep(list(projects_in), length(int_subsets)), int_subsets, ~prod(.y %in% .x)))
  current_portfolio_interaction_value <- sum(current_portfolio_interactions * val_int_subsets)
  
  # current portfolio total value
  current_portfolio_full_value <- current_portfolio_marginal_value + current_portfolio_interaction_value
  
  return(current_portfolio_full_value)
}

# function to compute portfolio cost
portfolio_cost <- function(portfolio, costs){
  return(sum(portfolio * costs))
}

# convert binary string representation of portfolio to an integer one (for tables)
bin_to_int_portfolio <- function(portfolio){
  x <- unlist(str_split(portfolio, "", nchar(portfolio)))
  return(paste0("[", unlist(str_c(which(x == 1), collapse = ",")), "]"))
}