# simulates portfolio additions and removals from a hypothetical sample of people who choose
# totally randomly. The number of people in the sample is given by the length of the "removals"
# vector and the entries in that vector give the number of removals each person makes. See 
# "make_random_portfolio_with_removals.R" for details of random selection, in particular how
# the removals are simulated.

simulate_random_sample <- function(removals, task_data, ...){
  
  benefits <- task_data$benefits
  costs <- task_data$costs
  budget <- task_data$budget
  int_subsets <- task_data$int_subsets
  val_int_subsets <- task_data$val_int_subsets
  
  # make random portfolios for each simulated respondent (same number as in sample)
  random_actions <- map(removals, simulate_random_portfolio_with_removals, costs = costs, budget = budget)
  
  # allocate each simulated respondent an ID
  for(i in 1:length(random_actions)){
    random_actions[[i]]$RespID <- i
  }
  
  # put into data frame
  random_actions <- random_actions %>% reduce(rbind) %>% data.frame() %>% filter(Action != "stop")
  
  # for each selection (i.e. each current portfolio in the data frame) work out which project each heuristic would 
  # have selected
  get_heuristics <- map(random_actions$Current_Portfolio, find_heuristic_selections, task_data = task_data) %>% 
    reduce(rbind) %>% 
    data.frame()
  
  # check if current and next portfolios are in budget
  # bit ugly: don't know how to do this with mutate so calculate first then add into df
  random_actions$current_in_budget <- map(random_actions$Current_Portfolio, in_budget, costs = costs, budget = budget) %>% unlist()
  random_actions$next_in_budget <- map(random_actions$Next_Portfolio, in_budget, costs = costs, budget = budget) %>% unlist()
  
  # combine actions with heuristic choices
  random_actions <- random_actions %>% cbind(get_heuristics)
  
  return(random_actions)
  
}

# test
# simulate_random_sample(c(1,3), task1_data)