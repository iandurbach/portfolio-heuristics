# function constructs a random portfolio with a given number of project removals.
#
# adds projects at random until the budget is reached. Then removes TWO projects when 
# the budget limit is exceeded -- the offending project and one other random project. 
# Then adds projects again until budget is reached, etc. Once the required number of removals 
# have been achieved, the next budget violation results in only the offending portfolio 
# being dropped, and the selection process ends.
#
# this is just a simple way of incorporating removals into the random selection process, 
# for comparison with the behavioural experiment data.

simulate_random_portfolio_with_removals <- function(target_nremovals, costs, budget){

  # initialise
  nprojects <- length(costs)
  available_projects <- 1:nprojects
  cost_violation <- 0
  nremovals <- 0
  
  # start with empty portfolio
  my_z = rep(0, nprojects)
  
  all_actions <- data.frame(RespID = as.integer(), 
                           Action = as.character(),
                           Current_Portfolio = list(), 
                           Next_Portfolio = list())
  
  while(nremovals <= target_nremovals){
    
    # record the starting portfolio
    start_z <- my_z
    
    # choose a random project from the available set
    proj <- sample(available_projects, 1)
    proposed_z <- my_z
    proposed_z[proj] <- 1
    
    # check if would exceed budget
    cost_violation <- ifelse(sum(proposed_z * costs) > budget, 1, 0)
    
    # if the budget is not violated, add the project.
    # if budget is violated and still need removals to make the quote, don't add that project, 
    # and also remove an additional project.
    # if budget is violated and no more removals needed, don't add that project, and stop.
    # this is just one way of ensuring that we get the desired number of removals in a sensible way
    if(cost_violation == 0){
      action <- "addition"
      project_to_change <- proj
      my_z <- proposed_z
      available_projects <- which(my_z == 0)
    } else if(nremovals < target_nremovals) {
      action <- "removal"
      nremovals <- nremovals + 1
      project_to_change <- sample(which(my_z == 1), 1)
      my_z[project_to_change] <- 0
      available_projects <- which(my_z == 0)
    } else if(nremovals == target_nremovals) {
      action <- "stop"
      nremovals <- nremovals + 1
      project_to_change <- 0
      my_z <- start_z
      available_projects <- which(my_z == 0)
    }
    
    this_action <- data.frame(RespID = 1, Action = action, Projects = project_to_change, Current_Portfolio = I(list(start_z)), Next_Portfolio = I(list(my_z)))
    all_actions <- rbind(all_actions, this_action)
  }
  
  return(all_actions)
 
}

# make_random_portfolio(3, costs = costs, budget = 7)
