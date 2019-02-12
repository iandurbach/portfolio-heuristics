rm(list = ls())
library(Rglpk)
source("simulation/fns/compute_selection_probs.R")
source("simulation/fns/create_interdependencies.R")
source("simulation/fns/compute_interdependent_BC.R")
source("simulation/fns/solve_portfolio.R")
source("simulation/fns/evaluate_z.R")
source("simulation/fns/construct_random_portfolios.R")
source("simulation/fns/take_the_best.R")
source("simulation/fns/dominance.R")
source("simulation/fns/run_one_simulation.R")
source("simulation/fns/context_strategy_func.R")
source("simulation/fns/lex.R")
source("simulation/fns/generateSkewedData_fn.R")

runExperiments <- function(numdatasets){
  ### INTERACTIONS 1
  all_res <- c()
  for(i in 1:numdatasets){
    print(paste("dataset:",i))
    x <- read.csv(file = paste(filepath,i,".csv", sep = ""))
    my_bp <- x$value
    my_cp <- x$cost
    nproj <- c(50)
    nCV <- c(3)
    budgets <- sum(x$cost) * round(c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),2)
    relative_budgets = round(c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),2)
    my_alphas <- c(0, 3, 6)
    my_gammas <- c(0, 0.5, 1)
    selprob <- c("equal","prop","invprop")
    interaction_pool <- c(10)
    random_nested = c(0, 1)
    all_res <- rbind(all_res, runSimulation(nproj, nCV, budgets, relative_budgets, my_alphas = my_alphas, random_nested = random_nested, my_selprob = selprob, interaction_pool = interaction_pool, my_bp = my_bp, my_cp = my_cp, my_gamma = my_gammas))
  }
  colnames(all_res) = c("nproj","nCV","budget","my_alpha","my_gamma","my_selprob","random_nested","interaction_pool",
                        "opt","min","rand","ttb","dom","greedynet","greedyvalue","greedycost", "mvpmax","lvpmax","rvpmax", "lex", "lex3cb", "lex3c",
                        "opt_nor","min_nor","rand_nor","ttb_nor","dom_nor","greedynet_nor","greedyvalue_nor", "greedycost_nor", "mvpmax_nor", "lvpmax_nor", "rvpmax_nor", "lex_nor", "lex3cb_nor", "lex3c_nor",
                        "opt_bare","min_bare","rand_bare","ttb_bare","dom_bare","greedynet_bare","greedyvalue_bare", "greedycost_bare", "mvpmax_bare", "lvpmax_bare", "rvpmax_bare", "lex_bare","lex3cb_bare", "lex3c_bare")
  write.csv(all_res, paste("simulation/results/all_", suffix, ".csv", sep = ""))
  all_res
}

#what proportion of the optimal value is due to the interactions?
value_decomposition <- function(numdatasets){
  ### INTERACTIONS 1
  all_res <- c()
  for(i in 1:numdatasets){
    print(paste("dataset:",i))
    x <- read.csv(file = paste(filepath,i,".csv", sep = ""))
    my_bp <- x$value
    my_cp <- x$cost
    nproj <- c(50)
    nCV <- c(3)
    budgets <- sum(x$cost) * round(c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),2)
    relative_budgets = round(c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),2)
    my_alphas <- c(0, 3, 6)
    my_gammas <- c(0, 0.5, 1)
    selprob <- c("equal","prop","invprop")
    interaction_pool <- c(10)
    random_nested = c(0, 1)
    all_res <- rbind(all_res, runOptValueDecomposition(nproj, nCV, budgets, relative_budgets, my_alphas = my_alphas, random_nested = random_nested, my_selprob = selprob, interaction_pool = interaction_pool, my_bp = my_bp, my_cp = my_cp, my_gamma = my_gammas))
  }
  colnames(all_res) = c("nproj","nCV","budget","my_alpha","my_gamma","my_selprob","random_nested","interaction_pool",
                        "opt", "opt_nointeractions", "opt_interactions")
  write.csv(all_res, paste("results/value_decomposition_", suffix, ".csv", sep = ""))
  all_res
}

#How big is pareto-simple set in each stage?
dominance_analysis <- function(numdatasets){
  ### INTERACTIONS 1
  all_res <- c()
  for(i in 1:numdatasets){
    print(paste("dataset:",i))
    x <- read.csv(file = paste(filepath,i,".csv", sep = ""))
    my_bp <- x$value
    my_cp <- x$cost
    nproj <- c(50)
    nCV <- c(3)
    budgets <- sum(x$cost) * round(c(0.9),2)
    relative_budgets = round(c(0.9),2)
    my_alphas <- c(0)
    my_gammas <- c(0)
    selprob <- c("equal")
    interaction_pool <- c(10)
    random_nested = c(0)
    all_res <- rbind(all_res, runDominancePrevalenceAnalysis(nproj, nCV, budgets, relative_budgets, my_alphas = my_alphas, random_nested = random_nested, my_selprob = selprob, interaction_pool = interaction_pool, my_bp = my_bp, my_cp = my_cp, my_gamma = my_gammas))
  }
  colnames(all_res) = c("nproj","nCV","budget","my_alpha","my_gamma","my_selprob","random_nested","interaction_pool", as.character(1:50))
  write.csv(all_res, paste("results/pareto_setsize_", suffix, ".csv", sep = ""))
  all_res
}


##FUNCTION:
runSimulation <- function(nproj, nCV, budgets, relative_budgets, my_alphas = c(0), my_selprob = "equal", random_nested = 0, interaction_pool = 10, my_bp, my_cp, my_gamma = c(0)){
  pars = expand.grid(nproj= nproj, nCV = nCV, budget = budgets, selprob = my_selprob, alpha = my_alphas, random_nested = random_nested, interaction_pool = interaction_pool, gamma = my_gamma)
  
  nruns = nrow(pars) # number of parameter combinations
  nreps = 5 # number of repetitions at each parameter combination
  
  all_res = matrix(0, nrow=nreps*nruns, ncol=50)
  cnt = 1
  #Rprof(line.profiling=TRUE)
  for(irun in 1:nruns){
    for(irep in 1:nreps){
      #print(pars[irun,])
      res = run_one_simulation(nproj = pars$nproj[irun], 
                               my_nCV = pars$nCV[irun],
                               my_budget = pars$budget[irun],
                               my_alpha = c(rep(pars$alpha[irun],4)),
                               my_selprob = pars$selprob[irun],
                               random_nested = pars$random_nested[irun], 
                               interaction_pool = pars$interaction_pool[irun],
                               my_bp = my_bp,
                               my_cp = my_cp,
                               my_gamma = c(rep(pars$gamma[irun],4)))
      
      res[3] = relative_budgets[which(budgets == pars$budget[irun])] #Replace budget with relative budget
      all_res[cnt,] = res
      cnt = cnt + 1
    }
  }
  all_res = data.frame(all_res)
  return(all_res)
} 

runOptValueDecomposition <- function(nproj, nCV, budgets, relative_budgets, my_alphas = c(0), my_selprob = "equal", random_nested = 0, interaction_pool = 10, my_bp, my_cp, my_gamma = c(0)){
  pars = expand.grid(nproj= nproj, nCV = nCV, budget = budgets, selprob = my_selprob, alpha = my_alphas, random_nested = random_nested, interaction_pool = interaction_pool, gamma = my_gamma)
  
  nruns = nrow(pars) # number of parameter combinations
  nreps = 5 # number of repetitions at each parameter combination
  all_res = matrix(0, nrow=nreps*nruns, ncol=11)
  cnt = 1
  #Rprof(line.profiling=TRUE)
  for(irun in 1:nruns){
    for(irep in 1:nreps){
      #print(pars[irun,])
      context = getContext(nproj = pars$nproj[irun], 
                           my_nCV = pars$nCV[irun],
                           my_budget = pars$budget[irun],
                           my_alpha = c(rep(pars$alpha[irun],4)),
                           my_selprob = pars$selprob[irun],
                           random_nested = pars$random_nested[irun], 
                           interaction_pool = pars$interaction_pool[irun],
                           my_bp = my_bp, 
                           my_cp = my_cp, 
                           my_gamma =  c(rep(pars$gamma[irun],4)))

      opt = getPortfolio("opt", context)
      my_optsol = evaluate_z(opt$solution[1:nproj], ipp=context$ipp, bp = context$bp,  cp = context$cp, Bi = context$Bi, Ci = context$Ci, budget = context$budget, decompose = T)
      res = c(nproj = pars$nproj[irun], 
               my_nCV = pars$nCV[irun],
               my_budget = pars$budget[irun],
               my_alpha = pars$alpha[irun],
               my_gamma = pars$gamma[irun],
               my_selprob = pars$selprob[irun],
               random_nested = pars$random_nested[irun], 
               interaction_pool = pars$interaction_pool[irun],
               my_optsol$benefit,
               my_optsol$benefit_bare,
               my_optsol$benefit_interactions)
      res[3] = relative_budgets[which(budgets == pars$budget[irun])] #Replace budget with relative budget
      all_res[cnt,] = res
      cnt = cnt + 1
    }
  }
  all_res = data.frame(all_res)
  return(all_res)
}

runDominancePrevalenceAnalysis <- function(nproj, nCV, budgets, relative_budgets, my_alphas = c(0), my_selprob = "equal", random_nested = 0, interaction_pool = 10, my_bp, my_cp, my_gamma = c(0)){
  pars = expand.grid(nproj= nproj, nCV = nCV, budget = budgets, selprob = my_selprob, alpha = my_alphas, random_nested = random_nested, interaction_pool = interaction_pool, gamma = my_gamma)
  nruns = nrow(pars) # number of parameter combinations
  nreps = 5 # number of repetitions at each parameter combination
  all_res = matrix(0, nrow=nreps*nruns, ncol=8 + 50)
  cnt = 1
  #Rprof(line.profiling=TRUE)
  for(irun in 1:nruns){
    for(irep in 1:nreps){
      #print(pars[irun,])
      context = getContext(nproj = pars$nproj[irun], 
                           my_nCV = pars$nCV[irun],
                           my_budget = pars$budget[irun],
                           my_alpha = c(rep(pars$alpha[irun],4)),
                           my_selprob = pars$selprob[irun],
                           random_nested = pars$random_nested[irun], 
                           interaction_pool = pars$interaction_pool[irun],
                           my_bp = my_bp, 
                           my_cp = my_cp, 
                           my_gamma =  c(rep(pars$gamma[irun],4)))
      
      dom = getPortfolio("dom", context, calculateDomPrevalence = T)
      res = c(nproj = pars$nproj[irun], 
              my_nCV = pars$nCV[irun],
              my_budget = pars$budget[irun],
              my_alpha = pars$alpha[irun],
              my_gamma = pars$gamma[irun],
              my_selprob = pars$selprob[irun],
              random_nested = pars$random_nested[irun], 
              interaction_pool = pars$interaction_pool[irun])
      datarow = c(res,dom$pareto_counts[[1]])
      #print(58-length(datarow))
      #print(datarow)
      if(length(datarow) <= 58){
        datarow = c(datarow, rep(NA, 58-length(datarow)))
      }else{
        datarow = datarow[1:58]
      }
      all_res[cnt,] = datarow
      cnt = cnt + 1
    }
  }
  all_res = data.frame(all_res)
  return(all_res)
}


