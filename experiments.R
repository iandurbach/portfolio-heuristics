#library(Rsymphony)
#library(lpSolve)
library(Rglpk)

#setwd("/Users/iandurbach/Documents/Research/150306_PortfolioHeuristics/")

source("code/generate_project_data.r")
source("code/compute_selection_probs.r")
source("code/create_interdependencies.r")
source("code/compute_interdependent_BC.r")
source("code/solve_portfolio.r")
source("code/evaluate_z.r")
source("code/construct_random_portfolios.r")
source("code/take_the_best.r")
source("code/dominance.R")
source("code/run_one_simulation.r")


### BASE CONTEXT 1
runAllExperiments <- function(numdatasets){
  all_res = c()
  for(i in 1:numdatasets){
    print(paste("dataset:",i))
    x <- read.csv(file = paste(filepath,i,".csv", sep = ""))
    my_bp <- x$value
    my_cp <- x$cost
    nproj <- c(50)
    nCV <- c(10)
    budgets <- c(sum(x$cost)/2)
    all_res <- rbind(all_res, runSimulation(nproj, nCV, budgets, relative_budgets = c(1/2), my_bp = my_bp, my_cp = my_cp))
  }
  colnames(all_res) = c("nproj","nCV","budget","my_alpha","my_gamma","my_selprob","random_nested","interaction_pool","opt","min","rand","ttb","dom","greedynet","greedyvalue","greedycost", "mvpmax","lvpmax","rvpmax",
                        "opt_nor","min_nor","rand_nor","ttb_nor","dom_nor","greedynet_nor","greedyvalue_nor", "greedycost_nor", "mvpmax_nor", "lvpmax_nor", "rvpmax_nor")
  write.csv(all_res, paste("results/base_context_1_", suffix, ".csv", sep = ""))
  
  
  ### BASE CONTEXT 2
  all_res = c()
  for(i in 1:numdatasets){
    print(paste("dataset:",i))
    x <- read.csv(file = paste(filepath,i,".csv", sep = ""))
    my_bp <- x$value
    my_cp <- x$cost
    nproj <- c(50)
    nCV <- c(3)
    sum(x$cost)
    budgets <- c(sum(x$cost)/5,sum(x$cost)/4,sum(x$cost)/3,sum(x$cost)/2,sum(x$cost)*2/3)
    relative_budgets = round(c(0.05, 0.1, 1/5,1/4,1/3,1/2,2/3),2)
    all_res <- rbind(all_res, runSimulation(nproj, nCV, budgets, relative_budgets, my_bp = my_bp, my_cp = my_cp))
  }
  colnames(all_res) = c("nproj","nCV","budget","my_alpha","my_gamma","my_selprob","random_nested","interaction_pool","opt","min","rand","ttb","dom","greedynet","greedyvalue","greedycost", "mvpmax","lvpmax","rvpmax",
                        "opt_nor","min_nor","rand_nor","ttb_nor","dom_nor","greedynet_nor","greedyvalue_nor", "greedycost_nor", "mvpmax_nor", "lvpmax_nor", "rvpmax_nor")
  write.csv(all_res, paste("results/base_context_2_", suffix, ".csv", sep = ""))
  
  
  ### INTERACTIONS 1
  all_res <- c()
  for(i in 1:numdatasets){
    print(paste("dataset:",i))
    x <- read.csv(file = paste(filepath,i,".csv", sep = ""))
    my_bp <- x$value
    my_cp <- x$cost
    nproj <- c(50)
    nCV <- c(3)
    budgets <- c(sum(x$cost)/5,sum(x$cost)/4,sum(x$cost)/3,sum(x$cost)/2,sum(x$cost)*2/3)
    relative_budgets = round(c(0.05, 0.1, 1/5,1/4,1/3,1/2,2/3),2)
    my_alphas <- c(3)
    my_gammas <- c(0,1,2)
    selprob <- c("equal","prop","invprop")
    interaction_pool <- c(10)
    random_nested = c(1)
    all_res <- rbind(all_res, runSimulation(nproj, nCV, budgets, relative_budgets, my_alphas = my_alphas, random_nested = random_nested, my_selprob = selprob, interaction_pool = interaction_pool, my_bp = my_bp, my_cp = my_cp, my_gamma = my_gammas))
  }
  colnames(all_res) = c("nproj","nCV","budget","my_alpha","my_gamma","my_selprob","random_nested","interaction_pool","opt","min","rand","ttb","dom","greedynet","greedyvalue","greedycost", "mvpmax","lvpmax","rvpmax",
                        "opt_nor","min_nor","rand_nor","ttb_nor","dom_nor","greedynet_nor","greedyvalue_nor", "greedycost_nor", "mvpmax_nor", "lvpmax_nor", "rvpmax_nor")
  write.csv(all_res, paste("results/base_context_3a_", suffix, ".csv", sep = ""))
  
  ### INTERACTIONS 2
  all_res <- c()
  for(i in 1:numdatasets){
    print(paste("dataset:",i))
    x <- read.csv(file = paste(filepath,i,".csv", sep = ""))
    my_bp <- x$value
    my_cp <- x$cost
    nproj <- c(50)
    nCV <- c(3)
    budgets <- c(sum(x$cost)/5,sum(x$cost)/4,sum(x$cost)/3,sum(x$cost)/2,sum(x$cost)*2/3)
    relative_budgets = round(c(0.05, 0.1, 1/5,1/4,1/3,1/2,2/3),2)
    selprob <- c("equal","prop","invprop")
    interaction_pool <- c(10)
    my_alphas <- c(3)
    my_gammas <- c(0,1,2)
    random_nested = c(0)
    all_res <- rbind(all_res, runSimulation(nproj, nCV, budgets, relative_budgets, my_alphas = my_alphas, random_nested = random_nested, my_selprob = selprob, interaction_pool = interaction_pool, my_bp = my_bp, my_cp = my_cp, my_gamma = my_gammas))
  }
  colnames(all_res) = c("nproj","nCV","budget","my_alpha","my_gamma","my_selprob","random_nested","interaction_pool","opt","min","rand","ttb","dom","greedynet","greedyvalue","greedycost", "mvpmax","lvpmax","rvpmax",
                        "opt_nor","min_nor","rand_nor","ttb_nor","dom_nor","greedynet_nor","greedyvalue_nor", "greedycost_nor", "mvpmax_nor", "lvpmax_nor", "rvpmax_nor")
  write.csv(all_res, paste("results/base_context_3b_", suffix, ".csv", sep = ""))
  
  
  ### INTERACTIONS 3
  all_res <- c()
  for(i in 1:numdatasets){
    print(paste("dataset:",i))
    x <- read.csv(file = paste(filepath,i,".csv", sep = ""))
    my_bp <- x$value
    my_cp <- x$cost
    nproj <- c(50)
    nCV <- c(3)
    budgets <- c(sum(x$cost)/5,sum(x$cost)/4,sum(x$cost)/3,sum(x$cost)/2,sum(x$cost)*2/3)
    relative_budgets = round(c(0.05, 0.1, 1/5,1/4,1/3,1/2,2/3),2)
    selprob <- c("equal")
    interaction_pool <- c(10)
    my_alphas <- c(0)
    my_gammas <- c(0,1,2)
    random_nested = c(0)
    all_res <- rbind(all_res, runSimulation(nproj, nCV, budgets, relative_budgets, my_alphas = my_alphas, random_nested = random_nested, my_selprob = selprob, interaction_pool = interaction_pool, my_bp = my_bp, my_cp = my_cp, my_gamma = my_gammas))
  }
  colnames(all_res) = c("nproj","nCV","budget","my_alpha","my_gamma","my_selprob","random_nested","interaction_pool","opt","min","rand","ttb","dom","greedynet","greedyvalue","greedycost", "mvpmax","lvpmax","rvpmax",
                        "opt_nor","min_nor","rand_nor","ttb_nor","dom_nor","greedynet_nor","greedyvalue_nor", "greedycost_nor", "mvpmax_nor", "lvpmax_nor", "rvpmax_nor")
  write.csv(all_res, paste("results/base_context_3c_", suffix, ".csv", sep = ""))
}



##FUNCTION:
runSimulation <- function(nproj, nCV, budgets,relative_budgets, my_alphas = c(0), my_selprob = "equal", random_nested = 0, interaction_pool = 10, my_bp, my_cp, my_gamma = c(0)){
  pars = expand.grid(nproj= nproj, nCV = nCV, budget = budgets, selprob = my_selprob, alpha = my_alphas, random_nested = random_nested, interaction_pool = interaction_pool, gamma = my_gamma)
  
  nruns = nrow(pars) # number of parameter combinations
  nreps = 1 # number of repetitions at each parameter combination
  
  all_res = matrix(0, nrow=nreps*nruns, ncol=30)
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


#
# SKEWED DATA
#
#nCV = number of constraint violations when choosing a random or a greedy portfolio.
#
filepath = "data/pos_skew_data_"
suffix = "psk"
runAllExperiments(5)
filepath = "data/neg_skew_data_"
suffix = "neg"
runAllExperiments(5)
filepath = "data/uniform_data_"
suffix = "uni"
runAllExperiments(5)
