### The goal of this script is to give an example of running the PDA simulation.
### Mostly this just involves running the scripts sourced below, which contain the 
### nuts-and-bolts of the simulation. The simulation consists of 2 main parts: 1)
### making the interactions between projects, 2) making portfolios using various
### heuristics. For more information on exactly how things are done, see the .R 
### files below, or see the paper.

### Ian Durbach, 18/12/2018

rm(list = ls())
library(Rglpk)

# function generating project values and costs
source("code/generateSkewedData_fn.R")

# functions for generating interactions between projects 
# see individual .R files for details of what each does
source("code/compute_selection_probs.R")
source("code/create_interdependencies.R")
source("code/compute_interdependent_BC.R")

# functions for creating various kinds of portfolios
source("code/evaluate_z.R") # evaluates the value of any portfolio
source("code/solve_portfolio.R") # computes the optimal portfolio
source("code/construct_random_portfolios.R") # constructs random portfolios
source("code/take_the_best.R") # constructs 'add-the-best' type portfolios
source("code/dominance.R") # constructs a portfolio with the "Pareto heuristic"
source("code/lex.R") # constructs portfolio with 'unit value with synergy' heuristic

# function putting it all together and running the PDA
source("code/run_one_simulation.R")

# generates 100 project values and costs datasets and save these as csv files.
for(i in 1:100){
  x <- generateUniformData(50, 0, 20)
  write.csv(x, paste("data/uniform_data_",i,".csv", sep = ""))
  x <- generateSkewedData(50, 5, 2, T)
  write.csv(x, paste("data/pos_skew_data_",i,".csv", sep = ""))
  #x <- generateSkewedData(50, 0.5, 0.8, T)
  x <- generateSkewedData(50, 5, 2, F)
  write.csv(x, paste("data/neg_skew_data_",i,".csv", sep = ""))
}

# load one of these datasets, for illustration. You would need to simulate over
# all generated datasets.
filepath = "data/pos_skew_data_"
i <- 1
print(paste("dataset:",i))
x <- read.csv(file = paste(filepath,i,".csv", sep = ""))
my_bp <- x$value # project values
my_cp <- x$cost # project costs
nproj <- nrow(x) # number of projects
nCV <- c(3) # number of cost violations heuristic encounters before terminating

# set up parameters for simulation (see paper for details)
budgets <- sum(x$cost) * round(c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),2)
relative_budgets = round(c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),2)
my_alphas <- c(0, 6)
my_gamma <- c(0, 0.5)
my_selprob <- c("equal","prop","invprop")
interaction_pool <- c(10)
random_nested = c(0, 1)

# make a data frame with all parameter combinations
pars = expand.grid(nproj= nproj, nCV = nCV, budget = budgets, selprob = my_selprob, alpha = my_alphas, random_nested = random_nested, interaction_pool = interaction_pool, gamma = my_gamma)

# run hypothetical portfolio analysis for dataset i, for each set of parameter combinations
# in 'pars' above. Here we just run the first 5 rows of pars, for illustration. Below we
# only run TWO simulations at each parameter value, again just for illustration. In a final
# simulation one must run many times at each combination.

all_res = data.frame() # set up empty data frame for results
for(irun in 1:5){
  for(nruns_per_irun in 1:2){
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
    
    res[3] = relative_budgets[which(budgets == pars$budget[irun])] # replace budget with relative budget
    all_res = rbind(all_res,c(pars[irun,], res)) # add results and pars used to get them 
  }
}
all_res
