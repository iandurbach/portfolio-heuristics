
library(Rglpk)

source("code/generate_project_data.R")
source("code/compute_selection_probs.R")
source("code/create_interdependencies.R")
source("code/compute_interdependent_BC.R")
source("code/solve_portfolio.R")
source("code/evaluate_z.R")
source("code/construct_random_portfolios.R")
source("code/take_the_best.R")
source("code/dominance.R")
source("code/run_one_simulation.R")
source("code/context_strategy_func.R")


#Run simple porftolio

value = c(1,1,2,2,5)
cost = c(1,2,3,4,2)
plot(value, cost)

interactions = c()
interactions[[1]] = matrix(c(1,2,3,2,3,4), ncol = 2)
interactions[[2]] = matrix(c(1,2,1,3), ncol = 2)

interaction_additive_value = c(3,3,3,3)
interaction_multiplicative_value = c(0,0,0,0)

interaction_additive_cost = c(0,0,0,0)
interaction_multiplicative_cost = c(0,0,0,0)

context = getContext(nproj = 5, 
                     my_nCV = 3, 
                     my_budget = 7, 
                     my_alpha = interaction_additive_value, 
                     my_gamma = interaction_multiplicative_value,
                     my_beta = interaction_additive_cost,
                     my_phi = interaction_multiplicative_cost,
                     interaction_pool = 5, 
                     my_bp = value, 
                     my_cp = cost, 
                     my_ipp = interactions,
                     my_ipp_neg = interactions,
                     order_int_proj = c(3,2))

opt = getPortfolio("opt", context)
nad = getPortfolio("nad", context)
random = getPortfolio("random", context)
atb = getPortfolio("atb", context)
atb_full = getPortfolio("atb_full", context)
atb_mvp = getPortfolio("atb_mvp", context)
atb_lvp = getPortfolio("atb_lvp", context)
for (i in 1:100) print(getPortfolio("atb_rvp", context))
for (i in 1:100) print(getPortfolio("atv", context)$benefit)

atv = getPortfolio("atv", context)
atv
atc = getPortfolio("atc", context)
atc
for (i in 1:100) {
  dom = getPortfolio("dom", context)
  print(dom$final_z)
  print(dom$benefit)
}

cost / value
cost / c(4,4,2,5,5)
