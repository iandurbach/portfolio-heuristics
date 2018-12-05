#library(Rsymphony)
#library(lpSolve)
library(Rglpk)

#setwd("/Users/iandurbach/Documents/Research/150306_PortfolioHeuristics/")

source("code/generate_project_data.R")
source("code/compute_selection_probs.R")
source("code/create_interdependencies.R")
source("code/compute_interdependent_BC.R")
source("code/solve_portfolio.R")
source("code/evaluate_z.R")
source("code/construct_random_portfolios.R")
source("code/take_the_best.R")
source("code/dominance.R")

################################
######## enter user data
################################

# generate bp, cp = benefit and cost of implementing project j

# ... from Liesio example
x = read.csv("liesio_data.csv")
my_bp = x[,2]
my_cp = x$cost

# ... randomly
#x = generate_project_data(nproj = 50, minbc = 0.008, maxbc = 0.013, mincp = 52, maxcp = 550)
#my_bp = x$bp
#my_cp = x$cp

my_budget = 7083   # budget constraint

# for positively related projects
my_nint = 6    # size of subset of related projects
my_order_int_proj = c(6,5,4,3,2) # order of interdependencies between projects
my_n_int_proj = c(1,3,5,6,10) # number of interdependencies of each order
my_use_ipp_str = 1   # 1 if use structured interdependencies, 0 for random (see below)
# benefit of implementing all projects in I_k = B_k
# B_k = bonus_add + bonus_mult * mean benefit of projects in I_k
# NOTE: in paper, I've removed reference to multiplier -- additive effects only
my_alpha = c(0,0,0,6,6) # additive effect for benefits
my_gamma = c(0,0,0,0,0) # multiplicative effect for benefits
my_beta = c(0,0,0,0,0) # additive effect for costs
my_phi = c(0,0,0,0,0) # multiplicative effect for costs
my_selprobs = "equal" # one of "equal", "prop", "invprop"

# for negatively related projects
my_nint_neg = 8    # size of subset of related projects
my_order_int_proj_neg = c(6,5,4,3,2) # order of interdependencies between projects
my_n_int_proj_neg = c(1,3,5,6,10) # number of interdependencies of each order
my_use_ipp_str_neg = 1   # 1 if use structured interdependencies, 0 for random (see below)
# penalty of implementing all projects in I_k = B_k
# B_k = bonus_add + bonus_mult * mean benefit of projects in I_k
my_alpha_neg = c(0,0,0,0,0) # additive effect for benefits
my_gamma_neg = c(0,0,0,0,0) # multiplicative effect for benefits
my_beta_neg = c(0,0,0,0,0) # additive effect for costs
my_phi_neg = c(0,0,0,0,0) # multiplicative effect for costs
my_selprobs_neg = "equal" # one of "equal", "prop", "invprop"

################################
######## end user data
################################

n = length(my_bp) # number of projects

# generate set of projects involved in positive interdependencies
selprobs = compute_selection_probs(selprobs = my_selprobs,bp=my_bp,cp=my_cp)
my_starting_proj = sample(1:n,my_nint,prob=selprobs)
my_starting_proj

# generate set of projects involved in negative interdependencies
selprobs_neg = compute_selection_probs(selprobs = my_selprobs_neg,bp=my_bp,cp=my_cp)
my_starting_proj_neg = sample(1:n,my_nint_neg,prob=selprobs_neg)
my_starting_proj_neg

# generate positive project interdependencies
my_ipp = create_interdependencies(starting_proj=my_starting_proj,
                                 n_int_proj=my_n_int_proj,
                                 order_int_proj=my_order_int_proj,
                                 use_ipp_str=my_use_ipp_str)
my_ipp

# generate negative project interdependencies
my_ipp_neg = create_interdependencies(starting_proj=my_starting_proj_neg,
                                  n_int_proj=my_n_int_proj_neg,
                                  order_int_proj=my_order_int_proj_neg,
                                  use_ipp_str=my_use_ipp_str_neg)
my_ipp_neg

# compute benefits and costs of positive interdependencies
my_BC = compute_interdependent_BC(ipp=my_ipp,
                                  bp=my_bp,
                                  cp=my_cp,
                                  alpha=my_alpha,
                                  gamma=my_gamma,
                                  beta=my_beta,
                                  phi=my_phi)
my_BC

# compute benefits and costs of negative interdependencies
my_BC_neg = compute_interdependent_BC(ipp=my_ipp_neg,
                                  bp=my_bp,
                                  cp=my_cp,
                                  alpha=my_alpha_neg,
                                  gamma=my_gamma_neg,
                                  beta=my_beta_neg,
                                  phi=my_phi_neg)
my_BC_neg

# solve a MILP to get optimal portfolio
my_optsol = solve_portfolio(ipp=c(my_ipp,my_ipp_neg),
                            order_int_proj = c(my_order_int_proj,my_order_int_proj_neg),
                            bp  = my_bp,
                            Bi = c(my_BC$Bi,my_BC_neg$Bi),
                            cp = my_cp,
                            Ci = c(my_BC$Ci,my_BC_neg$Ci),
                            budget = my_budget)
my_optsol

# solve a MILP to get nadir portfolio
my_nadsol = solve_portfolio(ipp=c(my_ipp,my_ipp_neg),
                            order_int_proj = c(my_order_int_proj,my_order_int_proj_neg),
                            bp  = my_bp,
                            Bi = c(my_BC$Bi,my_BC_neg$Bi),
                            cp = my_cp,
                            Ci = c(my_BC$Ci,my_BC_neg$Ci),
                            budget = my_budget,
                            max = FALSE)
my_nadsol

# generate singel random solution, evaluate (may be infeasible)
my_z = sample(c(1,0),n,replace=T)
my_1randsol = evaluate_z(z=my_z,
                        ipp=c(my_ipp,my_ipp_neg),
                        bp  = my_bp,
                        Bi = c(my_BC$Bi,my_BC_neg$Bi),
                        cp = my_cp,
                        Ci = c(my_BC$Ci,my_BC_neg$Ci),
                        budget = my_budget)
my_1randsol  

# compute mean performance of random feasible portfolio
my_randsols = construct_random_portfolios(nRP = 100,
                                          nCV = 10,
                                          ipp=c(my_ipp,my_ipp_neg),
                                          bp  = my_bp,
                                          Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                          cp = my_cp,
                                          Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                          budget = my_budget)
my_randsols$benefit
hist(my_randsols$benefit)
mean(my_randsols$benefit)

# generate take-the-best portfolio
my_ttb = take_the_best(nCV = 10,
                       ipp=c(my_ipp,my_ipp_neg),
                       bp  = my_bp,
                       Bi = c(my_BC$Bi,my_BC_neg$Bi),
                       cp = my_cp,
                       Ci = c(my_BC$Ci,my_BC_neg$Ci),
                       budget = my_budget)
my_ttb$benefit

my_dom = domBased2(nCV = 10,
                  ipp=c(my_ipp,my_ipp_neg),
                  bp  = my_bp,
                  Bi = c(my_BC$Bi,my_BC_neg$Bi),
                  cp = my_cp,
                  Ci = c(my_BC$Ci,my_BC_neg$Ci),
                  budget = my_budget)

my_dom$benefit

# output results
v_zopt = my_optsol$objval  # value of optimal portfolio
v_znad = my_nadsol$objval  # value of nadir portfolio
v_zrand = mean(my_randsols$benefit)   # mean value of random portfolio
v_zttb = my_ttb$benefit   # value of take-the-best portfolio
v_dom = my_dom$benefit
v_zopt
v_znad
v_zrand
v_zttb
v_dom


# standardised outputs
PL_zopt = 1
PL_znad = 0
PL_zrand = (v_zrand-v_znad) / (v_zopt - v_znad)
PL_zttb = (v_zttb-v_znad) / (v_zopt - v_znad)
PL_zdom = (v_dom-v_znad) / (v_zopt - v_znad)
PL_zopt
PL_znad
PL_zrand
PL_zttb
PL_zdom

