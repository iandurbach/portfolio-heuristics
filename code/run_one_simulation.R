run_one_simulation = function(nproj, my_nCV, my_budget, my_alpha, my_selprob = "equal", random_nested = 0, interaction_pool = 10, my_bp = NULL, my_cp = NULL, my_gamma){  
  ################################
  ######## enter user data
  ################################
  
  # for positively related projects
  my_nint = interaction_pool    # size of subset of related projects
  my_order_int_proj = c(5,4,3,2) #c(6,5,4,3,2) # order of interdependencies between projects
  my_n_int_proj = c(2,6,8,10) #c(1,3,5,6,10) # number of interdependencies of each order
  my_use_ipp_str = random_nested   # 1 if use structured interdependencies, 0 for random (see below)
  # benefit of implementing all projects in I_k = B_k
  # B_k = bonus_add + bonus_mult * mean benefit of projects in I_k
  # NOTE: in paper, I've removed reference to multiplier -- additive effects only
  my_alpha = my_alpha # additive effect for benefits
  my_gamma = my_gamma # multiplicative effect for benefits
  my_beta = c(0,0,0,0,0,0) # additive effect for costs
  my_phi = c(0,0,0,0,0,0) # multiplicative effect for costs
  my_selprobs = my_selprob # one of "equal", "prop", "invprop"
  # for negatively related projects
  my_nint_neg = 8    # size of subset of related projects
  my_order_int_proj_neg = c(3,2) # order of interdependencies between projects
  my_n_int_proj_neg = c(4,2) # number of interdependencies of each order
  my_use_ipp_str_neg = 0   # 1 if use structured interdependencies, 0 for random (see below)
  # penalty of implementing all projects in I_k = B_k
  # B_k = bonus_add + bonus_mult * mean benefit of projects in I_k
  my_alpha_neg = c(0,0) # additive effect for benefits
  my_gamma_neg = c(0,0) # multiplicative effect for benefits
  my_beta_neg = c(0,0) # additive effect for costs
  my_phi_neg = c(0,0) # multiplicative effect for costs
  my_selprobs_neg = "equal" # one of "equal", "prop", "invprop"
  
  ################################
  ######## end user data
  ################################
  
  #x = generate_project_data(nproj = nproj, minbc = 0.008, maxbc = 0.013, mincp = 52, maxcp = 550)
  
  #my_bp = x$bp
  #my_cp = x$cp
  if(is.null(my_bp) & is.null(my_cp)){
    x = read.csv("liesio_data.csv")
    my_bp = x[,2]
    my_cp = x$cost
  }

  
  n = length(my_bp) # number of projects
  
  # generate set of projects involved in positive interdependencies
  selprobs = compute_selection_probs(selprobs = my_selprobs,bp=my_bp,cp=my_cp)
  my_starting_proj = sample(1:n,my_nint,prob=selprobs)
  #my_starting_proj
  # generate set of projects involved in negative interdependencies
  selprobs_neg = compute_selection_probs(selprobs = my_selprobs_neg,bp=my_bp,cp=my_cp)
  my_starting_proj_neg = sample(1:n,my_nint_neg,prob=selprobs_neg)
  #my_starting_proj_neg
  # generate positive project interdependencies
  my_ipp = create_interdependencies(starting_proj=my_starting_proj,
                                    n_int_proj=my_n_int_proj,
                                    order_int_proj=my_order_int_proj,
                                    use_ipp_str=my_use_ipp_str)
  #my_ipp
  # generate negative project interdependencies
  my_ipp_neg = create_interdependencies(starting_proj=my_starting_proj_neg,
                                        n_int_proj=my_n_int_proj_neg,
                                        order_int_proj=my_order_int_proj_neg,
                                        use_ipp_str=my_use_ipp_str_neg)
  #my_ipp_neg
  # compute benefits and costs of positive interdependencies
  my_BC = compute_interdependent_BC(ipp=my_ipp,
                                    bp=my_bp,
                                    cp=my_cp,
                                    alpha=my_alpha,
                                    gamma=my_gamma,
                                    beta=my_beta,
                                    phi=my_phi)
  #my_BC
  # compute benefits and costs of negative interdependencies
  my_BC_neg = compute_interdependent_BC(ipp=my_ipp_neg,
                                        bp=my_bp,
                                        cp=my_cp,
                                        alpha=my_alpha_neg,
                                         gamma=my_gamma_neg,
                                        beta=my_beta_neg,
                                        phi=my_phi_neg)
  #my_BC_neg
  ################################
  ######## models
  ################################
  # solve a MILP to get optimal portfolio
  #print("optsol")
  my_optsol = solve_portfolio(ipp=c(my_ipp,my_ipp_neg),
                              order_int_proj = c(my_order_int_proj,my_order_int_proj_neg),
                              bp  = my_bp,
                              Bi = c(my_BC$Bi,my_BC_neg$Bi),
                              cp = my_cp,
                              Ci = c(my_BC$Ci,my_BC_neg$Ci),
                              budget = my_budget)

  #my_optsol
  # solve a MILP to get nadir portfolio
  #print("nadsol")
  
  my_nadsol = solve_portfolio(ipp=c(my_ipp,my_ipp_neg),
                              order_int_proj = c(my_order_int_proj,my_order_int_proj_neg),
                              bp  = my_bp,
                              Bi = c(my_BC$Bi,my_BC_neg$Bi),
                              cp = my_cp,
                              Ci = c(my_BC$Ci,my_BC_neg$Ci),
                              budget = my_budget,
                              max = FALSE)
  #my_nadsol
  # compute mean performance of random feasible portfolio
  my_randsols = construct_random_portfolios(nRP = 1,
                                            nCV = my_nCV,
                                            ipp=c(my_ipp,my_ipp_neg),
                                            bp  = my_bp,
                                            Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                            cp = my_cp,
                                            Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                            budget = my_budget)
  #my_randsols$benefit
  #hist(my_randsols$benefit)
  #mean(my_randsols$benefit)
  # generate greedy portfolio
  my_ttb = take_the_best(nCV = my_nCV,
                         ipp=c(my_ipp,my_ipp_neg),
                         bp  = my_bp,
                         Bi = c(my_BC$Bi,my_BC_neg$Bi),
                         cp = my_cp,
                         Ci = c(my_BC$Ci,my_BC_neg$Ci),
                         budget = my_budget)
  
  greedy_netvalue = greedy_netvalue(nCV = my_nCV,
                         ipp=c(my_ipp,my_ipp_neg),
                         bp  = my_bp,
                         Bi = c(my_BC$Bi,my_BC_neg$Bi),
                         cp = my_cp,
                         Ci = c(my_BC$Ci,my_BC_neg$Ci),
                         budget = my_budget)

  mvp_max = mvp_max(nCV = my_nCV,
                    ipp=c(my_ipp,my_ipp_neg),
                    bp  = my_bp,
                    Bi = c(my_BC$Bi,my_BC_neg$Bi),
                    cp = my_cp,
                    Ci = c(my_BC$Ci,my_BC_neg$Ci),
                    budget = my_budget)
  
  lvp_max = lvp_max(nCV = my_nCV,
                    ipp=c(my_ipp,my_ipp_neg),
                    bp  = my_bp,
                    Bi = c(my_BC$Bi,my_BC_neg$Bi),
                    cp = my_cp,
                    Ci = c(my_BC$Ci,my_BC_neg$Ci),
                    budget = my_budget)
  
  rvp_max = rvp_max(nCV = my_nCV,
                    ipp=c(my_ipp,my_ipp_neg),
                    bp  = my_bp,
                    Bi = c(my_BC$Bi,my_BC_neg$Bi),
                    cp = my_cp,
                    Ci = c(my_BC$Ci,my_BC_neg$Ci),
                    budget = my_budget)
  
  greedy_value = greedy_value(nCV = my_nCV,
                                    ipp=c(my_ipp,my_ipp_neg),
                                    bp  = my_bp,
                                    Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                    cp = my_cp,
                                    Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                    budget = my_budget)
  
  greedy_cost = greedy_cost(nCV = my_nCV,
                                    ipp=c(my_ipp,my_ipp_neg),
                                    bp  = my_bp,
                                    Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                    cp = my_cp,
                                    Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                    budget = my_budget)
  #my_ttb$benefit
  #We normalize values, cost and budget.
  nor_bp = (my_bp - min(my_bp))/(max(my_bp)-min(my_bp))
  nor_cp = (my_cp - min(my_cp))/(max(my_cp)-min(my_cp))
  nor_budget = (my_budget - min(my_cp))/(max(my_cp)-min(my_cp))

  my_dom = construct_dombased_portfolios(nRP = 1,
                                       nCV = my_nCV,
                                       ipp=c(my_ipp,my_ipp_neg),
                                       nor_bp  = nor_bp,
                                       bp = my_bp,
                                       Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                       nor_cp = nor_cp,
                                       cp = my_cp,
                                       Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                       budget = my_budget)

 my_dom_eval <- apply(my_dom$final_z, FUN = evaluate_z, MARGIN = 1, ipp = c(my_ipp,my_ipp_neg), bp  = my_bp, Bi = c(my_BC$Bi,my_BC_neg$Bi), 
             cp = my_cp, Ci = c(my_BC$Ci,my_BC_neg$Ci), budget = my_budget) 
 
 #my_dom_benefit = unlist(lapply(my_dom_eval, FUN= benefit))
 #order(cueValidity(cp, bp, budget))
  
  # my_cumdom_o1 = construct_cumdombased_portfolios(nRP = 50,
  #                                                 nCV = my_nCV,
  #                                                 ipp=c(my_ipp,my_ipp_neg),
  #                                                 nor_bp  = nor_bp,
  #                                                 bp = my_bp,
  #                                                 Bi = c(my_BC$Bi,my_BC_neg$Bi),
  #                                                 nor_cp = nor_cp,
  #                                                 cp = my_cp,
  #                                                 Ci = c(my_BC$Ci,my_BC_neg$Ci),
  #                                                 budget = my_budget,
  #                                                 cueOrder = c(1,2))
  
   my_cumdom_o1 <- list(final_z=0,benefit=0,cost=0,feasible=0,g=0)
  
  # my_cumdom_o2 = construct_cumdombased_portfolios(nRP = 50,
  #                                                nCV = my_nCV,
  #                                                ipp=c(my_ipp,my_ipp_neg),
  #                                                nor_bp  = nor_bp,
  #                                                bp = my_bp,
  #                                                Bi = c(my_BC$Bi,my_BC_neg$Bi),
  #                                                nor_cp = nor_cp,
  #                                                cp = my_cp,
  #                                                Ci = c(my_BC$Ci,my_BC_neg$Ci),
  #                                                budget = my_budget,
  #                                                cueOrder = c(2,1))
  
  my_cumdom_o2 <- list(final_z=0,benefit=0,cost=0,feasible=0,g=0)
  

  # output results
  v_zopt = my_optsol$objval  # value of optimal portfolio
  v_znad = my_nadsol$objval  # value of nadir portfolio
  v_zrand = mean(my_randsols$benefit)   # mean value of random portfolio
  v_zttb = my_ttb$benefit   # value of take-the-best portfolio
  v_greedy_netvalue = greedy_netvalue$benefit# value of greedy net value portfolio
  v_dom = mean(my_dom$benefit)
  v_greedyvalue = mean(greedy_value$benefit)
  v_greedycost = mean(greedy_cost$benefit)
  v_mvpmax = mean(mvp_max$benefit)
  v_lvpmax = mean(lvp_max$benefit)
  v_rvpmax = mean(rvp_max$benefit)
  
  v_zopt
  v_znad
  v_zrand
  v_zttb
  
  # standardised outputs
  PL_zopt = 1
  PL_znad = 0
  PL_zrand = (v_zrand-v_znad) / (v_zopt - v_znad)
  PL_zttb = (v_zttb-v_znad) / (v_zopt - v_znad)
  PL_greedy_netvalue = (v_greedy_netvalue-v_znad) / (v_zopt - v_znad)
  PL_zdom = (v_dom-v_znad) / (v_zopt - v_znad)
  PL_greedyvalue = (v_greedyvalue-v_znad) / (v_zopt - v_znad)
  PL_greedycost = (v_greedycost-v_znad) / (v_zopt - v_znad)
  PL_mvpmax = (v_mvpmax-v_znad) / (v_zopt - v_znad)
  PL_lvpmax = (v_lvpmax-v_znad) / (v_zopt - v_znad)
  PL_rvpmax = (v_rvpmax-v_znad) / (v_zopt - v_znad)
  
    # collect inputs and outputs
  inputs = c(n,my_nCV,my_budget,my_alpha[1],my_gamma[1],my_selprob,random_nested, interaction_pool)
  outputs = c(v_zopt,v_znad,v_zrand,v_zttb,v_dom,v_greedy_netvalue,v_greedyvalue,v_greedycost,v_mvpmax, v_lvpmax, v_rvpmax, PL_zopt,PL_znad,PL_zrand,PL_zttb,PL_zdom,PL_greedy_netvalue,PL_greedyvalue,PL_greedycost,PL_mvpmax, PL_lvpmax, PL_rvpmax)
  return(c(inputs,outputs))
}

benefit <- function(x){
  x$benefit
}
