source(file = "code/context_strategy_func.R")
library(tidyverse)
loadContext <- function(task = 1){
  if(task == 1){
    #benefits
    b = readxl::read_xlsx("../exp_data/portfolio_data.xlsx", range = "C3:L3", col_names = F)
    #costs
    c = readxl::read_xlsx("../exp_data/portfolio_data.xlsx", range = "C5:L5", col_names = F)
    #interactions
    i = readxl::read_xlsx("../exp_data/portfolio_data.xlsx", range = "C10:L19", col_names = F)
    budget = 280
    
  }else if(task == 2){
    #benefits
    b = readxl::read_xlsx("../exp_data/Task2-Experiment.xlsx", range = "C3:G3", col_names = F)
    #costs
    c = readxl::read_xlsx("../exp_data/Task2-Experiment.xlsx", range = "C5:G5", col_names = F)
    #interactions
    i = readxl::read_xlsx("../exp_data/Task2-Experiment.xlsx", range = "C10:G14", col_names = F)
    budget = 7
  }
  for(ix1 in (1:ncol(i))){
    for(ix2 in (ix1:ncol(i))){
      i[ix1,ix2] = 0
    }
  }
  i = cbind(proj1 =(1:ncol(i)), i)
  colnames(i) = c("proj1",(1:(ncol(i)-1)))
  longi = gather(i, key, value,-proj1)
  longi = subset(longi, value != 0)
  iaslist = c()
  longi$proj1 = as.numeric(longi$proj1)
  longi$key = as.numeric(longi$key)
  imatrix = data.matrix(unname(longi[,1:2]))
  for(j in 1:nrow(longi)){
    iaslist[[j]] = unname(data.frame(imatrix[j,]))
  }
  if(task == 2){#hardcoded 3 way interactions
    longi = rbind(longi, c(1, 1, 3), c(1, 1, 3))
    iaslist[[3]] = unname(data.frame(c(1,2,3)))
    iaslist[[4]] = unname(data.frame(c(2,3,4)))
  }
  context = getContext(nproj = length(b),
                       my_nCV = NULL,
                       my_budget = budget,
                       my_alpha = longi$value,
                       my_bp = unlist(unname(b)),
                       my_cp = unlist(unname(c)),
                       my_ipp = iaslist,
                       my_ipp_neg = list(),
                       my_BC_neg = c(),
                       my_gamma = rep(0, nrow(longi)),
                       my_beta = c(),
                       my_phi = c(),
                       order_int_proj = rep(2, nrow(longi)),
                       n_int_proj = rep(1,nrow(longi)))
  context$ipp = context$ipp[1:nrow(longi)]
  context$order_int_proj = context$order_int_proj[1:nrow(longi)]
  context$Bi = context$Bi[1:nrow(longi)]
  context$Ci = rep(0,nrow(longi))
  context
}
