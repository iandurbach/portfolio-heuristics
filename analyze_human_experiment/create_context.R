source(file = "code/context_strategy_func.R")
library(tidyverse)
#benefits
b = readxl::read_xlsx("../exp_data/portfolio_data.xlsx", range = "C3:L3", col_names = F)
#costs
c = readxl::read_xlsx("../exp_data/portfolio_data.xlsx", range = "C5:L5", col_names = F)
#interactions
i = readxl::read_xlsx("../exp_data/portfolio_data.xlsx", range = "C10:L19", col_names = F)
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
budget = 280

context = getContext(nproj = length(b),
                     my_nCV = NULL,
                     my_budget = budget,
                     my_alpha = longi$value,
                     my_bp = unlist(unname(b)),
                     my_cp = unlist(unname(c)),
                     my_ipp = iaslist,
                     my_gamma = rep(0, nrow(longi)),
                     my_beta = c(),
                     my_phi = c(),
                     order_int_proj = rep(2, nrow(longi)),
                     n_int_proj = rep(1,nrow(longi)))
context$ipp = context$ipp[1:nrow(longi)]
context$order_int_proj = context$order_int_proj[1:nrow(longi)]
context$Bi = context$Bi[1:nrow(longi)]
context$Ci = rep(0,nrow(longi))
