library(tidyverse)
library(reshape)
library(gridExtra)
library(plyr)
library(dplyr)

Xall = c()
Xall = rbind(Xall, cbind(read.csv("results/value_decomposition_uni"), data = "uniform"))
Xall = rbind(Xall, cbind(read.csv("results/value_decomposition_neg.csv"), data = "negative"))
Xall = rbind(Xall, cbind(read.csv("results/value_decomposition_psk.csv"), data = "positive"))

Xall <- Xall %>% mutate(proportion_no_interactions = opt_nointeractions/opt) 
save(Xall, file = "results/opt_value_decomposition.RData")