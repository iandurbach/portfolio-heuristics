library(tidyverse)
library(reshape)
library(gridExtra)

renameModels <- function(x)
  revalue(x, c("opt_nor"="Optimal", "min_nor" = "Nadir", "ttb_nor" = "Greedy", "greedy_net_nor"="Greedy Net", "dom_nor" = "Heuristic", "rand_nor" = "Random", "greedynet_nor" = "greedynet", "greedyvalue_nor" = "greedyvalue", "greedycost_nor" = "greedycost", "opt"="Optimal", "min" = "Nadir", "ttb" = "Greedy", "dom" = "Heuristic","greedy_netvalue"="Greedy Net", "rand" = "Random", "mvpmax" = "mvp", "lvpmax" = "lvp", "mvpmax_nor" = "mvp", "lvpmax_nor" = "lvp", "rvpmax" = "rvp", "rvpmax_nor" = "rvp"))

se <- function(x)
  se=sd(x)/sqrt(length(x))

Xall = c()
Xall = rbind(Xall, cbind(read.csv("base_context_3a_neg.csv"), data = "negative"))
Xall = rbind(Xall, cbind(read.csv("base_context_3b_neg.csv"), data = "negative"))
Xall = rbind(Xall, cbind(read.csv("base_context_3c_neg.csv"), data = "negative"))
Xall = rbind(Xall, cbind(read.csv("base_context_3a_psk.csv"), data = "positive"))
Xall = rbind(Xall, cbind(read.csv("base_context_3b_psk.csv"), data = "positive"))
Xall = rbind(Xall, cbind(read.csv("base_context_3c_psk.csv"), data = "positive"))
Xall = rbind(Xall, cbind(read.csv("base_context_3a_uni.csv"), data = "uniform"))
Xall = rbind(Xall, cbind(read.csv("base_context_3b_uni.csv"), data = "uniform"))
Xall = rbind(Xall, cbind(read.csv("base_context_3c_uni.csv"), data = "uniform"))

normalized <- Xall[c(1,2,3,4,5,6,7,8,20,21,22,23,24,25,26,27,28,29,30, 31)]
normalized <- melt(normalized, id = c("X", "nproj", "nCV", "budget", "my_alpha","my_selprob","random_nested", "interaction_pool", "data"))

absolute <- Xall[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, 17, 18,19, 31)]#Absolute values
absolute <- melt(absolute, id = c("X", "nproj", "nCV", "budget", "my_alpha","my_selprob","random_nested", "interaction_pool", "data"))

dat <- rbind(mutate(normalized, normalized = "Normalized"), mutate(absolute, normalized = "Absolute"))
dat$budget <- round(dat$budget, 2)
dat$variable<- renameModels(dat$variable)
dat$my_alpha = revalue(factor(dat$my_alpha), c("0"="No Interactions", "3"="With Interactions"))
dat$random_nested = revalue(factor(dat$random_nested), c("0"="Random", "1"="Nested"))
dat$my_selprob = revalue(factor(dat$my_selprob), c("1"="Neutral", "2"="Goodish", "3"="Poorish"))

save(dat, file = "results/dat.RData")