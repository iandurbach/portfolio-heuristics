library(tidyverse)
library(reshape)
library(gridExtra)
library(plyr)
library(dplyr)


renameModels <- function(x)
  revalue(x, c("opt_nor"="Optimal", "min_nor" = "Nadir", "ttb_nor" = "Greedy", "dom_nor" = "Heuristic", "rand_nor" = "Random", "greedynet_nor" = "greedynet", "greedyvalue_nor" = "greedyvalue", "greedycost_nor" = "greedycost", "lex_nor" = "lex", "lex3c_nor" = "lex3c", "lex3cb_nor" = "lex3cb",
               "opt"="Optimal", "min" = "Nadir", "ttb" = "Greedy", "dom" = "Heuristic", "rand" = "Random", "mvpmax" = "mvp", "lvpmax" = "lvp", "mvpmax_nor" = "mvp", "lvpmax_nor" = "lvp", "rvpmax" = "rvp", "rvpmax_nor" = "rvp", "lex" = "lex", "lex3c" = "lex3c", "lex3cb" = "lex3cb",
               "opt_bare"="Optimal", "min_bare" = "Nadir", "ttb_bare" = "Greedy", "dom_bare" = "Heuristic", "rand_bare" = "Random", "greedynet_bare" = "greedynet", "greedyvalue_bare" = "greedyvalue", "greedycost_bare" = "greedycost", "lex_bare" = "lex", "lex3c_bare" = "lex3c", "lex3cb_bare" = "lex3cb",
               "mvpmax_bare" = "mvp", "lvpmax_bare" = "lvp", "rvpmax_bare" = "rvp", "rvpmax_bare" = "rvp"))

Xall = c()
#Xall = rbind(Xall, cbind(read.csv("results/base_context_3a_neg.csv"), data = "negative"))
#Xall = rbind(Xall, cbind(read.csv("results/base_context_3b_neg.csv"), data = "negative"))
#Xall = rbind(Xall, cbind(read.csv("results/base_context_3c_neg.csv"), data = "negative"))
#Xall = rbind(Xall, cbind(read.csv("results/base_context_3a_psk.csv"), data = "positive"))
#Xall = rbind(Xall, cbind(read.csv("results/base_context_3b_psk.csv"), data = "positive"))
#Xall = rbind(Xall, cbind(read.csv("results/base_context_3c_psk.csv"), data = "positive"))
#Xall = rbind(Xall, cbind(read.csv("results/base_context_3a_uni.csv"), data = "uniform"))
#Xall = rbind(Xall, cbind(read.csv("results/base_context_3b_uni.csv"), data = "uniform"))
#Xall = rbind(Xall, cbind(read.csv("results/base_context_3c_uni.csv"), data = "uniform"))
Xall = rbind(Xall, cbind(read.csv("results/all_uni.csv"), data = "uniform"))
Xall = rbind(Xall, cbind(read.csv("results/all_psk.csv"), data = "negative"))
Xall = rbind(Xall, cbind(read.csv("results/all_neg.csv"), data = "positive"))

normalized <- Xall[c(1,2,3,4,5,6,7,8,9,24,25,26,27,28,29,30,31,32,33,34,35,36,37,52)]
normalized <- melt(normalized, id = c("X", "nproj", "nCV", "budget", "my_alpha", "my_gamma","my_selprob", "random_nested", "interaction_pool", "data"))

absolute <- Xall[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,52)]#Absolute values
absolute <- melt(absolute, id = c("X", "nproj", "nCV", "budget", "my_alpha", "my_gamma","my_selprob","random_nested", "interaction_pool", "data"))

bare <- Xall[c(1,2,3,4,5,6,7,8,9,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52)]#Bare values
bare <- melt(bare, id = c("X", "nproj", "nCV", "budget", "my_alpha", "my_gamma","my_selprob","random_nested", "interaction_pool", "data"))

dat <- rbind(mutate(normalized, normalized = "Normalized"), mutate(absolute, normalized = "Absolute"), mutate(bare, normalized = "Bare"))
dat$normalized = factor(dat$normalized)
dat$budget <- round(dat$budget, 2)
dat$variable<- renameModels(dat$variable)
dat$my_alpha = revalue(factor(dat$my_alpha), c("0"="No Interactions", "3"="Low Interactions", "6"="High Interactions"))
dat$my_gamma = revalue(factor(dat$my_gamma), c("0"="No Multiplicative Interactions", "0.5"="Low Multiplicative Interactions", "1"="High Multiplicative Interactions"))
dat$random_nested = revalue(factor(dat$random_nested), c("0"="Random", "1"="Nested"))
dat$my_selprob = revalue(factor(dat$my_selprob), c("1"="Neutral", "2"="Goodish", "3"="Poorish"))

save(dat, file = "results/dat.RData")