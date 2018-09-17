library("ggplot2")
library("reshape")
library("plyr")
library("dplyr")
library("tidyr")
library("gridExtra")

load("results/dat.RData")

se <- function(x)
  se=sd(x)/sqrt(length(x))

#############################################
#### With vs Without Interactions ####
uniform <- subset(dat, data == "uniform")

subset(uniform , normalized == "Normalized" & my_alpha == "Low Interactions" & my_gamma == "No Multiplicative Interactions") %>% group_by(variable) %>% dplyr::summarize(meanv = mean(value), se = se(value))

grouped = uniform %>% group_by(my_alpha, my_gamma, normalized, variable, budget) %>% dplyr::summarize(meanv = mean(value), se = se(value))
p = ggplot(grouped, aes(x = budget, y = meanv, colour = variable)) + geom_line() + geom_point() + facet_wrap(normalized~my_alpha, scales="free")
p = p + geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - se, ymax = meanv + se, colour = variable), size = 0.4, width=0.01)
p

neg <- subset(dat, data == "negative")
grouped = neg %>% group_by(my_alpha, normalized, variable, budget) %>% summarize(meanv = mean(value), se = mean(se(value)))
p = ggplot(grouped, aes(x = budget, y = meanv, colour = variable)) + geom_line() + geom_point() + facet_wrap(normalized~my_alpha, scales="free")
p = p + geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - se, ymax = meanv + se, colour = variable), size = 0.4, width=0.01)
p

pos <- subset(dat, data == "positive")
grouped = pos %>% group_by(my_alpha, normalized, variable, budget) %>% summarize(meanv = mean(value), se = se(value))
p = ggplot(grouped, aes(x = budget, y = meanv, colour = variable)) + geom_line() + geom_point() + facet_wrap(normalized~my_alpha, scales="free")
p = p + geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - se, ymax = meanv + se, colour = variable), size = 0.4, width=0.01)
p

#### Selection probabilities and random vs nested interactions ####
uniform <- subset(dat, data == "uniform" & my_alpha == "With Interactions" & normalized == "Normalized")
grouped = uniform %>% group_by(random_nested, my_selprob, variable, budget) %>% summarize(meanv = mean(value), se = se(value))
p = ggplot(grouped, aes(x = budget, y = meanv, colour = variable)) + geom_line() + geom_point() + facet_wrap(my_selprob~random_nested, scales="free")
p = p + geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - se, ymax = meanv + se, colour = variable), size = 0.4, width=0.01)
p

neg <- subset(dat, data == "uniform" & my_alpha == "With Interactions" & normalized == "Normalized")
grouped = neg %>% group_by(random_nested, my_selprob, variable, budget) %>% summarize(meanv = mean(value), se = se(value))
p = ggplot(grouped, aes(x = budget, y = meanv, colour = variable)) + geom_line() + geom_point() + facet_wrap(my_selprob~random_nested, scales="free")
p = p + geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - se, ymax = meanv + se, colour = variable), size = 0.4, width=0.01)
p

pos <- subset(dat, data == "uniform" & my_alpha == "With Interactions" & normalized == "Normalized")
grouped = pos %>% group_by(random_nested, my_selprob, variable, budget) %>% summarize(meanv = mean(value), se = se(value))
p = ggplot(grouped, aes(x = budget, y = meanv, colour = variable)) + geom_line() + geom_point() + facet_wrap(my_selprob~random_nested, scales="free")
p = p + geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - se, ymax = meanv + se, colour = variable), size = 0.4, width=0.01)
p

#############################################
#### Value decomposition ####
detach(package:plyr)

load("results/opt_value_decomposition.RData")

grouped = Xall %>% group_by(my_alpha, my_gamma, budget, data) %>% summarize(meanv = mean(proportion_no_interactions), se = se(proportion_no_interactions))
grouped$my_alpha = factor(grouped$my_alpha)
levels(grouped$my_alpha)[1] = paste0("Alpha: ", levels(grouped$my_alpha)[1])
grouped$my_gamma = factor(grouped$my_gamma)
levels(grouped$my_gamma)[1] = paste0("Gamma: ", levels(grouped$my_gamma)[1])
p = ggplot(grouped, aes(x = budget, y = meanv)) + geom_line() + geom_point()  + facet_grid(my_gamma~my_alpha) +ylim(c(0,1))
p = p + geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - se, ymax = meanv + se), size = 0.4, width=0.01)
p + ggtitle("Proportion of Value not contributed by Interactions")
