#################################################
## Plots Decomposition of value of optimal portfolio ##
## into marginal and interaction contributions ##
## Note: plot not used in paper, eventually
################################################

library(tidyverse)
library(gridExtra)

load("results/opt_value_decomposition.RData")

se <- function(x)
  
  se = sd(x)/sqrt(length(x))

# set data for plot
Xall <- Xall %>% filter(my_alpha == 0, my_gamma > 0, budget > 0.05, budget <= 0.9)

# compute mean and se of proportions contributed by interactions 
grouped <- Xall %>% 
  group_by(my_gamma, budget) %>% 
  summarize(meanv = mean(1-proportion_no_interactions), se = se(1-proportion_no_interactions))

# add facet labels for plot
grouped$my_gamma <- factor(grouped$my_gamma)
levels(grouped$my_gamma) <- paste0("Gamma = ", levels(grouped$my_gamma))

# set main aesthetic variables
p <- ggplot(grouped, aes(x = budget, y = meanv)) + 
  geom_line() + geom_point()  + facet_grid(. ~ my_gamma) + ylim(c(0,1)) + 
  geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - se, ymax = meanv + se), size = 0.4, width=0.01)

# add a few plot options
p0 <- p + theme_bw(base_size=24) + 
  xlab("Budget (prop. of sum of all project costs)") + ylab("% contributed by interactions") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  scale_x_continuous(breaks = c(.2,.5,.8))

p0

# save
ggsave("results/int_contributions.png", p0, width = 12, height = 5, dpi = 300)

rm(Xall, p, p0)