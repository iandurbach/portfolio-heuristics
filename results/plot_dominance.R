paretosetsize_neg = read.csv("results/pareto_setsize_neg.csv")
paretosetsize_psk = read.csv("results/pareto_setsize_psk.csv")
paretosetsize_uni = read.csv("results/pareto_setsize_uni.csv")
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)


summarized_tidy = function(data){
  tidy = data.frame(data) %>% 
  gather(pareto, value, -c(X, nproj, nCV, budget, my_alpha, my_gamma, my_selprob, random_nested, interaction_pool)) %>%
  mutate(pareto = as.numeric(str_extract(pareto, "[0-9][0-9]?")) -1) %>% 
  group_by(pareto) %>% 
  summarise(mean_value = mean(value), sd_above = mean(value)+sd(value), sd_below = mean(value)-sd(value))
}
tidy_neg = summarized_tidy(paretosetsize_neg)
tidy_psk = summarized_tidy(paretosetsize_psk)
tidy_uni = summarized_tidy(paretosetsize_uni)

pdf("results/fig/dominated_projects_portfolio.pdf", width = 6, height = 4)
ggplot() + theme_minimal() + theme(legend.key.size = unit(1.6, 'lines')) + 
  geom_point(data = tidy_neg, aes(x = pareto, y = mean_value, color = "blue")) + 
  geom_ribbon(data = tidy_neg, aes(x = pareto, ymin = sd_below, ymax = sd_above), fill = "blue", alpha = 0.1) +
  geom_point(data = tidy_psk, aes(x = pareto, y = mean_value, color = "red")) + 
  geom_ribbon(data = tidy_psk, aes(x = pareto, ymin = sd_below, ymax = sd_above), fill = "red", alpha = 0.1) +
  geom_point(data = tidy_uni, aes(x = pareto, y = mean_value, color = "black")) + 
  geom_ribbon(data = tidy_uni, aes(x = pareto, ymin = sd_below, ymax = sd_above), alpha = 0.1) +
  scale_color_manual(values = c("black", "red", "blue"), labels = c("uniform", "positively \nskewed", "negatively \nskewed"), name = "") + 
  ylab("mean number of\nnot dominated projects") + xlab("number of picked projects")
dev.off()
