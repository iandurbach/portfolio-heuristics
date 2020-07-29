## Plots for submitted version of paper "Psychological heuristics for portfolio decisions"

library(tidyverse)
library(gridExtra)
library(dplyr)
library(tidyverse)
library(forcats)
library(gridExtra)
library(RColorBrewer)

load("simulation/results/processed_dat.RData")

se <- function(x){ se = sd(x)/sqrt(length(x)) }

dat <- dat %>%
  mutate(heuristic = fct_recode(heuristic,
                                "Unit Value" = "AtB-myo",
                                "Highest Value" = "AtV",
                                "Lowest Cost" = "AtC",
                                "Added Value" = "AtB",
                                "Added Value Most"= "AtB-mv",
                                "Added Value Least" = "AtB-lv",
                                "Added Value Random" = "AtB-rv",
                                "Unit Value with Synergy" = "Lex",
                                "Lex3c" = "Lex3c",
                                "Lex3cb" = "lex3cb",
                                "Pareto" = "ND"))

##################################
## Figure 1: Overall absolute performance of heuristics
## split into (a) AtB heuristics, (b) others
##################################

# set data for plot
newdata <- dat %>% 
  filter(normalized == "Absolute") %>%
  filter(any_ints == "With Interactions", my_alpha == "No Interactions") %>%
  filter(budget >= 0.05, budget <= 0.9) %>%
  #filter(random_nested == "Random") %>%
  #filter(my_selprob == "Random") %>%
  droplevels()

# (a) the AtB-type heuristics 
heurs <- c("Added Value", "Added Value Most", "Added Value Random", "Added Value Least", "Opt", "Nadir", "Random", "Unit Value with Synergy","Lex3c","Lex3cb")
heurs2 <-  c("Added Value", "Added Value Most", "Added Value Random", "Added Value Least")

#heurs2 = c("Added Value","Unit Value with Synergy","Lex3c","Lex3cb")

# compute mean performances
grouped = newdata %>% 
  filter(heuristic %in% heurs) %>%
  group_by(heuristic,budget) %>% 
  dplyr::summarize(meanv = mean(value), se = se(value))

# will use Opt and Random performance to plot an envelope of best/worst performance
ranges = grouped %>% 
  filter(heuristic %in% c("Random", "Opt")) %>%
  group_by(budget) %>% 
  dplyr::summarize(ymax = max(meanv), ymin = min(meanv))

# get Nadir performance, will plot as a line later (absolute worst)
nadir = grouped %>% 
  filter(heuristic %in% c("Nadir")) %>%
  group_by(budget) %>% 
  dplyr::summarize(ymax = max(meanv), ymin = min(meanv))

# reduce the set of heuristics to plot (exclude Opt, Random, Nadir)
grouped <- filter(grouped, heuristic %in% heurs2)

# set the main aesthetic variables
p = ggplot(grouped, aes(x = budget, 
                        y = meanv, 
                        colour = heuristic,
                        shape = heuristic))

# envelope of best/worst performance
p = p + geom_ribbon(ranges, 
                    mapping = aes(x = budget, ymin = ymin, ymax = ymax),
                    inherit.aes = F,
                    fill = "grey70",
                    alpha = 0.5)

# plot heuristic performance with error bars, and nadir performance
p = p +  
  geom_line(size = 1.25) + 
  geom_point(size = 3) +
  geom_line(nadir, mapping = aes(x = budget, y = ymin), lty = 2, inherit.aes = F) +
  geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - 2 * se, ymax = meanv + 2 * se, colour = heuristic), size = 0.4, width=0.01) +
  annotate("text", x = 0.06, y = 640, label = "(a)", size = 9) + 
  scale_color_manual(values = brewer.pal(9, "Set1"))

# few plot options, mainly resizing text
p = p + theme_bw(base_size=24) + 
  xlab("Budget (prop. of sum of all project costs)") + ylab("Portfolio value") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  theme(legend.position = "bottom", legend.text=element_text(size=18),
        legend.title=element_blank(), legend.key.size = unit(2, 'lines')) + 
  guides(colour = guide_legend(nrow = 3)) 

p1 <- p

p1

# (b) myopic + non-dom heuristics
heurs <- c("Unit Value", "Highest Value", "Lowest Cost", "Pareto", "Opt", "Nadir", "Random", "Unit Value with Synergy", "Lex3c","Lex3cb")
heurs2 <-  c("Unit Value", "Highest Value", "Lowest Cost",  "Unit Value with Synergy", "Pareto")

# compute mean performances
grouped = newdata %>% 
  filter(heuristic %in% heurs) %>%
  group_by(heuristic,budget) %>% 
  dplyr::summarize(meanv = mean(value), se = se(value))

# will use Opt and Random performance to plot an envelope of best/worst performance
ranges = grouped %>% 
  filter(heuristic %in% c("Random", "Opt")) %>%
  group_by(budget) %>% 
  dplyr::summarize(ymax = max(meanv), ymin = min(meanv))

# get Nadir performance, will plot as a line later (absolute worst)
nadir = grouped %>% 
  filter(heuristic %in% c("Nadir")) %>%
  group_by(budget) %>% 
  dplyr::summarize(ymax = max(meanv), ymin = min(meanv))

# reduce the set of heuristics to plot (exclude Opt, Random, Nadir)
grouped <- filter(grouped, heuristic %in% heurs2)

#change order of heuristics
grouped$heuristic = factor(grouped$heuristic, levels = heurs2)

# set the main aesthetic variables
p = ggplot(grouped, aes(x = budget, 
                        y = meanv, 
                        colour = heuristic,
                        shape = heuristic))

# envelope of best/worst performance
p = p + geom_ribbon(ranges, 
                    mapping = aes(x = budget, ymin = ymin, ymax = ymax),
                    inherit.aes = F,
                    fill = "grey70",
                    alpha = 0.5)

# plot heuristic performance with error bars, and nadir performance
p = p +  
  geom_line(size = 1.25) + 
  geom_point(size = 3) +
  geom_line(nadir, mapping = aes(x = budget, y = ymin), lty = 2, inherit.aes = F) +
  geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - 2 * se, ymax = meanv + 2 * se, colour = heuristic), size = 0.4, width=0.01) +
  annotate("text", x = 0.06, y = 640, label = "(b)", size = 9) + scale_color_manual(values = brewer.pal(9, "Set1")[5:9])

# few plot options, mainly resizing text
p = p + theme_bw(base_size=24) + 
  xlab("Budget (prop. of sum of all project costs)") + ylab("Performance") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  theme(legend.position = "bottom", legend.text=element_text(size=18),
        legend.title=element_blank(), legend.key.size = unit(2, 'lines')) + 
  guides(colour = guide_legend(nrow = 3)) 

p2 <- p

p2

# combine plots (a) and (b) and save
ggsave("simulation/results/absoluteperf.png", arrangeGrob(p1, p2, ncol = 2), 
       width = 14, height = 7, dpi = 300)


##################################
## Figure 2: Quantiles of relative performance of heuristics
##################################

# set data for plot
newdata <- dat %>% 
  filter(normalized == "Absolute") %>%
  filter(any_ints == "With Interactions", my_alpha == "No Interactions") %>%
  filter(budget >= 0.05, budget <= 0.9) %>%
  droplevels()

heurs = c("Unit Value", "Unit Value with Synergy", "Added Value", 
          "Added Value Most", "Added Value Least", "Added Value Random",
          "Highest Value", "Lowest Cost", "Pareto", "Random", "Nadir")

heurs_in_order = c("Added Value", "Added Value Most", "Added Value Least", "Added Value Random",
                   "Unit Value with Synergy", "Unit Value", "Highest Value", "Lowest Cost",
                   "Pareto", "Random", "Nadir")

# compute quantiles of performances
grouped = newdata %>% 
  filter(heuristic %in% heurs) %>%
  group_by(heuristic,budget) %>% 
  dplyr::summarize(q10 = quantile(100*normvalue,0.1),
                  q25 = quantile(100*normvalue,0.25),
                  q50 = quantile(100*normvalue,0.5),
                  q75 = quantile(100*normvalue,0.75),
                  q90 = quantile(100*normvalue,0.9))

grouped$heuristic = factor(grouped$heuristic, levels = heurs_in_order)

# set the main aesthetic variables
p = ggplot(grouped, aes(x = budget, 
                        y = q50, 
                        colour = heuristic))

# envelope of 25/75 performance
p = p + geom_ribbon(grouped, 
                    mapping = aes(x = budget, ymin = q25, ymax = q75, 
                                  fill = heuristic),
                    inherit.aes = F,
                    alpha = 0.5)

# envelope of 10/90 performance
p = p + geom_ribbon(grouped, 
                    mapping = aes(x = budget, ymin = q10, ymax = q90, 
                                  fill = heuristic),
                    inherit.aes = F,
                    alpha = 0.3)

# plot heuristic performance with error bars, and nadir performance
p = p +  
  geom_line(size = 1.25) +
  geom_point(size = 2) +
  facet_wrap(~ heuristic, nrow = 3) 

# few plot options, mainly resizing text
p = p + theme_bw(base_size=24) + 
  coord_cartesian(ylim=c(20, 100)) +
  scale_x_continuous(breaks = c(.2,.5,.8)) +
  xlab("Budget (prop. of sum of all project costs)") + ylab("% of optimal portfolio value achieved") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), strip.text = element_text(size = 14)) +
  theme(legend.position = "none") 

p1 <- p

p1

# save
ggsave("simulation/results/relativeperf.png", p1, width = 10.5, height = 7, dpi = 300)

##################################
## Figure 3: Quantiles of relative performance of heuristics when no interactions
## NOT USED
##################################

# set data for plot
newdata <- dat %>% 
  filter(normalized == "Absolute") %>%
  filter(any_ints == "No Interactions") %>%
  filter(budget >= 0.05, budget <= 0.9) %>%
  droplevels()

heurs <- c("Unit Value", "Pareto", "Highest Value", "Lowest Cost")

# compute quantiles of performances
grouped = newdata %>% 
  filter(heuristic %in% heurs) %>%
  group_by(heuristic,budget) %>% 
  summarize(q10 = quantile(100*normvalue,0.1),
            q25 = quantile(100*normvalue,0.25),
            q50 = quantile(100*normvalue,0.5),
            q75 = quantile(100*normvalue,0.75),
            q90 = quantile(100*normvalue,0.9))

grouped_m = newdata %>% 
  filter(X != 1730) %>% # remove one obs where opt = rand
  filter(heuristic %in% heurs) %>%
  group_by(heuristic,budget) %>% 
  summarize(meanv = mean(100*normvalue),
            se = se(100*normvalue))

# set the main aesthetic variables
p = ggplot(grouped, aes(x = budget, 
                        y = q50, 
                        colour = heuristic))

# envelope of 25/75 performance
p = p + geom_ribbon(grouped, 
                    mapping = aes(x = budget, ymin = q25, ymax = q75, 
                                  fill = heuristic),
                    inherit.aes = F,
                    alpha = 0.5)

# envelope of 10/90 performance
p = p + geom_ribbon(grouped, 
                    mapping = aes(x = budget, ymin = q10, ymax = q90, 
                                  fill = heuristic),
                    inherit.aes = F,
                    alpha = 0.3)

# plot heuristic performance with error bars, and nadir performance
p = p +  
  geom_line(size = 1.25) + 
  geom_point(size = 3) +
  facet_wrap(~ heuristic, nrow = 1) 

p = p + 
  geom_line(grouped_m, mapping = aes(x = budget, y = meanv), lty = 2, inherit.aes = F) +
  geom_errorbar(grouped_m, mapping = aes(x = budget, ymin = meanv - 2 * se, ymax = meanv + 2 * se), 
                inherit.aes = F, size = 0.4, width=0.01)

# few plot options, mainly resizing text
p = p + theme_bw(base_size=24) + 
  coord_cartesian(ylim=c(-50, 100)) +
  scale_x_continuous(breaks = c(.2,.5,.8)) +
  xlab("Budget (prop. of sum of all project costs)") + ylab("Relative Performance") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  theme(legend.position = "none") 

p1 <- p

p1

# save
ggsave("simulation/results/relativeperf-noint.png", p1, width = 12, height = 4, dpi = 300)


##################################
## Figure 4: effect of interaction size (my gamma)
##################################

# set data for plot
newdata <- dat %>% 
  filter(normalized == "Absolute") %>%
  filter(my_alpha == "No Interactions") %>%
  filter(budget > 0.05, budget < 0.9) %>%
  droplevels()

newdata <- newdata %>% 
  mutate(newvar = ifelse(any_ints == "No Interactions", "None", 
                         ifelse(my_gamma == "Small Interactions", "Small",
                                "Large")))

heurs <- c("Added Value", "Unit Value with Synergy", "Added Value Most", "Unit Value")

# compute quantiles of performances
grouped = newdata %>% 
  filter(X != 1730) %>% # remove one obs where opt = rand
  filter(heuristic %in% heurs) %>%
  group_by(heuristic,budget, my_gamma) %>% 
  dplyr::summarize(meanv = mean(100*normvalue),
            se = se(100*normvalue))

grouped$heuristic = factor(grouped$heuristic, levels = heurs)

grouped$my_gamma2 <- factor(grouped$my_gamma, levels = c("No Interactions","Small Interactions", "Large Interactions"),
                            ordered = TRUE, labels = c(expression(paste("No Interactions")), expression(paste("Moderate Interactions (",gamma," = 0.5)")), expression(paste("Large Interactions (",gamma," = 1)"))))

# set the main aesthetic variables
p = ggplot(grouped, aes(x = budget, 
                        y = meanv, 
                        colour = heuristic,
                        shape = heuristic))

# plot heuristic performance with error bars, and nadir performance
p = p + 
  geom_line(size = 1.23) +
  geom_point(size = 3) +
  geom_errorbar(aes(x = budget, ymin = meanv - 2 * se, ymax = meanv + 2 * se),
                size = 0.4, width=0.01) + 
  facet_wrap(~ my_gamma2, nrow = 1, labeller = label_parsed) 

# few plot options, mainly resizing text
p = p + theme_bw(base_size=24) + 
  coord_cartesian(ylim=c(30, 100)) +
  scale_x_continuous(breaks = c(.2,.5,.8)) +
  xlab("Budget (prop. of sum of all project costs)") + ylab("% optimal") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  theme(legend.position = "bottom", legend.text=element_text(size=20),
        legend.title=element_blank(), legend.key.size = unit(3, 'lines'), strip.text = element_text(size = 14)) + 
  #scale_color_manual(labels = c("No Interactions", expression(paste(gamma," = 0.5")), expression(paste(gamma," = 1"))), values = 2:4) +
  #scale_shape_manual(labels = c("No Interactions", expression(paste(gamma," = 0.5")), expression(paste(gamma," = 1"))), values = 2:4) +
  guides(colour = guide_legend(nrow = 1)) 


p1 <- p

p1

# save
ggsave("simulation/results/relativeperf-intsize.png", p1, width = 14, height = 5, dpi = 300)

##################################
## Figure 5: Show sens to budget changes when nested
##################################

# set data for plot
newdata <- dat %>% 
  filter(any_ints == "With Interactions", my_alpha == "No Interactions") %>%
  filter(budget >= 0.1, budget <= 0.8) %>%
  droplevels()

# select heuristics we want to show (plus random for stdization)
heurs <- c("Added Value", "Unit Value with Synergy", "Added Value Most", "Pareto")

# compute quantiles of performances
grouped = newdata %>% 
  filter(heuristic %in% heurs) %>%
  group_by(heuristic, random_nested, my_selprob, budget) %>% 
  dplyr::summarize(meanv = mean(100 * normvalue),
            se = se(100 * normvalue)) %>%
  ungroup() %>%
  mutate(my_selprob = factor(my_selprob, levels = c("Good projects", "Poor projects", "Random")))

grouped$heuristic = factor(grouped$heuristic, levels = heurs)

# set the main aesthetic variables
p = ggplot(grouped, aes(x = budget, 
                        y = meanv, 
                        colour = heuristic,
                        shape = heuristic))

# plot heuristic performance with error bars, and nadir performance
p = p +  
  geom_line(size = 1.25) + 
  geom_point(size = 3) +
  geom_errorbar(grouped, 
                mapping = aes(x = budget, 
                              ymin = meanv - 2 * se, 
                              ymax = meanv + 2 * se,
                              colour = heuristic), size = 0.4, width=0.01) +
  facet_grid(random_nested ~ my_selprob, scales = "free") 

# few plot options, mainly resizing text
p = p + theme_bw(base_size=24) + 
  scale_x_continuous(breaks = c(.2,.5,.8)) + scale_y_continuous(breaks = c(50,100)) +
  xlab("Budget (prop. of sum of all project costs)") + ylab("% optimal") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  theme(legend.position = "bottom", legend.text=element_text(size=20), strip.text = element_text(size = 14),
        legend.title=element_blank(), legend.key.size = unit(3, 'lines')) + 
  guides(colour = guide_legend(nrow = 1)) 

p

# save
ggsave("simulation/results/interactions.png", p, width = 14, height = 8, dpi = 300)
