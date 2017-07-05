library(tidyverse)
library(xtable)

load("dat.RData")

options(dplyr.print_max = 1e4)

se <- function(x)
  se=sd(x)/sqrt(length(x))

normdat <- filter(dat, normalized == "Normalized" &
                    variable != "Optimal" &
                    variable != "Nadir")
normdat <- droplevels(normdat)
normdat$variable <- factor(normdat$variable,
                           levels=c("greedynet","mvp","lvp","rvp","Greedy","greedyvalue",
                                    "greedycost","Heuristic","Random"))

normdat <- mutate(normdat, heuristic = fct_recode(variable,
                                        "AtB" = "greedynet", 
                                        "AtB-mvp" = "mvp",
                                        "AtB-lvp" = "lvp",
                                        "AtB-rvp" = "rvp",
                                        "AtB-myopic" = "Greedy",
                                        "AtV" = "greedyvalue",
                                        "AtC" = "greedycost",
                                        "Non-dom" = "Heuristic",
                                        "Random" = "Random"))

# no interactions

t0 <- filter(normdat, my_alpha == "No Interactions") %>% group_by(heuristic) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))

t1 <- filter(normdat, my_alpha == "No Interactions") %>% group_by(heuristic,my_selprob) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))

t2 <- filter(normdat, my_alpha == "No Interactions") %>% group_by(heuristic,random_nested) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))

t3 <- filter(normdat, my_alpha == "No Interactions") %>% group_by(heuristic,budget) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))

t4 <- filter(normdat, my_alpha == "No Interactions") %>% group_by(heuristic,data) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))

t0 <- xtabs(meanv ~ ., data = t0[,-3])
t0 <- 100*(t0 - t0[9])/(100 - t0[9])
xtable(t0, digits=0,type = "latex")

t1 <- xtabs(meanv ~ my_selprob + heuristic, data = t1)
t1 <- 100*(t1 - t1[,9])/(100 - t1[,9])
xtable(t1, digits=0,type = "latex")

t2 <- xtabs(meanv ~ random_nested + heuristic, data = t2)
t2 <- 100*(t2 - t2[,9])/(100 - t2[,9])
xtable(t2, digits=0,type = "latex")

t3 <- xtabs(meanv ~ budget + heuristic, data = t3)
t3 <- 100*(t3 - t3[,9])/(100 - t3[,9])
xtable(t3, digits=0,type="latex")
1-t3[1,]/t3[5,]

t4 <- xtabs(meanv ~ data + heuristic, data = t4)
t4 <- 100*(t4 - t4[,9])/(100 - t4[,9])
xtable(t4, digits=0,type="latex")

## with interactions

t0 <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))

t1 <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,my_selprob) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))

t2 <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,random_nested) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))

t3 <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,budget) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))

t4 <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,data) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))

t0 <- xtabs(meanv ~ ., data = t0[,-3])
t0 <- 100*(t0 - t0[9])/(100 - t0[9])
xtable(t0, digits=0,type = "latex")

t1 <- xtabs(meanv ~ my_selprob + heuristic, data = t1)
t1 <- 100*(t1 - t1[,9])/(100 - t1[,9])
xtable(t1, digits=0,type = "latex")

t2 <- xtabs(meanv ~ random_nested + heuristic, data = t2)
t2 <- 100*(t2 - t2[,9])/(100 - t2[,9])
xtable(t2, digits=0,type = "latex")

t3 <- xtabs(meanv ~ budget + heuristic, data = t3)
t3 <- 100*(t3 - t3[,9])/(100 - t3[,9])
xtable(t3, digits=0,type="latex")
1-t3[1,]/t3[5,]

t4 <- xtabs(meanv ~ data + heuristic, data = t4)
t4 <- 100*(t4 - t4[,9])/(100 - t4[,9])
xtable(t4, digits=0,type="latex")


## digging into budget

tb <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,budget,random_nested) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))
xtabs(meanv ~ budget + heuristic + random_nested, data = tb)

tb <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,budget,my_selprob) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2)) %>%
  group_by(heuristic,my_selprob) %>% summarize(diff = 1 - min(meanv)/max(meanv))
xtabs(diff ~ my_selprob + heuristic, data = tb)

tb <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,budget,data) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2)) %>% 
  group_by(budget,data) %>% mutate(meanv = (meanv - meanv[heuristic == "Random"])/(100 - meanv[heuristic == "Random"])) %>%
  group_by(heuristic,data) %>% summarize(diff = 1 - min(meanv)/max(meanv))
xtabs(diff ~ data + heuristic, data = tb)

tb <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,budget,random_nested) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2)) %>%
  group_by(heuristic,random_nested) %>% summarize(diff = 1 - min(meanv)/max(meanv))
xtabs(diff ~ random_nested + heuristic, data = tb)

tb <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,budget,random_nested,data) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2)) %>%
  group_by(heuristic,random_nested,data) %>% summarize(diff = 1 - min(meanv)/max(meanv))
xtabs(diff ~ data + heuristic + random_nested, data = tb)

## at constrained budget

tb <- filter(normdat, my_alpha == "With Interactions" & budget == 0.2) %>% group_by(heuristic,data,random_nested) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))
xtabs(meanv ~ data + heuristic + random_nested, data = tb)

## digging into why atv/c do well in positive skew env

t3 <- filter(normdat, my_alpha == "With Interactions" & data == "positive") %>% group_by(heuristic,budget) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))
xtabs(meanv ~ budget + heuristic, data = t3)

tb <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,data,random_nested) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))
xtabs(meanv ~ data + heuristic + random_nested, data = tb)

tb <- filter(normdat, my_alpha == "With Interactions") %>% group_by(heuristic,data,my_selprob) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))
xtabs(meanv ~ data + heuristic + my_selprob, data = tb)

## plots

heurs <- c("Random","AtB","AtB-rvp","AtB-myopic","Non-dom","AtV")

# show sens to budget changes when pos skew data
newdata <- filter(mutate(normdat, data = fct_collapse(data,"Positive" = "positive",
                                                      "Not positive" = c("negative","uniform"))), 
                  my_alpha == "With Interactions" & heuristic %in% heurs)
grouped <- newdata %>% group_by(heuristic,budget,data) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))
grouped <- grouped %>% group_by(budget,data) %>% mutate(meanv = 100*(meanv - meanv[heuristic == "Random"])/(100 - meanv[heuristic == "Random"]))
grouped <- filter(grouped, heuristic != "Random")
p = ggplot(grouped, aes(x = budget, y = meanv, colour = heuristic, shape = heuristic)) + 
  geom_line() + geom_point(size=6) + facet_grid(. ~ data) 
p = p + geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - se, ymax = meanv + se, colour = heuristic), size = 0.4, width=0.01)
p = p + theme_bw(base_size=24) +
  xlab("Budget (prop. of sum of all project costs)") + ylab("Performance") + 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20)) +
  theme(legend.position = "bottom",legend.text=element_text(size=20),
        legend.title=element_blank(), legend.key.size = unit(3, 'lines')) + 
  guides(colour = guide_legend(nrow = 1)) 
p

xtabs(meanv ~ data + budget + heuristic, data = grouped)

ggsave("budgetXdata.png", p, width=13, height=6.5, dpi=200)

# show sens to budget changes when nested
newdata <- filter(normdat, my_alpha == "With Interactions" & heuristic %in% heurs)
grouped = newdata %>% group_by(heuristic,budget,random_nested) %>% 
  summarize(meanv = round(mean(100*value),0), se = round(se(100*value),2))
grouped <- grouped %>% group_by(budget,random_nested) %>% mutate(meanv = 100*(meanv - meanv[heuristic == "Random"])/(100 - meanv[heuristic == "Random"]))
grouped <- filter(grouped, heuristic != "Random")
p = ggplot(grouped, aes(x = budget, y = meanv, colour = heuristic, shape = heuristic)) + 
  geom_line() + geom_point(size = 6) + facet_grid(.~random_nested)
p = p + geom_errorbar(grouped, mapping = aes(x = budget, ymin = meanv - se, ymax = meanv + se, colour = heuristic), size = 0.4, width=0.01)
p = p + theme_bw(base_size=24) +
  xlab("Budget (prop. of sum of all project costs)") + ylab("Performance") + 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20)) +
  theme(legend.position = "bottom",legend.text=element_text(size=20),
        legend.title=element_blank(), legend.key.size = unit(3, 'lines')) + 
  guides(colour = guide_legend(nrow = 1)) 
p

xtabs(meanv ~ random_nested + budget + heuristic, data = grouped)

ggsave("budgetXnested.png", p, width=13, height=6.5, dpi=200)
