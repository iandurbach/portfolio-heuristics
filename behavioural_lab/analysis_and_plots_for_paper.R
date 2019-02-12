library(tidyverse)
library(readxl)
library(ggalluvial)
library(gridExtra)
library(xtable)

source("behavioural_lab/utils.R")

# res 1 and 2 look at the final portfolio selected, and how this was put together
# res 3 looks at the first few selections, regardless of the final portfolio

########################################################
### For "Task 1": 5 projects with no project interactions
########################################################

task_data <- readRDS("behavioural_lab/data/task1_project_data.Rds")
actions <- readRDS("behavioural_lab/output/task1_selection_data.Rds")

finals <- actions %>% group_by(RespID) %>% 
  slice(n()) %>% # last entry has the selected portfolio
  select(RespID, Next_Portfolio) %>% 
  mutate(portfolio_value = portfolio_benefit(unlist(Next_Portfolio), task_data),
         portfolio_cost = portfolio_cost(unlist(Next_Portfolio), task_data$costs),
         portfolio = str_c(unlist(Next_Portfolio), collapse = ""),
         portfolio_alt = I(list(unlist(Next_Portfolio) * 1:5)))

final_selection_sequence <- actions %>% 
  filter(Action == "addition") %>% 
  group_by(RespID, Projects) %>% summarize(last_sel = max(ActionID)) %>% 
  left_join(finals %>% select(RespID, portfolio_alt), by = "RespID") %>%
  rowwise() %>%
  filter(Projects %in% portfolio_alt) %>%
  ungroup() %>% 
  arrange(RespID, last_sel) %>% 
  group_by(RespID) %>%
  summarise(ordered_seq = paste(Projects, collapse = "-"))

effort <- actions %>% 
  filter((next_in_budget == 1)|(Projects == dplyr::lead(Projects))) %>%
  group_by(RespID, Action) %>% count() %>% 
  spread(Action, n) %>% replace_na(list(removal = 0)) %>%
  mutate(total = addition + removal)

res1_t1 <- finals %>% 
  left_join(effort, by = "RespID") %>% 
  group_by(portfolio) %>% 
  summarize(n = n(), 
            value = mean(portfolio_value),
            cost = mean(portfolio_cost),
            mean_add = mean(addition),
            mean_rem = mean(removal),
            mean_tot = mean(total)) %>% 
  arrange(desc(n))

res2_t1 <- finals %>% 
  left_join(effort, by = "RespID") %>% 
  left_join(final_selection_sequence, by = "RespID") %>%
  group_by(portfolio, ordered_seq) %>%
  count() %>% arrange(portfolio) %>% ungroup()

res1_t1
res2_t1

alluv_res2_t1 <- res2_t1 %>% filter(n > 1) %>% select(-portfolio) %>%
  separate(ordered_seq, into = c("select_1", "select_2", "select_3"), sep = "-", remove = FALSE) %>% 
  dplyr::select(select_1, select_2, select_3, n) %>% 
  mutate_at(1:3, funs(str_c("P",.))) %>%
  replace_na(list(select_3 = "-")) %>%
  ggplot(aes(y = n, axis1 = select_1, axis2 = select_2, axis3 = select_3)) +
  geom_alluvium(aes(fill = select_1), width = 1/6) +
  geom_stratum(width = 1/6) +
  geom_label(stat = "stratum", label.strata = TRUE, size = 4, label.size = 0) +
  guides(fill = FALSE) + 
  annotate(geom = "text", x = 0.83, y = 60, label = "a)", size = 8) +
  scale_x_discrete(limits = c("1st", "2nd", "3rd"), expand = c(.05, .05)) +
  ylab("Number of responses") + xlab("Task 1 Selection order") + ylim(0,60) +
  theme(axis.text=element_text(size=20), 
        axis.title=element_text(size=20))

# first three selections

first_selection <- actions %>% filter(Action == "addition") %>%
  arrange(RespID, ActionID) %>%
  group_by(RespID) %>%
  summarize(first = first(Projects))

first_2_selections <- actions %>% filter(Action == "addition") %>%
  arrange(RespID, ActionID) %>%
  group_by(RespID) %>%
  slice(1:2) %>%
  summarize(first_2 = paste(Projects, collapse = "-"))

first_3_selections <- actions %>% filter(Action == "addition") %>%
  arrange(RespID, ActionID) %>%
  group_by(RespID) %>%
  slice(1:3) %>%
  summarize(first_3 = paste(Projects, collapse = "-"))

first_all <- first_selection %>% left_join(first_2_selections, by = "RespID") %>% left_join(first_3_selections, by = "RespID")

first_all_1 <- first_all %>% group_by(first) %>% summarize(n1 = n())
first_all_2 <- first_all %>% group_by(first_2) %>% summarize(n2 = n())
first_all_3 <- first_all %>% group_by(first_3) %>% summarize(n3 = n())

first_all <- first_all %>% 
  left_join(first_all_1, by = "first") %>%
  left_join(first_all_2, by = "first_2") %>%
  left_join(first_all_3, by = "first_3") %>%
  arrange(desc(n1), desc(n2), desc(n3)) 

res3_t1 <- first_all %>% distinct(first_3, .keep_all = TRUE) %>% 
  select(-RespID) %>% 
  separate(first_3, into = c("select_1", "select_2", "select_3"), sep = "-", remove = FALSE)

res3_t1

alluv_res3_t1 <- res3_t1 %>% 
  dplyr::select(select_1, select_2, select_3, n3) %>% 
  mutate_at(1:3, funs(str_c("P",.))) %>%
  replace_na(list(select_3 = "-")) %>% 
  ggplot(aes(y = n3, axis1 = select_1, axis2 = select_2, axis3 = select_3)) +
  geom_alluvium(aes(fill = select_1), width = 1/8) +
  geom_stratum(width = 1/8) +
  geom_label(stat = "stratum", label.strata = TRUE, size = 4, label.size = 0) +
  guides(fill = FALSE) + 
  annotate(geom = "text", x = 0.83, y = 75, label = "a)", size = 8) +
  scale_x_discrete(limits = c("1st", "2nd", "3rd"), expand = c(.05, .05)) +
  ylab("Number of responses") + xlab("Task 1 Selection order") +
  theme(axis.text=element_text(size=20), 
        axis.title=element_text(size=20))

########################################################
### For "Task 2": 5 projects with no project interactions
########################################################

task_data <- readRDS("behavioural_lab/data/task2_project_data.Rds")
actions <- readRDS("behavioural_lab/output/task2_selection_data.Rds")

finals <- actions %>% group_by(RespID) %>% 
  slice(n()) %>% # last entry has the selected portfolio
  select(RespID, Next_Portfolio) %>% 
  mutate(portfolio_value = portfolio_benefit(unlist(Next_Portfolio), task_data),
         portfolio_cost = portfolio_cost(unlist(Next_Portfolio), task_data$costs),
         portfolio = str_c(unlist(Next_Portfolio), collapse = ""),
         portfolio_alt = I(list(unlist(Next_Portfolio) * 1:5)))

final_selection_sequence <- actions %>% 
  filter(Action == "addition") %>% 
  group_by(RespID, Projects) %>% summarize(last_sel = max(ActionID)) %>% 
  left_join(finals %>% select(RespID, portfolio_alt), by = "RespID") %>%
  rowwise() %>%
  filter(Projects %in% portfolio_alt) %>%
  ungroup() %>% 
  arrange(RespID, last_sel) %>% 
  group_by(RespID) %>%
  summarise(ordered_seq = paste(Projects, collapse = "-"))

effort <- actions %>% 
  filter((next_in_budget == 1)|(Projects == dplyr::lead(Projects))) %>%
  group_by(RespID, Action) %>% count() %>% 
  spread(Action, n) %>% replace_na(list(removal = 0)) %>%
  mutate(total = addition + removal)

res1_t2 <- finals %>% 
  left_join(effort, by = "RespID") %>% 
  group_by(portfolio) %>% 
  summarize(n = n(), 
            value = mean(portfolio_value),
            cost = mean(portfolio_cost),
            mean_add = mean(addition),
            mean_rem = mean(removal),
            mean_tot = mean(total)) %>% 
  arrange(desc(n))

res2_t2 <- finals %>% 
  left_join(effort, by = "RespID") %>% 
  left_join(final_selection_sequence, by = "RespID") %>%
  group_by(portfolio, ordered_seq) %>%
  count() %>% arrange(portfolio) %>% ungroup()

res1_t2
res2_t2

alluv_res2_t2 <- res2_t2 %>% filter(n > 1) %>% select(-portfolio) %>%
  separate(ordered_seq, into = c("select_1", "select_2", "select_3"), sep = "-", remove = FALSE) %>% 
  dplyr::select(select_1, select_2, select_3, n) %>% 
  mutate_at(1:3, funs(str_c("P",.))) %>%
  replace_na(list(select_3 = "-")) %>%
  ggplot(aes(y = n, axis1 = select_1, axis2 = select_2, axis3 = select_3)) +
  geom_alluvium(aes(fill = select_1), width = 1/6) +
  geom_stratum(width = 1/6) +
  geom_label(stat = "stratum", label.strata = TRUE, size = 4, label.size = 0) +
  guides(fill = FALSE) + 
  annotate(geom = "text", x = 0.83, y = 60, label = "b)", size = 8) +
  scale_x_discrete(limits = c("1st", "2nd", "3rd"), expand = c(.05, .05)) +
  ylab("Number of responses") + xlab("Task 2 Selection order") + ylim(0, 60) +
  theme(axis.text.y = element_blank(), axis.text=element_text(size=20), 
        axis.title.y = element_blank(), axis.title=element_text(size=20))

# first three selections

first_selection <- actions %>% filter(Action == "addition") %>%
  arrange(RespID, ActionID) %>%
  group_by(RespID) %>%
  summarize(first = first(Projects))

first_2_selections <- actions %>% filter(Action == "addition") %>%
  arrange(RespID, ActionID) %>%
  group_by(RespID) %>%
  slice(1:2) %>%
  summarize(first_2 = paste(Projects, collapse = "-"))

first_3_selections <- actions %>% filter(Action == "addition") %>%
  arrange(RespID, ActionID) %>%
  group_by(RespID) %>%
  slice(1:3) %>%
  summarize(first_3 = paste(Projects, collapse = "-"))

first_all <- first_selection %>% left_join(first_2_selections, by = "RespID") %>% left_join(first_3_selections, by = "RespID")

first_all_1 <- first_all %>% group_by(first) %>% summarize(n1 = n())
first_all_2 <- first_all %>% group_by(first_2) %>% summarize(n2 = n())
first_all_3 <- first_all %>% group_by(first_3) %>% summarize(n3 = n())

first_all <- first_all %>% 
  left_join(first_all_1, by = "first") %>%
  left_join(first_all_2, by = "first_2") %>%
  left_join(first_all_3, by = "first_3") %>%
  arrange(desc(n1), desc(n2), desc(n3))

res3_t2 <- first_all %>% distinct(first_3, .keep_all = TRUE) %>% select(-RespID) %>% 
  separate(first_3, into = c("select_1", "select_2", "select_3"), sep = "-", remove = FALSE)

res3_t2

alluv_res3_t2 <- res3_t2 %>%
  dplyr::select(select_1, select_2, select_3, n3) %>% 
  mutate_at(1:3, funs(str_c("P",.))) %>%
  replace_na(list(select_3 = "-")) %>% 
  ggplot(aes(y = n3, axis1 = select_1, axis2 = select_2, axis3 = select_3)) +
  geom_alluvium(aes(fill = select_1), width = 1/8) +
  geom_stratum(width = 1/8) +
  geom_label(stat = "stratum", label.strata = TRUE, size = 4, label.size = 0) +
  guides(fill = FALSE) +
  annotate(geom = "text", x = 0.83, y = 75, label = "b)", size = 8) +
  scale_x_discrete(limits = c("1st", "2nd", "3rd"), expand = c(.05, .05)) +
  ylab("Number of responses") + xlab("Task 2 Selection order") +
  theme(axis.text.y = element_blank(), axis.text=element_text(size=20), 
        axis.title.y = element_blank(), axis.title=element_text(size=20))


ggsave("behavioural_lab/results/experiment_final3order.png", arrangeGrob(alluv_res2_t1, alluv_res2_t2, ncol = 2, widths = c(10,9)), width=14, height=7, dpi = 200)

ggsave("behavioural_lab/results/experiment_first3order.png", arrangeGrob(alluv_res3_t1, alluv_res3_t2, ncol = 2, widths = c(10,9)), width=14, height=7, dpi = 200)

save(res1_t1, res2_t1, res3_t1, res1_t2, res2_t2, res3_t2, file = "behavioural_lab/results/results_final_selections.RData")

# processing for tables

res_t1_ranks <- res2_t1 %>% 
  group_by(portfolio) %>% 
  mutate(rank_ord_seq = rank(desc(n), ties.method = "random")) %>% ungroup() %>% 
  arrange(portfolio, rank_ord_seq) %>%
  complete(portfolio, rank_ord_seq) %>%
  filter(rank_ord_seq <= 3) 

res_t1 <- res_t1_ranks %>% 
  select(-ordered_seq) %>% spread(rank_ord_seq, n) %>%
  left_join(res_t1_ranks %>% select(-n) %>% spread(rank_ord_seq, ordered_seq), by = "portfolio") %>%
  left_join(res1_t1, by = "portfolio") %>%
  mutate(seq_rank1 = str_c(`1.y`," (", `1.x`,")"),
         seq_rank2 = str_c(`2.y`," (", `2.x`,")"),
         seq_rank3 = str_c(`3.y`," (", `3.x`,")")) %>%
  select(-(2:7)) %>%
  rowwise() %>% mutate(portfolio = bin_to_int_portfolio(portfolio)) %>% ungroup() %>%
  arrange(desc(n))

res_t2_ranks <- res2_t2 %>% 
  group_by(portfolio) %>% 
  mutate(rank_ord_seq = rank(desc(n), ties.method = "random")) %>% ungroup() %>% 
  arrange(portfolio, rank_ord_seq) %>%
  complete(portfolio, rank_ord_seq) %>%
  filter(rank_ord_seq <= 3) 

res_t2 <- res_t2_ranks %>% 
  select(-ordered_seq) %>% spread(rank_ord_seq, n) %>%
  left_join(res_t2_ranks %>% select(-n) %>% spread(rank_ord_seq, ordered_seq), by = "portfolio") %>%
  left_join(res1_t2, by = "portfolio") %>%
  mutate(seq_rank1 = str_c(`1.y`," (", `1.x`,")"),
         seq_rank2 = str_c(`2.y`," (", `2.x`,")"),
         seq_rank3 = str_c(`3.y`," (", `3.x`,")")) %>%
  select(-(2:7)) %>%
  rowwise() %>% mutate(portfolio = bin_to_int_portfolio(portfolio)) %>% ungroup() %>%
  arrange(desc(n))

# table 1
tab1_p1 <- res_t1 %>% select(portfolio, n, value, cost, mean_add, mean_rem) %>% slice(1:5)
tab1_p2 <- res_t2 %>% select(portfolio, n, value, cost, mean_add, mean_rem) %>% slice(1:5)
tab1 <- cbind(tab1_p1, tab1_p2)
print(xtable(tab1, digits = c(0,rep(c(0,0,0,0,1,1),2))), type = "latex")

# table 2
tab2_p1 <- res_t1 %>% select(portfolio, seq_rank1, seq_rank2, seq_rank3) %>% slice(1:5)
tab2_p2 <- res_t2 %>% select(portfolio, seq_rank1, seq_rank2, seq_rank3) %>% slice(1:5)
tab2 <- cbind(tab2_p1, tab2_p2)
print(xtable(tab2, type = "latex"))

##################################
# Figure 7: Proportion of project selections that were consistent with each heuristic
##################################

additions_with_heuristics_rand <- readRDS("behavioural_lab/output/results_simulated_random_selections.Rds")
additions_with_heuristics_obs <- readRDS("behavioural_lab/output/results_observed_selections.Rds")

### weighted proportions (heuristics can overlap)

rand <- additions_with_heuristics_rand %>% group_by(task, Sim_ID) %>%
  summarize(sum_unitv = mean(w_unitv),
            sum_value = mean(w_value),
            sum_cost = mean(w_cost),
            sum_addedv = mean(w_addedv),
            sum_addedv_mvp = mean(w_addedv_mvp),
            sum_other = mean(w_other))

rand_long <- rand %>% gather(heuristic, prop_using, -Sim_ID, -task) %>%
  mutate(heuristic = str_remove(heuristic, "sum_")) %>%
  filter(heuristic != "addedv_mvp") %>%
  filter(!(heuristic == "addedv" & task == 1))

obs <- additions_with_heuristics_obs %>% group_by(task) %>%
  summarize(sum_unitv = mean(w_unitv),
            sum_value = mean(w_value),
            sum_cost = mean(w_cost),
            sum_addedv = mean(w_addedv),
            sum_addedv_mvp = mean(w_addedv_mvp),
            sum_other = mean(w_other))

obs_long <- obs %>% gather(heuristic, prop_using, -task) %>%
  mutate(heuristic = str_remove(heuristic, "sum_")) %>%
  filter(heuristic != "addedv_mvp") %>%
  filter(!(heuristic == "addedv" & task == 1))

rand_long$heuristic <- factor(rand_long$heuristic, 
                              levels = c("unitv", "value", "cost", "addedv", "other"),
                              labels = c("Unit Value", "Highest Value", "Lowest Cost", "Added Value", "Other"))

obs_long$heuristic <- factor(obs_long$heuristic, 
                             levels = c("unitv", "value", "cost", "addedv", "other"),
                             labels = c("Unit Value", "Highest Value", "Lowest Cost", "Added Value", "Other"))

rand_long$task <- factor(rand_long$task, levels = c(1,2), labels = c("Task 1 (No interactions)", "Task 2 (With interactions)"))
obs_long$task <- factor(obs_long$task, levels = c(1,2), labels = c("Task 1 (No interactions)", "Task 2 (With interactions)"))

p1 <- ggplot(rand_long, aes(x = prop_using)) +
  geom_histogram(bins = 50) +
  geom_vline(data = obs_long, aes(xintercept = prop_using), colour = "red") +
  facet_grid(task ~ heuristic) +
  ylab("Density") + xlab("Weighted proportion of selections consistent with heuristic") +
  theme(axis.text.y = element_blank(), axis.text=element_text(size=20), 
        axis.title=element_text(size=20), strip.text = element_text(size = 14))
p1

ggsave("behavioural_lab/results/experiment_proportions.png", p1, width=12, height=6, dpi = 300)

# prop of simulations exceeding observed value (kind of p-value)
rand_long %>% left_join(obs_long, by = c("task", "heuristic")) %>% group_by(task, heuristic) %>%
  summarise(plevel = sum(prop_using.x > prop_using.y)) 

##################################
# prop of opportunities to fulfil an interaction subset that were taken 
##################################

additions_with_heuristics_obs %>% group_by(RespID, task) %>%
  mutate(Next_Projects = lead(Projects),
         Current_Portfolio = lead(Current_Portfolio)) %>% ungroup() %>% 
  rowwise() %>%
  mutate(Portfolio = str_c(unlist(Current_Portfolio), collapse = ""),
         P1_in = str_sub(Portfolio, 1, 1),
         P2_in = str_sub(Portfolio, 2, 2),
         P3_in = str_sub(Portfolio, 3, 3),
         P4_in = str_sub(Portfolio, 4, 4),
         P5_in = str_sub(Portfolio, 5, 5)) %>% 
  ungroup() %>% 
  filter(Projects != Next_Projects, !is.na(Next_Projects)) %>%
  mutate(opport_complete_subset = ifelse(  ((Projects == 1) & (P2_in == "0")) |
                                             ((Projects == 1) & (P3_in == "0")) |
                                             ((Projects == 2) & (P1_in == "0")) |
                                             ((Projects == 3) & (P1_in == "0")), 1, 0),
         complete_subset = ifelse(  ((Projects == 1) & (Next_Projects == 2)) |
                                      ((Projects == 1) & (Next_Projects == 3)) |
                                      ((Projects == 2) & (Next_Projects == 1)) |
                                      ((Projects == 3) & (Next_Projects == 1)), 1, 0),
         is_best = ifelse(((task == 1) & (is_unitv == 1))|((task == 2) & (is_addedv == 1)), 1, 0)) %>%
  filter(Projects <= 3) %>%
  group_by(task, Projects, Next_Projects, is_best) %>% summarize(n_opport = sum(opport_complete_subset),
                                                                 n_taken = sum(complete_subset)) %>%
  ungroup() %>% 
  group_by(task, is_best) %>% summarize(total_completing = sum(n_taken),
                                        total_selections = sum(n_opport),
                                        prop_completing = total_completing / total_selections) 

