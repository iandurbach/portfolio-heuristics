source("code/generate_project_data.R")
source("code/compute_selection_probs.R")
source("code/create_interdependencies.R")
source("code/compute_interdependent_BC.R")
source("code/solve_portfolio.R")
source("code/evaluate_z.R")
source("code/construct_random_portfolios.R")
source("code/take_the_best.R")
source("code/dominance.R")
source("code/lex.R")
source("analyze_human_experiment/create_context.R")
source("analyze_human_experiment/responses_detail_functions.R")
source("analyze_human_experiment/heuristic_functions.R")

library(Rglpk)

task = 1
context = loadContext(task)
if(task == 1){
  nproj = 10
  responses = readxl::read_xlsx("../exp_data/portfolio_responses.xlsx", range = "J2:Q74", col_names = T)
  responses_detail = as.data.frame(readxl::read_xlsx("../exp_data/portfolio_responses.xlsx", range = "A2:H721", col_names = T))
}else{
  nproj = 5
  responses = readxl::read_xlsx("../exp_data/Task2_Seq&Final_Submis.xlsx", range = "J2:M76", col_names = T)
  responses_detail = as.data.frame(readxl::read_xlsx("../exp_data/Task2_Seq&Final_Submis.xlsx", range = "A2:G559", col_names = T))
}

responses_order = orderFormat(responses_detail, nproj)
nRemovals = numRemovals(responses_detail)
nProjects = numProjects(responses_detail)
nProjects = data.frame(cbind(RespID = nProjects$RespID, numstages = apply(nProjects, FUN = function(x){sum(!is.na(x))}, MARGIN = 1)))
responses_order = data.frame(responses_order)
colnames(responses_order) = c("RespID", 1:nproj)
responses_order_gather = responses_order %>% gather(project, stage_added, -RespID)
responses_order_gather = subset(responses_order_gather, stage_added != 0)
responses_order_gather$project = as.numeric(as.character(responses_order_gather$project))

#compute heuristic's portfolios
opt = getPortfolio("opt", context)
optportfolio = which(opt$solution[1:nproj] == 1)
atbfull = lapply(1:100, function(j) which(getPortfolio("atb_full",context)$final_z == 1))
atbfullportfolio = which(getPortfolio("atb_full",context)$final_z == 1)
atbfullportfolio_order = getPortfolio("atb_full",context)$order_z

atb = lapply(1:100, function(j) which(getPortfolio("atb",context)$final_z == 1))
atbportfolio = which(getPortfolio("atb",context)$final_z == 1)
atbportfolio_order = getPortfolio("atb",context)$order_z

atv = lapply(1:100, function(j) which(getPortfolio("atv",context)$final_z == 1))
atvportfolio = which(getPortfolio("atv",context)$final_z == 1)
atvportfolio_order = getPortfolio("atv",context)$order_z

atc = lapply(1:100, function(j) which(getPortfolio("atc",context)$final_z == 1))
atcportfolio = which(getPortfolio("atc",context)$final_z == 1)
atcportfolio_order = getPortfolio("atc",context)$order_z

lex = lapply(1:100, function(j) which(getPortfolio("lex",context)$final_z == 1))
lexportfolio = which(getPortfolio("lex",context)$final_z == 1)
lexportfolio_order = getPortfolio("lex",context)$order_z

atbmvp = lapply(1:100, function(j) which(getPortfolio("atb_mvp",context)$final_z == 1))
atbmvpportfolio = which(getPortfolio("atb_mvp",context)$final_z == 1)
atbmvpportfolio_order = getPortfolio("atb_mvp",context)$order_z

atblvp = lapply(1:100, function(j) which(getPortfolio("atb_lvp",context)$final_z == 1))
atblvpportfolio = atblvp[1]

atbrvp = lapply(1:100, function(j) which(getPortfolio("atb_rvp",context)$final_z == 1))
atbrvpportfolio = atbrvp[1]

optportfolio
atbfullportfolio
atbportfolio
atvportfolio
atcportfolio
lexportfolio
atbmvpportfolio
atblvpportfolio
atbrvpportfolio

#visualize portfolio selections in order
heur_portfolios = data.frame()
heur_portfolios$heur = factor(levels = c("added value", "unit value", "highest value", "added value most"))
names(atbfullportfolio_order) = 1:nproj
names(atbportfolio_order) = 1:nproj
names(atvportfolio_order) = 1:nproj
names(atbmvpportfolio_order) = 1:nproj
heur_portfolios = rbind(heur_portfolios, c(heur = "added value", atbfullportfolio_order), 
                                         c(heur = "unit value", atbportfolio_order),
                                         c(heur = "highest value", atvportfolio_order),
                                         c(heur = "added value most", atbmvpportfolio_order))
colnames(heur_portfolios) = c("RespID", 1:nproj)
heur_portfolios_gather = heur_portfolios %>% gather(project, stage_added, -RespID)
heur_portfolios_gather = subset(heur_portfolios_gather, stage_added != 0)
heur_portfolios_gather$project = as.numeric(as.character(heur_portfolios_gather$project))
responses_order_gather$RespID = factor(responses_order_gather$RespID, levels = nProjects$RespID[order(nProjects$numstages)])
responses_order_gather = rbind(responses_order_gather, heur_portfolios_gather)
heur_panel_labeller <- function(string){
  string[nchar(string) < 5] = ""
  return(string)
}
pp = ggplot(responses_order_gather, aes(x = stage_added, y = project, group = RespID)) + 
  geom_line(alpha = 0.2) + geom_text(aes(x = stage_added, y = project,label = project), size = 3) +
  scale_y_continuous(breaks = 1:nproj, labels = c(1,"",3, "", 5, "", 7, "",9, "")) +
  facet_wrap(~RespID, labeller = labeller(RespID = heur_panel_labeller)) + theme_minimal() + theme(axis.text.y = element_text(size = 7), panel.grid.major.y = element_line(color = "grey"), strip.text = element_text(size = 10)) +
  xlab("stage") + ylab("project added") 
pp
ggsave("results/fig/order_detail.png", pp, width = 12, height = 12)

#compute matches in final portfolio selections
atbfull_matches = apply(responses, MARGIN = 1, FUN = comparePortfolio, portfolio = atbfullportfolio) %>% table
atb_matches = apply(responses, MARGIN = 1, FUN = comparePortfolio, portfolio = atbportfolio) %>% table
atv_matches = apply(responses, MARGIN = 1, FUN = comparePortfolio, portfolio = atvportfolio) %>% table
atmvp_matches = apply(responses, MARGIN = 1, FUN = comparePortfolio, portfolio = atbmvpportfolio) %>% table

#compute matches in participants with no removals
no_removals_ids = subset(nRemovals, nremovals == 0) %>% select(RespID) %>% c()
no_removals = subset(responses, RespID %in% no_removals_ids$RespID)
atbfull_matches_nr = apply(no_removals, MARGIN = 1, FUN = comparePortfolio, portfolio = atbfullportfolio) %>% table
atb_matches_nr = apply(no_removals, MARGIN = 1, FUN = comparePortfolio, portfolio = atbportfolio) %>% table
atv_matches_nr = apply(no_removals, MARGIN = 1, FUN = comparePortfolio, portfolio = atvportfolio) %>% table
atmvp_matches_nr = apply(no_removals, MARGIN = 1, FUN = comparePortfolio, portfolio = atbmvpportfolio) %>% table

#see detail of participants with no removals
no_removals_order = subset(responses_order, RespID %in% no_removals_ids$RespID)
no_removals_order_gather = subset(responses_order_gather, RespID %in% no_removals_ids$RespID)
no_removals_nProjects = subset(nProjects, RespID %in% no_removals_ids$RespID)
no_removals_order_gather$RespID = factor(no_removals_order_gather$RespID, levels = no_removals_nProjects$RespID[order(no_removals_nProjects$numstages)])
no_removals_order_gather = rbind(no_removals_order_gather, heur_portfolios_gather)
heur_panel_labeller <- function(string){
  string[nchar(string) < 5] = ""
  return(string)
}
pp = ggplot(no_removals_order_gather, aes(x = stage_added, y = project, group = RespID)) + 
  geom_line(alpha = 0.2) + geom_text(aes(x = stage_added, y = project,label = project), size = 3, ) +
  scale_y_continuous(breaks = 1:nproj, labels = c(1,"",3, "", 5, "", 7, "",9, "")) +
  facet_wrap(~RespID, labeller = labeller(RespID = heur_panel_labeller), ncol = 4) + theme_minimal() + theme(axis.text.y = element_text(size = 7), panel.grid.major.y = element_line(color = "grey"), strip.text = element_text(size = 10)) +
  xlab("stage") + ylab("project added") 
pp
ggsave("results/fig/no_removal_detail.png", pp, width = 6, height = 9)

atbfull_ordermatch = order_match(responses_order, atbfullportfolio_order)
atb_ordermatch = order_match(responses_order, atbportfolio_order)
atv_ordermatch = order_match(responses_order, atvportfolio_order)
atmvp_ordermatch = order_match(responses_order, atbmvpportfolio_order)

#####################
# analysis of additions conditional on current portfolio
#####################
responses_order
results = c()
results$total_additions = rep(0, ncol(responses_order)-1)
results$additions_uv = rep(0, ncol(responses_order)-1)
results$additions_suv = rep(0, ncol(responses_order)-1)
results$additions_av = rep(0, ncol(responses_order)-1)
results$additions_lc = rep(0, ncol(responses_order)-1)
results$additions_hv = rep(0, ncol(responses_order)-1)
results$additions_uv_av = rep(0, ncol(responses_order)-1)
results$additions_uv_hv = rep(0, ncol(responses_order)-1)
results$additions_uv_lc = rep(0, ncol(responses_order)-1)
results$additions_av_lc = rep(0, ncol(responses_order)-1)
heuristics_choice_set_size = data.frame()
colnames(heuristics_choice_set_size) = c("stage", "heuristic", "set_size")
additions_detail = data.frame()
colnames(additions_detail) = c("stage", "added_value", "unit_value", "highest_value", "lowest_cost")

for(i in 1:nrow(responses_order)){
  for(stage in 1:max(responses_order[i,2:ncol(responses_order)])){
    currportfolio = as.numeric(unname(responses_order[i,2:ncol(responses_order)]))
    curr_selection = which(currportfolio == stage)
    currportfolio[currportfolio >= stage] = 0
    currportfolio[currportfolio > 0] = 1
    rownames(currportfolio) = c()
    proj_rand = nextProject_random(context, currportfolio)
    proj_uv = nextProject_unitvalue(context, currportfolio)
    proj_suv = nextProject_unitvalue_synergy(context, currportfolio)
    proj_av = nextProject_addedvalue(context, currportfolio)
    proj_lc = nextProject_lowcost(context, currportfolio)
    proj_hv = nextProject_highvalue(context, currportfolio)
    heuristics_choice_set_size = bind_rows(heuristics_choice_set_size, c(stage = stage, heuristic = "unit value", set_size = length(proj_uv)))
    #heuristics_choice_set_size = bind_rows(heuristics_choice_set_size, c(stage = stage, heuristic ="unit value synergy", set_size =length(proj_suv)))
    heuristics_choice_set_size = bind_rows(heuristics_choice_set_size, c(stage = stage, heuristic ="added value", set_size =length(proj_av)))
    heuristics_choice_set_size = bind_rows(heuristics_choice_set_size, c(stage = stage, heuristic ="lowest cost", set_size =length(proj_lc)))
    heuristics_choice_set_size = bind_rows(heuristics_choice_set_size, c(stage = stage, heuristic ="highest value", set_size =length(proj_hv)))
    heuristics_choice_set_size = bind_rows(heuristics_choice_set_size, c(stage = stage, heuristic ="alternatives\nwithin budget", set_size =length(proj_rand)))
    results$total_additions[stage] = results$total_additions[stage] + 1
    if(length(proj_rand) == 0) print(sum(currportfolio * context$cp))
    results$random_expectedvalue[stage] = results$random_expectedvalue[stage] + (1/length(proj_rand))
    all_heurs = union(proj_uv, union(proj_suv, union(proj_av, union(proj_lc, proj_hv))))
    all_heurs_ex_uv = union(proj_av, union(proj_lc, proj_hv))
    all_heurs_ex_av = union(proj_uv, union(proj_lc, proj_hv))
    all_heurs_ex_lc = union(proj_uv, union(proj_av, proj_hv))
    all_heurs_ex_hv = union(proj_uv, union(proj_av, proj_lc))
    if((curr_selection %in% proj_uv) & !(curr_selection %in% all_heurs_ex_uv)) results$additions_uv[stage] = results$additions_uv[stage] + 1
    #if((curr_selection %in% proj_suv) & !(curr_selection %in% setdiff(all_heurs,proj_suv))) results$additions_suv[stage] = results$additions_suv[stage] + 1
    if((curr_selection %in% proj_av) & !(curr_selection %in% all_heurs_ex_av)) results$additions_av[stage] = results$additions_av[stage] + 1
    if((curr_selection %in% proj_lc) & !(curr_selection %in% all_heurs_ex_lc)) results$additions_lc[stage] = results$additions_lc[stage] + 1
    if((curr_selection %in% proj_hv) & !(curr_selection %in% all_heurs_ex_hv)) results$additions_hv[stage] = results$additions_hv[stage] + 1
    if((curr_selection %in% union(proj_uv, proj_av))) results$additions_uv_av[stage] = results$additions_uv_av[stage] + 1
    if((curr_selection %in% union(proj_uv, proj_hv))) results$additions_uv_hv[stage] = results$additions_uv_hv[stage] + 1
    if((curr_selection %in% union(proj_av, proj_hv))) results$additions_av_hv[stage] = results$additions_av_hv[stage] + 1
    additions_detail = bind_rows(additions_detail, c(stage = stage, 
                                                     added_value = curr_selection %in% proj_av,
                                                     unit_value = curr_selection %in% proj_uv, 
                                                     lowest_cost = curr_selection %in% proj_lc,
                                                     highest_value = curr_selection %in% proj_hv))
  }
}
results
additions_detail

#First number is stage, remaining numbers mean "added_value", "unit_value", "lowest_cost", "highest_value", correspondingly.
table(apply(additions_detail, 1, paste, collapse = ""))

#choice set size:
heuristics_choice_set_size$set_size = as.numeric(as.character(heuristics_choice_set_size$set_size))
heuristics_choice_set_size$heuristic = factor(heuristics_choice_set_size$heuristic, levels =  c("alternatives\nwithin budget", "added value", "unit value", "lowest cost", "highest value"))
ggplot(data = heuristics_choice_set_size, aes(x = set_size)) + geom_histogram(stat = "count") + facet_grid(stage~heuristic) + theme_minimal() + scale_x_continuous(breaks = c(1,5,10))


#####################
# analysis of first addition to portfolio
#####################

proj_uv = nextProject_unitvalue(context, rep(0, nproj))
proj_suv = nextProject_unitvalue_synergy(context, rep(0, nproj))
proj_av = nextProject_addedvalue(context, rep(0, nproj))
proj_lc = nextProject_lowcost(context, rep(0, nproj))
proj_hv = nextProject_highvalue(context, rep(0, nproj))

text_heurs = data.frame(rbind(cbind(heur = "unit value", x = proj_uv, y = 37),
                       cbind(heur = "added value", x = proj_av, y = 35),
                       cbind(heur = "highest value", x = proj_hv, y = 30),
                       cbind(heur = "lowest cost", x = proj_lc, y = 33)))
text_heurs$x = as.numeric(as.character(text_heurs$x))
text_heurs$y = as.numeric(as.character(text_heurs$y))
firstAddition = firstAddition(responses_detail = responses_detail, nproj)
fa_plot = ggplot(data = firstAddition, aes(x = Response_1)) + geom_histogram(stat = "count") + 
  theme_minimal() + 
  scale_x_continuous(breaks = c(1:10)) + 
  ylim(c(0, 40)) +
  xlab("first project added") + 
  ylab("# participants") + 
  geom_text(data = text_heurs, aes(x = x, y =y, label = heur), size = 4) 
fa_plot
  

