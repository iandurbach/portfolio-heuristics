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

library(Rglpk)
orderFormat <- function(responses_detail){
  #prepare order of responses
  responses_order = c()
  for(resp in unique(responses_detail$RespID)){
    re = subset(responses_detail, RespID == resp, select = -c(EntryID))
    responses_order_instance = rep(0, 10)
    order = 1
    for(i in 1:nrow(re)){
      if(i == 1){
        responses_order_instance[as.integer(re[1,'Response_1'])] = 1
        order = order + 1
      }else{
        diff = setdiff(unlist(re[i-1,]), unlist(re[i,]))
        diff = diff[!is.na(diff)]
        if(length(diff) > 0){ 
          order_removed = responses_order_instance[as.integer(diff)]
          responses_order_instance[as.integer(diff)] = 0
          #print(paste0("removed ",diff))
          responses_order_instance[responses_order_instance > order_removed] = responses_order_instance[responses_order_instance > order_removed] - 1
          order = order - 1
        }
        diff = setdiff(unlist(re[i,]), unlist(re[i-1,]))
        diff = diff[!is.na(diff)]
        if(length(diff) > 0){
          responses_order_instance[as.integer(diff)] = order
          #print(paste0("added ",diff, " order:", order))
          order = order + 1
          if(length(diff) == 2){ #are there two projects added in same step?
            responses_order_instance[as.integer(diff)[2]] = order
            #print(paste0("added ",diff, " order:", order))
            order = order + 1
          }
        }
      }
    }
    responses_order = rbind(responses_order, c(RespID = re$RespID[1], responses_order_instance))
  }
  responses_order
}
responses = readxl::read_xlsx("../exp_data/portfolio_responses.xlsx", range = "J2:Q74", col_names = T)
responses_detail = as.data.frame(readxl::read_xlsx("../exp_data/portfolio_responses.xlsx", range = "A2:H721", col_names = T))
responses_order = orderFormat(responses_detail)

opt = getPortfolio("opt", context)
optportfolio = which(opt$solution[1:10] == 1)
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

#returns an array with the number of projects that select the same projects in same order.
order_match = function(responses_order, portfolio_order = atbfullportfolio_order){
  order_match = c()
  subset_resporder = responses_order
  for(i in 1:max(portfolio_order)){
    if(nrow(subset_resporder) == 1){
      if((which(subset_resporder == i) - 1) != which(portfolio_order == i)){
        subset_resporder = data.frame()
      }
    }else{
      subset_resporder = subset(subset_resporder, unlist(apply(subset_resporder, FUN = function(x){which(x == i) - 1}, MARGIN = 1)) == which(portfolio_order == i))
    }
    order_match = c(order_match, nrow(subset_resporder))
  }
  order_match
}

#returns the percentage of match between x and portfolio.
comparePortfolio = function(x, portfolio){
  length(intersect(x[3:8], portfolio))/length(portfolio)
}

atbfull_matches = apply(responses, MARGIN = 1, FUN = comparePortfolio, portfolio = atbfullportfolio[[1]]) %>% table
atb_matches = apply(responses, MARGIN = 1, FUN = comparePortfolio, portfolio = atbportfolio[[1]]) %>% table
atv_matches = apply(responses, MARGIN = 1, FUN = comparePortfolio, portfolio = atvportfolio[[1]]) %>% table
atmvp_matches = apply(responses, MARGIN = 1, FUN = comparePortfolio, portfolio = atbmvpportfolio[[1]]) %>% table

match = data.frame(rbind(cbind(heur = "Added Value", num = atbfull_matches, match = names(atbfull_matches)),
              cbind(heur = "Unit Value", num = atb_matches, match = names(atb_matches)),
              cbind(heur = "Highest Value", num = atv_matches, match = names(atv_matches)),
              cbind(heur = "Added Value Most", num = atmvp_matches, match = names(atmvp_matches))))
match$num = as.numeric(as.character(match$num))
match$match = as.numeric(as.character(match$match))
match = match %>% group_by(heur) %>% mutate(cumsumnum = rev(cumsum(rev(num)))) 
p1 = ggplot(data = match, aes(colour = heur, fill = heur, shape = heur, x = match, y = cumsumnum)) + geom_point() + geom_line() + theme_minimal() + ylab("# responses") + xlab("min match") + theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(2, 'lines'))+ 
  guides(colour = guide_legend(nrow = 2))  
ggsave("results/fig/minmatch.png",p1, width = 4, height = 5, dpi = 300)

atbfull_ordermatch = order_match(responses_order, atbfullportfolio_order)
atb_ordermatch = order_match(responses_order, atbportfolio_order)
atv_ordermatch = order_match(responses_order, atvportfolio_order)
atmvp_ordermatch = order_match(responses_order, atbmvpportfolio_order)
