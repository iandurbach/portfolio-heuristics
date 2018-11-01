source("analyze_human_experiment/create_context.R")
source("code/lex.R")
library(Rglpk)

responses = readxl::read_xlsx("../exp_data/portfolio_responses.xlsx", range = "J2:Q74", col_names = T)

opt = getPortfolio("opt", context)
optportfolio = which(opt$solution[1:10] == 1)
atbfull = lapply(1:100, function(j) which(getPortfolio("atb_full",context)$final_z == 1))
atbfullportfolio = atbfull[1]

atb = lapply(1:100, function(j) which(getPortfolio("atb",context)$final_z == 1))
atbportfolio = atb[1]

atv = lapply(1:100, function(j) which(getPortfolio("atv",context)$final_z == 1))
atvportfolio = atv[1]

atc = lapply(1:100, function(j) which(getPortfolio("atc",context)$final_z == 1))
atcportfolio = atc[1]

lex = lapply(1:100, function(j) which(getPortfolio("lex",context)$final_z == 1))
lexportfolio = lex[1]

atbmvp = lapply(1:100, function(j) which(getPortfolio("atb_mvp",context)$final_z == 1))
atbmvpportfolio = atbmvp[1]

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
