source("analyze_human_experiment/create_context.R")

responses_detail = as.data.frame(readxl::read_xlsx("../exp_data/portfolio_responses.xlsx", range = "A2:H721", col_names = T))


nRemovals = numRemovals(responses_detail = responses_detail)
nProjs = numProjects(responses_detail = responses_detail)
nProjs_numstages = data.frame(cbind(RespID = nProjs$RespID, numstages = apply(nProjs, FUN = function(x){sum(!is.na(x))}, MARGIN = 1)))
nProjs_gather =  nProjs %>% gather(stage, num_projs, -RespID)
nProjs_gather$stage = as.numeric(as.character(nProjs_gather$stage))
nProjs_gather$RespID = factor(nProjs_numstages$RespID, levels = nProjs_numstages$RespID[order(nProjs_numstages$numstages)])

p = ggplot(data = nProjs_gather, aes(x = stage, y = num_projs)) + geom_line() + theme_minimal() + facet_wrap(~RespID) + ylab("# projects") + xlab("stage") #+ theme(strip.text = element_blank())
ggsave("results/fig/numprojsperstage.png",p, width = 9, height = 7, dpi = 300)
