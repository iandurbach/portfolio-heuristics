nextProject_random = function(context, current_portfolio){
  remaining_budget = context$budget - sum(current_portfolio * context$cp)
  in_budget = ifelse(context$cp <= remaining_budget, 1, 0)
  rel_dat = data.frame(cbind(proj = 1:length(context$cp), current_portfolio, in_budget))
  selection = filter(rel_dat, current_portfolio == 0 & in_budget == 1)
  selection$proj
}

nextProject_unitvalue = function(context, current_portfolio){
  uv = context$bp/context$cp
  remaining_budget = context$budget - sum(current_portfolio * context$cp)
  in_budget = ifelse(context$cp <= remaining_budget, 1, 0)
  rel_dat = data.frame(cbind(proj = 1:length(context$cp), uv, current_portfolio, in_budget))
  selection = filter(rel_dat, current_portfolio == 0 & in_budget == 1) %>% filter(uv == max(uv))
  selection$proj
}

nextProject_unitvalue_synergy = function(context, current_portfolio){
  uv = context$bp/context$cp
  synergy_projects = unique(unlist(context$ipp))
  synergy_indicator = rep(0, length(context$bp))
  synergy_indicator[synergy_projects] = 1
  remaining_budget = context$budget - sum(current_portfolio * context$cp)
  in_budget = ifelse(context$cp <= remaining_budget, 1, 0)
  rel_dat = data.frame(cbind(proj = 1:length(context$cp), uv, current_portfolio, in_budget, synergy_indicator))
  selection = filter(rel_dat, current_portfolio == 0 & in_budget == 1) %>% filter(synergy_indicator == 1) %>% filter(uv == max(uv))
  selection$proj
}

nextProject_addedvalue = function(context, current_portfolio){
  getNetValue = function(bp, Bi, current_portfolio, ipp){
    v = bp;
    ptc = project_to_complete(ipp, current_portfolio)
    extra_value = rep(0,length(current_portfolio))
    for (i in 1:length(ptc)){
      extra_value[ptc[i]] = extra_value[ptc[i]] + Bi[i]
    }
    v = bp + extra_value
    return(v)
  }
  net_value = getNetValue(context$bp, context$Bi, current_portfolio, context$ipp)
  uv = net_value/context$cp
  remaining_budget = context$budget - sum(current_portfolio * context$cp)
  in_budget = ifelse(context$cp <= remaining_budget, 1, 0)
  rel_dat = data.frame(cbind(proj = 1:length(context$cp), uv, current_portfolio, in_budget))
  selection = filter(rel_dat, current_portfolio == 0 & in_budget == 1) %>% filter(uv == max(uv))
  selection$proj
}


nextProject_lowcost = function(context, current_portfolio){
  remaining_budget = context$budget - sum(current_portfolio * context$cp)
  in_budget = ifelse(context$cp <= remaining_budget, 1, 0)
  rel_dat = data.frame(cbind(proj = 1:length(context$cp), cp = context$cp, current_portfolio, in_budget))
  selection = filter(rel_dat, current_portfolio == 0 & in_budget == 1) %>% filter(cp == min(cp))
  selection$proj
}

nextProject_highvalue = function(context, current_portfolio){
  remaining_budget = context$budget - sum(current_portfolio * context$cp)
  in_budget = ifelse(context$cp <= remaining_budget, 1, 0)
  rel_dat = data.frame(cbind(proj = 1:length(context$cp), bp = context$bp, current_portfolio, in_budget))
  selection = filter(rel_dat, current_portfolio == 0 & in_budget == 1) %>% filter(bp == max(bp))
  selection$proj
}
