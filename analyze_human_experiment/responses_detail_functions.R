orderFormat <- function(responses_detail, nproj){
  #prepare order of responses
  responses_order = c()
  for(resp in unique(responses_detail$RespID)){
    re = subset(responses_detail, RespID == resp, select = -c(EntryID))
    responses_order_instance = rep(0, nproj)
    order = 1
    for(i in 1:nrow(re)){
      if(i == 1){
        responses_order_instance[as.integer(re[1,'Response_1'])] = 1
        order = order + 1
      }else{
        diff = setdiff(unlist(re[i-1,]), unlist(re[i,]))
        diff = diff[!is.na(diff)]
        if(length(diff) > 0){ 
          nremovals = length(diff)
          order_removed = responses_order_instance[as.integer(diff)]
          responses_order_instance[as.integer(diff)] = 0
          #print(paste0("removed ",diff))
          responses_order_instance[responses_order_instance > order_removed] = responses_order_instance[responses_order_instance > order_removed] - nremovals
          order = order - nremovals
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

firstAddition <- function(responses_detail, nproj){
  first_addition = c()
  responses_detail %>% group_by(RespID) %>% filter(is.na(Response_2) & row_number()==1) %>% select(c(RespID, Response_1))
}

numProjects = function(responses_detail){
  numProjects = c()
  for(resp in unique(responses_detail$RespID)){
    re = subset(responses_detail, RespID == resp, select = -c(EntryID))
    num_projects_instance = c()
    for(i in 1:nrow(re)){
      num_projects_instance = c(num_projects_instance, sum(!is.na(unlist(re[i,])))-1)
    }
    names(num_projects_instance) = 1:length(num_projects_instance)
    numProjects = bind_rows(numProjects, c(RespID = resp, num_projects_instance))
  }
  numProjects
}

numRemovals = function(responses_detail){
  numRemovals = c()
  for(resp in unique(responses_detail$RespID)){
    re = subset(responses_detail, RespID == resp, select = -c(EntryID))
    num_removals_instance = 0
    for(i in 1:nrow(re)){
      if(i == 1){
      }else{
        diff = setdiff(unlist(re[i-1,]), unlist(re[i,]))
        diff = diff[!is.na(diff)]
        num_removals_instance = num_removals_instance + length(diff)
      }
    }
    numRemovals = rbind(numRemovals, c(RespID = resp, nremovals = num_removals_instance))
  }
  data.frame(numRemovals)
}

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

#returns the number of differing projects between x and portfolio
comparePortfolio = function(x, portfolio){
  length(setdiff(x[3:8], portfolio)) + length(setdiff(portfolio, x[3:8]))
}