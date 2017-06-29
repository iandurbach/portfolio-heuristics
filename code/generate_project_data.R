####################
####################
### Function: generate_project_data
### Inputs:   nproj,minbc,maxbc,mincost,maxcost
### Author:   Ian Durbach (indurbach@gmail.com)
### Update:   25/2/2016
###
### Generates alternative/project data, specifically benefits and costs for 
### each of nproj alternatives.
####################
####################

generate_project_data = function(nproj,minbc,maxbc,mincp,maxcp){
  bc_ratio = runif(nproj,minbc,maxbc)
  cp = runif(nproj,mincp,maxcp)
  bp = cp * bc_ratio
  return(list(bp = bp, cp = cp))
}