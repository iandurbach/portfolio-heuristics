###############################################################################
## Constant interactions are the same size no matter the project
## Proportional interactions depend on the size of the projects in the int'n
## Want constant and proportional interactions to be roughly equally sized 
## otherwise we confound the effect of interaction size and interaction type 
## Can use this code to calculate the values that all n-way proportional   
## interactions must be scaled by in order to that mean n-way prop int       
## = mean n-way constant int.
###############################################################################

#### user specifies mean value of constant interactions
mv_ci2 <- 5 # 2-way
mv_ci3 <- 5 # 3-way
mv_ci4 <- 5 # 4-way
mv_ci5 <- 5 # 5-way

#### function to get multiplier for proportional interactions
# bp: vector of project benefits
# intorder: how many projects involved in the interaction
# mv_ci: mean value of constant interaction of order 'intorder'
get_multiplier <- function(bp,intorder,mv_ci){
  # mean value of a intorder-way product of project benefits taken from bp
  mv_pi <- mean(replicate(10000,sum(bp[sample(1:length(bp),intorder)])))
  # hence to make equal to mean additive interaction of size ab, par must be:
  mult_pi <- mv_ci / mv_pi
  # output
  return(list(mv_pi = mv_pi, mult_pi = mult_pi))
}

#### function for applying the "negative skewing" transform to a dataset
neg_trform <- function(x){
  # this function as used by Simon
  max(x) - x + 0.1
}

#### checks on our data

## uniform data
load("liesio_data.RData")
bp <- x[,2]
ud2 <- get_multiplier(bp = bp,intorder = 2, mv_ci = mv_ci2) # 2-way
ud3 <- get_multiplier(bp = bp,intorder = 3, mv_ci = mv_ci3) # 3-way
ud4 <- get_multiplier(bp = bp,intorder = 4, mv_ci = mv_ci4) # 4-way
ud5 <- get_multiplier(bp = bp,intorder = 5, mv_ci = mv_ci5) # 5-way

## positively skew data
get_multiplier(bp = rgamma(50,5,2),intorder = 2, mv_ci = mv_ci2) # 2-way
get_multiplier(bp = rgamma(50,5,2),intorder = 3, mv_ci = mv_ci3) # 3-way
get_multiplier(bp = rgamma(50,5,2),intorder = 4, mv_ci = mv_ci4) # 4-way
get_multiplier(bp = rgamma(50,5,2),intorder = 5, mv_ci = mv_ci5) # 5-way
# note the multipliers here will be random cos of random data, take a resampled mean 
pd2 <- mean(replicate(50,get_multiplier(bp = rgamma(50,5,2),intorder = 2, mv_ci = mv_ci2)$mult_pi)) # 2-way
pd3 <- mean(replicate(50,get_multiplier(bp = rgamma(50,5,2),intorder = 3, mv_ci = mv_ci3)$mult_pi)) # 3-way
pd4 <- mean(replicate(50,get_multiplier(bp = rgamma(50,5,2),intorder = 4, mv_ci = mv_ci4)$mult_pi)) # 4-way
pd5 <- mean(replicate(50,get_multiplier(bp = rgamma(50,5,2),intorder = 5, mv_ci = mv_ci5)$mult_pi)) # 5-way

## negatively skew data
get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 2, mv_ci = mv_ci2) # 2-way
get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 3, mv_ci = mv_ci3) # 3-way
get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 4, mv_ci = mv_ci4) # 4-way
get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 5, mv_ci = mv_ci5) # 5-way
# note the multipliers here will be random cos of random data, take a resampled mean 
nd2 <- mean(replicate(50,get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 2, mv_ci = mv_ci2)$mult_pi)) # 2-way
nd3 <- mean(replicate(50,get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 3, mv_ci = mv_ci3)$mult_pi)) # 3-way
nd4 <- mean(replicate(50,get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 4, mv_ci = mv_ci4)$mult_pi)) # 4-way
nd5 <- mean(replicate(50,get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 5, mv_ci = mv_ci5)$mult_pi)) # 5-way

round(c(ud2$mult_pi,ud3$mult_pi,ud4$mult_pi,ud5$mult_pi),2) # multipliers for uniform data (2-way to 5-way)
round(c(pd2,pd3,pd4,pd5),2) # multipliers for pos skew data (2-way to 5-way)
round(c(nd2,nd3,nd4,nd5),2) # multipliers for neg skew data (2-way to 5-way)
