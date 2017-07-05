###############################################################################
## Want additive and multiplicative interactions to be roughly equally sized ##
## otherwise we confound the effect of interaction size and interaction type ##
## Can use this code to calculate the values that all n-way multiplicative   ##
## interactions must be scaled by in order to that mean n-way mult int       ##
## = mean n-way add int.
###############################################################################

#### user specifies mean value of additive interactions
mv_ai2 <- 5 # 2-way
mv_ai3 <- 5 # 3-way
mv_ai4 <- 5 # 4-way
mv_ai5 <- 5 # 5-way

#### function to get multiplier for multiplicative interactions
# bp: vector of project benefits
# intorder: how many projects involved in the interaction
# mv_ai: mean value of additive interaction of order 'intorder'
get_multiplier <- function(bp,intorder,mv_ai){
  # mean value of a intorder-way product of project benefits taken from bp
  mv_mi <- mean(replicate(10000,prod(bp[sample(1:length(bp),intorder)])))
  # hence to make equal to mean additive interaction of size ab, par must be:
  mult_mi <- mv_ai / mv_mi
  # output
  return(list(mv_mi = mv_mi, mult_mi = mult_mi))
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
ud2 <- get_multiplier(bp = bp,intorder = 2, mv_ai = mv_ai2) # 2-way
ud3 <- get_multiplier(bp = bp,intorder = 3, mv_ai = mv_ai3) # 3-way
ud4 <- get_multiplier(bp = bp,intorder = 4, mv_ai = mv_ai4) # 4-way
ud5 <- get_multiplier(bp = bp,intorder = 5, mv_ai = mv_ai5) # 5-way

## positively skew data
get_multiplier(bp = rgamma(50,5,2),intorder = 2, mv_ai = mv_ai2) # 2-way
get_multiplier(bp = rgamma(50,5,2),intorder = 3, mv_ai = mv_ai3) # 3-way
get_multiplier(bp = rgamma(50,5,2),intorder = 4, mv_ai = mv_ai4) # 4-way
get_multiplier(bp = rgamma(50,5,2),intorder = 5, mv_ai = mv_ai5) # 5-way
# note the multipliers here will be random cos of random data, take a resampled mean 
pd2 <- mean(replicate(50,get_multiplier(bp = rgamma(50,5,2),intorder = 2, mv_ai = mv_ai2)$mult_mi)) # 2-way
pd3 <- mean(replicate(50,get_multiplier(bp = rgamma(50,5,2),intorder = 3, mv_ai = mv_ai3)$mult_mi)) # 3-way
pd4 <- mean(replicate(50,get_multiplier(bp = rgamma(50,5,2),intorder = 4, mv_ai = mv_ai4)$mult_mi)) # 4-way
pd5 <- mean(replicate(50,get_multiplier(bp = rgamma(50,5,2),intorder = 5, mv_ai = mv_ai5)$mult_mi)) # 5-way

## negatively skew data
get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 2, mv_ai = mv_ai2) # 2-way
get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 3, mv_ai = mv_ai3) # 3-way
get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 4, mv_ai = mv_ai4) # 4-way
get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 5, mv_ai = mv_ai5) # 5-way
# note the multipliers here will be random cos of random data, take a resampled mean 
nd2 <- mean(replicate(50,get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 2, mv_ai = mv_ai2)$mult_mi)) # 2-way
nd3 <- mean(replicate(50,get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 3, mv_ai = mv_ai3)$mult_mi)) # 3-way
nd4 <- mean(replicate(50,get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 4, mv_ai = mv_ai4)$mult_mi)) # 4-way
nd5 <- mean(replicate(50,get_multiplier(bp = neg_trform(rgamma(50,5,2)),intorder = 5, mv_ai = mv_ai5)$mult_mi)) # 5-way

c(ud2$mult_mi,ud3$mult_mi,ud4$mult_mi,ud5$mult_mi) # multipliers for uniform data (2-way to 5-way)
c(pd2,pd3,pd4,pd5) # multipliers for pos skew data (2-way to 5-way)
c(nd2,nd3,nd4,nd5) # multipliers for neg skew data (2-way to 5-way)
