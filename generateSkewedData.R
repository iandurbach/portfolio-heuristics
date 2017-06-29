generateSkewedData <- function(n, shape, rate, inverse){
  value <- rgamma(n, shape, rate)
  if(inverse) value = max(value) + 0.1 - value
  costPerHect <- runif(n, 80, 120)
  cost <- value * costPerHect
  return(data.frame(cbind(value, cost)))
}

generateUniformData <- function(n, min, max){
  value <- runif(n, min, max)
  costPerHect <- runif(n, 80, 120)
  cost <- value * costPerHect
  return(data.frame(cbind(value, cost)))
}

for(i in 1:100){
  x <- generateUniformData(50, 0, 5)
  write.csv(x, paste("data/uniform_data_",i,".csv", sep = ""))
  x <- generateSkewedData(50, 0.5, 0.8, F)
  write.csv(x, paste("data/pos_skew_data_",i,".csv", sep = ""))
  x <- generateSkewedData(50, 0.5, 0.8, T)
  write.csv(x, paste("data/neg_skew_data_",i,".csv", sep = ""))
}



