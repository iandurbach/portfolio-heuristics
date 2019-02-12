source("simulation/experiment_master_fns.R")
#
# GENERATE SIMULATED DATASETS (RUNNING THIS WILL OVERWRITE EXISTING DATASETS!)
#
# generates 100 project values and costs datasets and save these as csv files.
for(i in 1:100){
  x <- generateUniformData(50, 0, 20)
  write.csv(x, paste("simulation/data/uniform_data_",i,".csv", sep = ""))
  x <- generateSkewedData(50, 5, 2, T)
  write.csv(x, paste("simulation/data/pos_skew_data_",i,".csv", sep = ""))
  #x <- generateSkewedData(50, 0.5, 0.8, T)
  x <- generateSkewedData(50, 5, 2, F)
  write.csv(x, paste("simulation/data/neg_skew_data_",i,".csv", sep = ""))
}

##############################
# Running experiments (takes a long time!)
##############################

# SKEWED DATA
#
#nCV = number of constraint violations when choosing a random or a greedy portfolio.
# 
filepath = "simulation/data/pos_skew_data_"
suffix = "psk"
all_res = runExperiments(100)
#all_res = value_decomposition(100)
#d = dominance_analysis(15)
filepath = "data/neg_skew_data_"
suffix = "neg"
all_res = runExperiments(100)
#all_res = value_decomposition(100)
#d = dominance_analysis(15)

filepath = "simulation/data/uniform_data_"
suffix = "uni"
all_res = runExperiments(100)
#all_res = value_decomposition(100)
#d = dominance_analysis(15)

#source("results/makedata.R")