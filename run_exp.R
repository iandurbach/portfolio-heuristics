source("experiments.R")
#
# SKEWED DATA
#
#nCV = number of constraint violations when choosing a random or a greedy portfolio.
#
filepath = "data/pos_skew_data_"
suffix = "psk"
all_res = runExperiments(100)
#all_res = value_decomposition(100)
#d = dominance_analysis(15)
filepath = "data/neg_skew_data_"
suffix = "neg"
all_res = runExperiments(100)
#all_res = value_decomposition(100)
#d = dominance_analysis(15)

filepath = "data/uniform_data_"
suffix = "uni"
all_res = runExperiments(100)
#all_res = value_decomposition(100)
#d = dominance_analysis(15)

#source("results/makedata.R")