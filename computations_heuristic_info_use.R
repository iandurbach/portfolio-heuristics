## computes and plots the number of operations needed by the various heuristics we use.

library(tidyverse)

# function for computing number of k-way assessments needed by the added-value heuristic
# given total number of projects J and number already in portfolio n 
atb <- function(n,k,J){
  
  df <- ifelse(((k - 2) >= 0) & (n >= (k - 1)),choose(n-1, k-2), 0) * (J-n)
  return(df)
  
}

# function for computing number of k-way assessments needed by the added-value-most heuristic
# given total number of projects J and number already in portfolio n 
# gives upper and lower bounds
atb_mv <- function(n,k,J){
  
  df_low <- ifelse( ((k - 3) >= 0) & (n >= (k - 1)), choose(n-2, k-3), 0) * (J-n)
  df_hi <- ifelse( ((k - 2) >= 0) & (n >= (k - 1)), choose(n-1, k-2), 0) * (J-n)
  
  df_low <- ifelse((k==2) & (n==1), J - 1, df_low)
  
  return(list(df_low = df_low, df_hi = df_hi))
}

# create data frame containing results for various values of J, n, k
nops_res <- data.frame(J = integer(),
                       n = integer(),
                       k = integer(),
                       opt = integer(),
                       atb = double(),
                       atb_mv_low = double(),
                       atb_mv_hi = double())

J <- 50
for(n in 0:(J-1)){
  for(k in 2:5){
    this_atb <- atb(n,k,J)
    this_atb_mv <- atb_mv(n,k,J)
    this_opt <- choose(J,k)
    this_res <- data.frame(J = J,n = n,k = k, opt = this_opt,
                           atb = this_atb, atb_mv_low = this_atb_mv$df_low,
                           atb_mv_hi = this_atb_mv$df_hi)
    nops_res <- rbind.data.frame(nops_res, this_res)
    
  }
}


# calculate cumulative numbers of assessments made by each heuristic, plot these rather than
# marginals.
cum_ops <- nops_res %>% 
  group_by(J,k) %>% 
  mutate(atb = cumsum(atb),
         atb_mv_low = cumsum(atb_mv_low),
         atb_mv_hi = cumsum(atb_mv_hi),
         opt = mean(opt))

cum_ops$k = factor(cum_ops$k)
levels(cum_ops$k) = paste0(levels(cum_ops$k), "-way")

# there are a lot more 5-way assessments than 2-way assessments; to plot these nicely
# on one set of y-axes I standardize by total number of assessments i.e. show the proportion
# of the total number of possible interactions that must be assessed by each heuristic.

# but I do keep absolute numbers for later text plotting
abs_cum_ops <- cum_ops 

cum_ops <- cum_ops %>% mutate(atb = atb / opt,
                              atb_mv_low = atb_mv_low / opt,
                              atb_mv_hi = atb_mv_hi / opt,
                              opt = 1)

# reshape into long format for plotting
cum_ops_long <- cum_ops %>% gather(opt, atb, atb_mv_low, atb_mv_hi, 
                                   key = "model", value = "nops")

# added-value IS the upper bound of added-value-most, so don't need both
cum_ops_long <- cum_ops_long %>% filter(model != "atb_mv_hi")
cum_ops_long <- droplevels(cum_ops_long)

# now for plotting

# set the main aesthetic variables
p = ggplot(cum_ops_long, aes(x = n, 
                        y = nops, 
                        colour = model))

# envelope of best/worst performance
p = p + geom_ribbon(cum_ops, 
                    mapping = aes(x = n, ymin = atb_mv_low, ymax = atb_mv_hi),
                    inherit.aes = F,
                    fill = "grey70",
                    alpha = 0.5)

# add total number of interactions as text in TL corner
p = p + geom_text(filter(abs_cum_ops, n == 0),
                  mapping = aes(x = 0, y = 0.98, label = opt),
                  inherit.aes = F,
                  size = 8,
                  hjust = 0,
                  vjust = 1)

# plot heuristic performance with error bars, and nadir performance
p = p + 
  geom_line() +
  facet_grid(. ~ k) 

# few plot options, mainly resizing text
p = p + theme_bw(base_size=24) + 
  xlab("Iteration of heuristic (number of projects included)") + 
  ylab("Interactions assessed") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  theme(legend.position = "bottom", legend.text=element_text(size=20),
        legend.title=element_blank(), legend.key.size = unit(3, 'lines')) + 
  scale_color_manual(labels = c("Added-Value","Added-Value-Most (lower bound)", "Optimal"), values = 2:4) +
  guides(colour = guide_legend(nrow = 1)) 


p1 <- p

p1

# save
ggsave("results/number_ops.png", p1, width = 12, height = 5, dpi = 300)
