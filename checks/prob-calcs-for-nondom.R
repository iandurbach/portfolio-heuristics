## calculates the proportion of available projects (projects not yet selected) that are 
## non-dominated. Generates random data (values, costs) that are correlated (rho). Then
## builds a portfolio using the PARETO heuristic, which randomly adds a non-dominated project
## to the existing portfolio. At each step, I evaluate the proportion of projects that are
## non-dominated, and plot the result.

## not used in final paper.

library(MASS)

N <- 30 # number of projects
rho <- -0 # desired correlation
Nruns <- 50

nproj <- rep(0, N)
nondom <- rep(0, N)

for(j in 1:Nruns){

  # generate correlated uniform RVs
  Sigma <- matrix(c(1,rho,rho,1),2,2)
  Sigma
  x <- mvrnorm(n = N, rep(0, 2), Sigma)
  
  px <- pnorm(x[,1])
  py <- pnorm(x[,2])
  
  # sort to make Pareto calcs quicker
  d = data.frame(px,py)
  D = d[order(d$px,d$py,decreasing=TRUE),]
  
  for(i in 1:N){
    
    # calculate set of non-dominated projects 
    front = D[which(!duplicated(cummin(D$py))),]
    
    # number of projects
    nproj[i] <- nproj[i] + nrow(D)
    # number of non-dominated projects
    nondom[i] <- nondom[i] + nrow(front)
    
    # select 1 non-dominated project and remove from consideration
    proj_pick <- sample(1:nrow(front), 1)
    D <- D[-which(D$px == front$px[proj_pick]),]
    
  }
  
}

nproj/Nruns
nondom/Nruns
nondom/nproj
plot(nondom/nproj, xlab = "N projects in portfolio", ylab = "Prop. remaining projects that are not dominated")
