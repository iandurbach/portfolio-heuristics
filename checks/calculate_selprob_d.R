#### choosing an exponent for modifying selection probabilities

# select projects with probability prop to (b/c)^d or (c/b)^d

## uniform data
load("liesio_data.RData")
cp <- x$cost
bp <- x[,2]
bcr_x <- sort(bp/cp)
plot(bcr_x,bcr_x/sum(bcr_x),type="l",ylim=c(0,0.2))
for(i in 2:8){lines(bcr_x,bcr_x^i/sum(bcr_x^i),col='red')}

## pos skew data
bp <- rgamma(50,5,2)
cp <- runif(50,min=0.8,max=1.2) * sum(bp)
bcr_x <- sort(bp/cp)
plot(bcr_x,bcr_x/sum(bcr_x),type="l",ylim=c(0,0.2))
for(i in 2:8){lines(bcr_x,bcr_x^i/sum(bcr_x^i),col='red')}

## neg skew data
bp <- rgamma(50,5,2)
bp <- max(bp) - bp + 0.1
cp <- runif(50,min=0.8,max=1.2) * sum(bp)
bcr_x <- sort(bp/cp)
plot(bcr_x,bcr_x/sum(bcr_x),type="l",ylim=c(0,0.2))
for(i in 2:8){lines(bcr_x,bcr_x^i/sum(bcr_x^i),col='red')}
