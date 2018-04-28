rm(list= ls())
al <- 0.001
be <- 0.00004
ga <- log(1.09)

prime <- 1000
x <- 30

b <- function(x) be*exp(ga*x)
Qyx <- function(x,u) log(log(1-u)*ga/-b(x)+1)/ga

v <- function(t) exp(-0.03*t)

# B)
set.seed(20160419)
m <- 100000
values <- matrix(0,ncol = 2, nrow = m)
for(i in seq(m)){
    values[i,1] <- qexp(runif(1),al)
    values[i,2] <- Qyx(x,runif(1))
}
Tx <- apply(values,1,min)

# C)
mean(Tx)

# D)
sum(values[,1] < values[,2])/m

# E)
Z <- prime*sapply(Tx,v)
Z[1]

# F)
mean(Z)