rm(list=ls())
# Paramètres ----
library(actuar)

lambda <- 2                 # Pois
alpha <- 1.5 ; beta <- 50   # Pareto
kappa <- c(0.1,0.5,0.9,0.99,0.999,0.9999)

# Réponses ----

no_a <- lambda*beta/(alpha-1) # La variance n'existe pas pour alpha < 2...

set.seed(2018)
n <- 1e6
M <- numeric(n)
X <- numeric(n)
for(i in seq(n)){
    M[i] <- qpois(runif(1),lambda)
    if(M[i]>0) X[i] <- sum(qpareto(runif(M[i]),alpha,beta))
}
no_i    <- mean(X)
no_ii   <- sapply(seq(0,1000,100),function(i) mean(X > i))
no_iii  <- sapply(seq(0,1000,100), function(i) sum(X[X>i]-i)/n)
no_iv   <- quantile(X,kappa)
no_v    <- sapply(no_iv,function(i) mean(X[X>i]))
