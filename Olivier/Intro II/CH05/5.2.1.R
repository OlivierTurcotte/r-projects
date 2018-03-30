## 5.2.1 - Olivier Turcotte

rm(list = ls())

# Parameters ----
a <- c(10,4,1)
b <- c(1/300,1/500,1/1000)

# Simulation ----

set.seed(2018)
m <- 100000
V <- matrix(0,ncol = 4,nrow = m)
for(i in seq_len(m)){
    u <- runif(1)
    V[i,1:3] <- qgamma(u,a,b)
}
V[,4] <- apply(V,1,sum) ## Somme des X

## (a) ----

## 
kappa <- c(0.9,0.95)
(VaR_S <- quantile(V[,4],kappa,type = 1))
c <- sapply(1:3,function(i) sort(V[,i])[m*kappa])

(sum(c[1,]) == VaR_S[1])[[1]]
(sum(c[2,]) == VaR_S[2])[[1]]## Vérificaion ok.

## (b) ----
(TVaR_S <- sapply(VaR_S,function(x) mean(V[,4][V[,4] > x])))
c0.9 <- sapply(1:3,function(i) mean(V[,i][V[,4] > VaR_S[1]]))
c0.95 <- sapply(1:3,function(i) mean(V[,i][V[,4] > VaR_S[2]]))

## Ratios servent à rien.
