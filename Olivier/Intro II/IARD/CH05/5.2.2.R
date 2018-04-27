## 5.2.2 - Olivier Turcotte 

rm(list = ls())
## Parameters ----
p_pareto <- c(a =18/8, l =100*(18/8-1))
p_gamma <- c(a =100^2/300^2, b =100/300^2)
p_ln <- c(u = log(100)-log(10)/2, sd =sqrt(log(10)))

## Simulation ----
library(actuar)

set.seed(2018)
m <- 100000
V <- matrix(0,ncol = 4,nrow = m)
for(i in seq_len(m)){
    u <- runif(1)
    V[i,1] <- qpareto(u,p_pareto["a"],p_pareto["l"])
    V[i,2] <- qgamma(u,p_gamma["a"],p_gamma["b"])
    V[i,3] <- qlnorm(u,p_ln["u"],p_ln["sd"])
}
V[,4] <- apply(V,1,sum) ## Somme des X


mean(V[,1]) ; mean(V[,2]) ; mean(V[,3]) ## Vérification paramètres


## (a) ----

## 
kappa <- c(0.9,0.99)
(VaR_S <- quantile(V[,4],kappa,type = 1))
c <- sapply(1:3,function(i) sort(V[,i])[m*kappa])

(sum(c[1,]) == VaR_S[1])[[1]]
(sum(c[2,]) == VaR_S[2])[[1]]## Vérificaion ok.

## (b) ----
(TVaR_S <- sapply(VaR_S,function(x) mean(V[,4][V[,4] > x])))
c0.9 <- sapply(1:3,function(i) mean(V[,i][V[,4] > VaR_S[1]]))
c0.99 <- sapply(1:3,function(i) mean(V[,i][V[,4] > VaR_S[2]]))
data.frame(TVaR_S[1], c0.9,TVaR_S[2],c0.99)
