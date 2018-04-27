rm(list = ls())
# Paramètres ----
r <- 0.5 ; q <- 0.2             # NBinom
mu <- log(100)-1/2 ; sigma <- 1 # LNorm
kappa <- c(0.1,0.5,0.9,0.99,0.999,0.9999)
# Réponses ----

no_a <- c(r*(1-q)/q*exp(mu+(sigma^2)/2), 
          r*(1-q)/q^2*exp(mu+sigma^2/2)^2+r*(1-q)/q*exp(2*mu+sigma^2)*(exp(sigma^2)-1))

set.seed(2018)
n <- 1e6
M <- numeric(n)
B <- numeric(n)
for(i in seq(n)){
    M[i] <- qnbinom(runif(1),r,q)
    if(M[i]>0){
        B[i] <- sum(qlnorm(runif(M[i]),mu,sigma))
    }
}
SL <- function(d) sum(B[B>d]-d)/n
no_i <- c(mean(B),mean(B^2)-mean(B)^2)
no_ii <- sapply(seq(0,1000,100),function(i) mean(B > i))
no_iii <- sapply(seq(0,1000,100),SL)
no_iv <- quantile(B,kappa)
no_v <- sapply(seq(kappa),function(i) mean(B[B>no_iv[i]]))

