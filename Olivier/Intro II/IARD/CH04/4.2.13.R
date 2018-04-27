## 4.2.13

m(list = ls())

p_bin <- c(r = 1.2, q = 1/4)
p_ln <- c(u = 2, s = 0.9)
p_pois <- c(l = 3)
p_weib <- c(t = 0.5,b = 1/4)

m <- 100000

set.seed(2013)
B <- matrix(0,nrow = m, ncol = 2)
M <- B


for(i in seq_len(m)){
    u <- runif(1)
    M[i,1] <- qnbinom(u,p_bin["r"],p_bin["q"])
    M[i,2] <- qpois(u,p_pois["l"])
    
    if(M[i,1] != 0){
        B[i,1] <- sum(qlnorm(runif(M[i,1]),p_ln["u"],p_ln["s"]))
    }
    
    if(M[i,2] != 0){
        B[i,2] <- sum(qweibull(runif(M[i,2]),p_weib["t"],p_weib["b"]))
    }
}

S <- apply(B,1,sum)


mean(S)
var(S)

repartition <- function(x) mean(S>x)

quantile(S,c(0.5,0.75,0.99,0.999,0.9999),type = 1)
