rm(list = ls())


# A) 

Qx1 <- function(u) 1/sqrt(-log(u))*1000
Qx2 <- function(u) 1/sqrt(1/u-1)*1000
Qx3 <- function(u) log(-2*(exp(log(2)*(u-1))-1))*-2000

# B)
m <- 100000
set.seed(20160419)
values <- matrix(0,ncol = 3, nrow = m)

for(i in seq_len(m)){
    values[i,1] <- Qx1(runif(1))
    values[i,2] <- Qx2(runif(1))
    values[i,3] <- Qx3(runif(1))
}
values[3,]

# C)
S <- apply(values,1,sum)
sum(S>20000)/m

# D)

TVaR <- function(k) mean(S[S > quantile(S,k,type = 1)] )
TVaR(0)
TVaR(0.9999)

# E)
apply(values[S > quantile(S,0.9999,type = 1),],2,mean)

# F)
noF <- apply(cbind(values[,1],values[,3]),1,sum)
sum(values[,2][noF <= 1500 | noF > 150000])/m