#Q01
rm(list = ls())
library(actuar)

# A)
x <- 164.8721
y <- 46707.74

a_par  <- -(2*y/x^2)/(1-y/x^2)
l_par <- x*(a_par-1)

a_gam <- x^2/y
b_gam <- x/y

s_log <- sqrt(log(y/x^2+1))
u_log <- log(x)-s_log^2/2

# B)
m <- 1000000
set.seed(20160419)
values <- matrix(0,nrow = m,ncol = 3)
for(i in seq(m)){
    values[i,1] <- qpareto(runif(1),a_par,l_par)
    values[i,2] <- qgamma(runif(1),a_gam,b_gam)
    values[i,3] <- qlnorm(runif(1),u_log,s_log)
}
values[m,]

# C)
S <- apply(values,1,sum)
S[3]

# D)
mean(S > 1500)

# E)
quantile(S,0.999,type = 1) # Vérif
v <- sort(S)[m*0.999]

# F)
mean(S[S>v])

# G)
values[S == v,]

# H)
apply(values[S > v,],2,mean)

# I)
x + y/sqrt(3*y)*qnorm(0.999)

