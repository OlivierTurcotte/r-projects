# N1 :poisson homogène de paramètre lambda
# N2 :poisson conditionnel taux tau
# 
# tau ~ Exp(1/lambda)
# lambda = 3
# X ~ Erlang(n, jiji )

#1.1

lambda <- 1

X <- list()
X$params <- c(2,2)
X$density <- function(x) dgamma(x,X$params[1],X$params[2])

L <- list()
L$density <- function(x) dexp(x,lambda)

N1 <- list()
N1$density <- function(x,t) dpois(x,t*lambda)

N2 <- list()
N2$density <- function(x,t) mean(dpois(x,t*rexp(100000,lambda)))

# a)
sapply(1:5,function(t) N1$density(0:10,t))
sapply(1:5,function(t) sapply(0:10,function(x) N2$density(x,t)))

# b)


poids1 <- sapply(1:5,function(t) N1$density(0:100,t))
poids2 <- sapply(1:5,function(t) sapply(0:100,function(x) N2$density(x,t)))

# E[N1(t)], t = 1:5
E_N1 <- apply(sapply(1:5,function(t) sapply(0:100, function(x) x*poids1[x+1,t])),2,sum)
E_N2 <- apply(sapply(1:5,function(t) sapply(0:100, function(x) x*poids2[x+1,t])),2,sum)

# E[X]
mean(rgamma(100000,X$params[1],X$params[2]))

# c)
V_N1 <- apply(sapply(1:5,function(t) sapply(0:100, function(x) x^2*poids1[x+1,t])),2,sum) - E_N1^2
V_N2 <- apply(sapply(1:5,function(t) sapply(0:100, function(x) x^2*poids2[x+1,t])),2,sum) - E_N2^2

var(rgamma(100000,X$params[1],X$params[2]))

# d)
