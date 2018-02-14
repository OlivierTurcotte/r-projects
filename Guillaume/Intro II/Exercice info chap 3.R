#######EXERCICE INFO CHAPITRE 3 #####
###--------------------------Numero 1------
rm(list=ls())
#a sur papier
#b sur papier
#c)
r <- 5
q <- 1/5
M <- function(x)
{
  dnbinom(x,r,q)
}
sum(M(0:10000))
distribution <- function(x,n=100000)
{
  z <- numeric(n)
  for(i in 1:n)
  {
    z[i] <- pgamma(x,2*i,1/5000)*M(i)
  }
  (M(0) + sum(z))
}
distribution(500000,1e6)
#d)
VaR <- optimize(function(x) abs(distribution(x,10000)-0.99),c(0,600000))[[1]]
TVaR <- function(x,n)
{
  z <- numeric(n)
  for(i in 1:n)
  {
    z[i] <- (2*i)*5000 * (1 - pgamma(x,2*i + 1,1/5000))*M(i)
  }
  sum(z)/(1-0.99)
}
TVaR(VaR,1e6)
###--------------------------Numero 2-----------
rm(list=ls())
distribution <- function(x)
{
  0.8 + 0.2*pexp(x,1/2500)
}
distribution(VaR)
VaR <- optimize(function(x) abs(0.2 * pexp(x,1/2500) - 0.19),c(0,10000))[[1]]

qnorm(0.95)
###--------------------------Numero 3---------
rm(list=ls())
#a)
set.seed(20160419)
m <- 1000000
v <- matrix(runif(2*m),ncol=2,byrow=T)
v[m,]
theta <-qlnorm(v[,1],-0.5,1)
theta[1]
theta[m]
M <- qpois(v[,2],2*theta)
M[5]
M[m]
#b)
sum(sapply(0:120,function(x) sum(M==x)/m))
mean(M)
mean(M^2)-mean(M)^2







