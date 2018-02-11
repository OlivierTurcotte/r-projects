#######            2.3 EXERCICES DE DEPANNAGE
###------------------Numero 1 reste a faire viii et ix -----------------
esperance <- sum(1/B)
variance <- sum(1/B^2)
sigma <- sqrt(variance)

f <- function(x,B=B)
{
  z <- numeric(9)
  q <- numeric(10)
  r <- 0
  t <- 0
  for (i in 1:10)
  {
    r <- 0
    for (j in 1:10)
    {
      if(j != i)
      {
        r <- r+1
        z[r] <- B[j]/(B[j]-B[i])
      }
    }
    t <- t+1
    q[t] <- prod(z)*(B[i]*exp(-1*B[i]*x))
  }
  sum(q)
}
plot(Vectorize(f(x,B),vectorize.args = "x"))




###------------------Numero 2 sur papier
###------------------Numero 3 corrige----------------------------------------------------
rm(list=ls())
#a)
mu <- log(10) - 0.18
sigma <- 0.6
lambda <- 1/10
alpha <- 2
beta <- 1/5
kappa <- c(0.001,0.01,0.1,0.5,0.9,0.99,0.999)

m <- 1000000
set.seed(20160419)
z <- matrix(runif(3*m),ncol=3,byrow=T)
z[m,]

(EX1 <- exp(mu + (sigma^2)/2))
(EX2 <- 1/lambda)
(EX3 <- alpha/beta)
(varX1 <- exp(2*mu + sigma^2)*(exp(sigma^2)-1))
(varX2 <- 1/lambda^2)
(varX3 <- alpha/beta^2)
VaRX1 <- exp(mu+ sigma*qnorm(kappa))
VaRX2 <- -(1/lambda)*log(1-kappa)
VaRX3 <- sapply(kappa, function(y) uniroot(function(x) pgamma(x,alpha,beta)- y,c(0,1000))[[1]])
TVaRX1 <- (1/(1- kappa))* exp(mu+sigma^2/2)*(1-pnorm(qnorm(kappa)- sigma))
TVaRX2 <- qexp(kappa,lambda) + EX2
TVaRX3 <- (1/(1-kappa))*EX3*(1-pgamma(qgamma(kappa,alpha,beta),alpha+1,beta))
#b)

esperanceS <- EX1+EX2+EX3
varianceS <- varX1 + varX2 + varX3

#c)

x1 <- qlnorm(z[,1],mu,sigma)
x2 <- qexp(z[,2],lambda)
x3 <- qgamma(z[,3],alpha,beta)

#d

s <- x1 + x2 + x3

#e)
#esperance
mean(x1)
mean(x2)
mean(x3)
#variance
mean(x1^2) - mean(x1)^2
mean(x2^2) - mean(x2)^2
mean(x3^2)- mean(x3)^2
#VaR
y1 <- sort(x1)
y2 <- sort(x2)
y3 <- sort(x3)
y1[kappa*m]
y2[kappa*m]
y3[kappa*m]
VaR1 <- sapply(kappa,function(x) uniroot(function(y) mean(x1<y) - x , c(0,1000))[[1]])
VaR2 <-  sapply(kappa,function(x) uniroot(function(y) mean(x2<y) - x , c(0,1000))[[1]])
VaR3 <-  sapply(kappa,function(x) uniroot(function(y) mean(x3<y) - x , c(0,1000))[[1]])
 #TVaR
sapply(kappa,function(x) mean(y1[((x*m)+1):m]))
sapply(kappa,function(x) mean(y2[((x*m)+1):m]))
sapply(kappa,function(x) mean(y3[((x*m)+1):m]))

## f 

mean(s)
mean(s^2)-mean(s)^2
s1 <- sort(s)
s1[kappa*m]
VaRS <- sapply(kappa,function(y) uniroot(function(x) mean(s<x) -y,c(0,10000))[[1]])
sapply(kappa,function(x) mean(s1[((x*m)+1):m]))
TVaRS <- sapply(VaRS[1:7],function(x) mean(s[s>x]))

## g 

BMV <- VaRX1 + VaRX2 + VaRX3 - VaRS
BMT <- TVaRX1 + TVaRX2 + TVaRX3 - TVaRS




 







