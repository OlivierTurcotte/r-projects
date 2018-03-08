########## Examen 2017 ###############
#a)
r <- 0.4
q <- 10/11
alpha <- 1.5
beta <- alpha/1000
Repartition <- function(x)
{
  dnbinom(0,r,q) + sum(sapply(seq_len(1000),function(i) pgamma(x,alpha*i,beta)*dnbinom(i,r,q)))
}
(Repartition(30))#ok
dnbinom(0,r,q)
sapply(c(0,50,100),Repartition)

sum(sapply(seq_len(1000),function(i) dnbinom(i,r,q)*alpha*i/beta * (1-pgamma(0,alpha*i + 1,beta))))/0.05
#b)
n <- 200
Repartition <- function(x)
{
  dnbinom(0,n*r,q) + sum(sapply(seq_len(1000),function(i) pgamma(x,alpha*i,beta)*dnbinom(i,n*r,q)))
}
Repartition(30*n)
sapply(c(0,50*n,100*n),Repartition)
z <- optimize(function(x) abs(Repartition(x)-0.99),c(0,100000))[[1]]

sum(sapply(seq_len(1000),function(i) dnbinom(i,n*r,q)*alpha*i/beta * (1-pgamma(z,alpha*i + 1,beta))))/(0.01*200)

### 2 ################
rm(list=ls())

set.seed(20170222)
m <- 100000
M <- numeric(m)
x <- numeric(m)
r <- 1.5
q <- 1/3
alpha <- 1.5
lambda <- 50
library(actuar)
for(i in 1:m)
{
  M[i] <- qnbinom(runif(1),r,q)
  if(M[i]>0)
  {
    x[i] <- sum(qpareto(runif(M[i]),alpha,lambda))
  }
}

x[m]

sum(x[x>1400]-1400)/m # ok 

sum(x[x>1200]-1200)/m

sum(exp(-0.001*x)/m)
  
  
  
  
  
  
  
  
  
  
  


### 3 ####################

rm(list=ls())
m <- 100000
set.seed(20160419)
z <- matrix(runif(3*m),ncol=3,byrow=T)
lambda <- c(1/50,20,100)
t <- c(1/2,2.5,2)
alpha <- c(0,0,2.5)

x1 <- ((-1*log(1-z[,1]))^(1/t[1]))/(lambda[1])
x1[m]
x2 <- ((z[,2])*lambda[2]^t[2]/(1-z[,2]))^(1/t[2])
x2[4]
x3 <- ((lambda[3]/((1-z[,3])^(1/alpha[3]))) - lambda[3])^(1/t[3])
x3[4]
y1 <- sort(x1)
y2 <- sort(x2)
y3 <- sort(x3)
mean(y1[y1>y1[0.9*m]])

sum(y3[(m*0.99 + 1):m])/(m*(0.01))
mean(y1)

s <- x1+x2+x3
s[3:4]
mean(s)
sum(s[(m*0.99 + 1):m])/(m*(0.01))

