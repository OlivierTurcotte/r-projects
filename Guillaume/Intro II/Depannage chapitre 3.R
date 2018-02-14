####-----------------------Depannage chapitre 3--------------------------------------
###------------------------Numero 1--------------------------------------------------
rm(list=ls())
alpha <- 1/2
beta <- 1/200
#a sur papier
#b
repartition <- function(x)
{
  z <- numeric(50)
  for (i in 1:50)
  {
    z[i] <- pgamma(x,i*alpha,beta)*dbinom(i,50,0.04)
  }
  dbinom(0,50,0.04) + sum(z)
}
Fx <- c(0,100,200,300,400,500,600,700,800,900,1000)
prob <- 1-sapply(Fx,function(x) repartition(x))

#c
 stop_loss <- function(x)
 {
   z <- numeric(50)
   for(i in 1:50)
   {
     z[i] <- dbinom(i,50,0.04)*(alpha*i/beta * (1-pgamma(x,alpha*i + 1,beta)) - x*(1-pgamma(x,alpha*i,beta)))
   }
   sum(z)
 }
 stoploss <- sapply(Fx,function(x) stop_loss(x))
 #d
 (TVaR <- stoploss/prob + Fx)
#e)
 es_tronque <- function(x)
 {
   z <- numeric(50)
   for(i in 1:50)
   {
     z[i] <- dbinom(i,50,0.04)*alpha*i/beta * (1-pgamma(x,alpha*i + 1, beta))
   }
   sum(z)
 }
estronque<-  sapply(Fx, function(x) es_tronque(x))
estronque/prob # mm chose que par la stop_loss
#f)
kappa <- c(0.1,0.5,0.9,0.99,0.999,0.9999)
VaR <- sapply(kappa,function(x) optimize(function(y) abs(repartition(y) - x),c(0,10000))[[1]])

#g)
estronque_VaR<-  sapply(VaR, function(x) es_tronque(x))
TVaRk <- estronque_VaR/(1-kappa)

#h)
set.seed(2018)
m <- 1000000
M <- numeric(m)
X <- numeric(m)
for(i in 1:m)
{
  M[i] <- qbinom(runif(1),50,0.04)
  if(M[i]>0)
  {
    X[i] <- sum(qgamma(runif(1:M[i]),alpha,beta))
  }
}

#a)
mean(X)
var(X)
#b
Fxexp <- function(x) sum(X>x)/m
q <- sapply(Fx,function(x) Fxexp(x))
#c
stop_loss <- function(z)
{
  sum(z[z>0]/m)
}
z <-matrix(sapply(Fx, function(x) X-x),nrow=11,byrow=T) 
stoploss <- sapply(1:11,function(x)stop_loss(z[x,]))
#d

(TVaRexp <- stoploss/q  + Fx)

#e
es_tronq <- function(x)
{
  sum(X[X>x])/m
}
es_tronq(10)
sapply(Fx,function(x) es_tronq(x))/q

#f
r <- sort(X)
(VaR_exp <- r[kappa*m] )
#g
sapply(1:6,function(x) sum(X[X>VaR_exp[x]]/m * 1/(1-kappa[x])))
TVaRk

###------------------------Numero 2-----------------------------------------------
rm(list=ls())
lambda <- 2
alpha <- 1/2
beta <- 1/200
#a)
EM <- lambda
VM <- lambda
EB <- alpha/beta
VB <- alpha/beta^2
(EX <- EB*EM)
(varX <- EM*VB + EB^2 * VM)
#b)
repartition <- function(x)
{
  z <- numeric(1000)
  for(i in 1:1000)
  {
    z[i] <- dpois(i,lambda)*(pgamma(x,alpha*i,beta))
  }
  dpois(0,lambda) + sum(z)
}
Fx <- 100*c(0:10)
probtheo <- sapply(Fx,function(x) 1-repartition(x))
probtheo
#c)
stop_loss <- function(x)
{
  z <- numeric(1000)
  for(i in 1:1000)
  {
    z[i] <- dpois(i,lambda)*(alpha*i/beta * (1-pgamma(x,alpha*i + 1,beta)) - x*(1-pgamma(x,alpha*i,beta)))
  }
  sum(z)
}
stoploss <- sapply(Fx,function(x) stop_loss(x))
stoploss
# d)
(TVaRtheo <- stoploss/probtheo + Fx)
#e)
es_tronque <- function(x)
{
  z <- numeric(1000)
  for(i in 1:1000)
  {
    z[i] <- dpois(i,lambda)*alpha*i/beta * (1-pgamma(x,alpha*i + 1, beta))
  }
  sum(z)
}
estronque <-  sapply(Fx, function(x) es_tronque(x))
estronque/probtheo # mm chose que par la stop_loss
TVaRtheo
#f)
kappa <- c(0.1,0.5,0.9,0.99,0.999,0.9999)
VaR <- function(z)
{
  if(z<=dpois(0,lambda))
    0
else
  optimize(function(y) abs(repartition(y) - z),c(0,10000))[[1]]
}
(VaRk <- sapply(kappa,function(z) VaR(z)))
#g)
estronque_VaR<- sapply(VaRk, function(x) es_tronque(x))
(TVaRk <- estronque_VaR/(1-kappa))
#h)
set.seed(2018)
m <- 1000000
M <- numeric(m)
X <- numeric(m)
for(i in 1:m)
{
  M[i] <- qpois(runif(1),lambda)
  if(M[i]>0)
  {
    X[i] <- sum(qgamma(runif(1:M[i]),alpha,beta))
  }
}
#a)
mean(X)
var(X)
#b
Fxexp <- function(x) sum(X>x)/m
 q <- sapply(Fx,function(x) Fxexp(x))
#c
stop_loss <- function(z)
{
  sum(z[z>0]/m)
}
z <-matrix(sapply(Fx, function(x) X-x),nrow=11,byrow=T) 
stoploss <- sapply(1:11,function(x)stop_loss(z[x,]))
#d

(TVaRexp <- stoploss/q  + Fx)

#e
es_tronq <- function(x)
{
  sum(X[X>x])/m
}
es_tronq(10)
sapply(Fx,function(x) es_tronq(x))/q

#f
r <- sort(X)
(VaR_exp <- r[kappa*m] )
#g
sapply(1:6,function(x) sum(X[X>VaR_exp[x]]/m * 1/(1-kappa[x])))
TVaRk









 
 
 
 
 
 
 
 
 
 
 
 
###------------------------Numero 3-----------------------------------------------
rm(list=ls())
r <- 1/2
q <- 1/5
alpha <- 1/2
beta <- 1/200
#a)
EM <- r*(1-q)/q
VM <-r*(1-q)/q^2
EB <- alpha/beta
VB <- alpha/beta^2
(EX <- EB*EM)
(varX <- EM*VB + EB^2 * VM)
#b)
repartition <- function(x)
{
  z <- numeric(1000)
  for(i in 1:1000)
  {
    z[i] <- dnbinom(i,r,q)*(pgamma(x,alpha*i,beta))
  }
  dnbinom(0,r,q) + sum(z)
}
Fx <- 100*c(0:10)
probtheo <- sapply(Fx,function(x) 1-repartition(x))
probtheo
#c)
stop_loss <- function(x)
{
  z <- numeric(1000)
  for(i in 1:1000)
  {
    z[i] <- dnbinom(i,r,q)*(alpha*i/beta * (1-pgamma(x,alpha*i + 1,beta)) - x*(1-pgamma(x,alpha*i,beta)))
  }
  sum(z)
}
stoploss <- sapply(Fx,function(x) stop_loss(x))
stoploss
# d)
(TVaRtheo <- stoploss/probtheo + Fx)
#e)
es_tronque <- function(x)
{
  z <- numeric(1000)
  for(i in 1:1000)
  {
    z[i] <- dnbinom(i,r,q)*alpha*i/beta * (1-pgamma(x,alpha*i + 1, beta))
  }
  sum(z)
}
estronque <-  sapply(Fx, function(x) es_tronque(x))
estronque/probtheo # mm chose que par la stop_loss
TVaRtheo
#f)
kappa <- c(0.1,0.5,0.9,0.99,0.999,0.9999)
VaR <- function(z)
{
  if(z<=dnbinom(0,r,q))
    0
  else
    optimize(function(y) abs(repartition(y) - z),c(0,10000))[[1]]
}
(VaRk <- sapply(kappa,function(z) VaR(z)))
#g)
estronque_VaR<- sapply(VaRk, function(x) es_tronque(x))
(TVaRk <- estronque_VaR/(1-kappa))
#h)
set.seed(2018)
m <- 1000000
M <- numeric(m)
X <- numeric(m)
for(i in 1:m)
{
  M[i] <- qnbinom(runif(1),r,q)
  if(M[i]>0)
  {
    X[i] <- sum(qgamma(runif(1:M[i]),alpha,beta))
  }
}
#a)
mean(X)
var(X)
#b
Fxexp <- function(x) sum(X>x)/m
(q <- sapply(Fx,function(x) Fxexp(x)))
probtheo
#c
stop_loss <- function(z)
{
  sum(z[z>0]/m)
}
z <-matrix(sapply(Fx, function(x) X-x),nrow=11,byrow=T) 
(stoploss2 <- sapply(1:11,function(x)stop_loss(z[x,])))
stoploss
#d

(TVaRexp <- stoploss/q  + Fx)
TVaRtheo

#e
es_tronq <- function(x)
{
  sum(X[X>x])/m
}
es_tronq(10)
sapply(Fx,function(x) es_tronq(x))/q
TVaRtheo

#f
r <- sort(X)
(VaR_exp <- r[kappa*m] )
VaRk
#g
sapply(1:6,function(x) sum(X[X>VaR_exp[x]]/m * 1/(1-kappa[x])))
TVaRk
###------------------------Numero 4------------------------------------------------
rm(list=ls())
##a)
r <- 1/2
q <- 1/5
mu <- log(100) - 1/2
sigma <- 1
EM <- r*(1-q)/q
VM <-r*(1-q)/q^2
EB <- exp(mu + sigma^2/2)
VB <- exp(2*mu + sigma^2)*(exp(sigma^2)-1)
(EX <- EB*EM)
(varX <- EM*VB + EB^2 * VM)
##b)
set.seed(2018)
m <- 1000000
M <- numeric(m)
X <- numeric(m)
for(i in 1:m)
{
  M[i] <- qnbinom(runif(1),r,q)
  if(M[i]>0)
    X[i] <- sum(qlnorm(runif(M[i]),mu,sigma))
}  

#i 
mean(X)
var(X)
#ii
Fx <- 100*c(0:10)
Fxexp <- function(x) sum(X>x)/m
(q <- sapply(Fx,function(x) Fxexp(x)))
#iii
stop_loss <- function(z)
{
  sum(z[z>0]/m)
}
z <-matrix(sapply(Fx, function(x) X-x),nrow=11,byrow=T) 
(stoploss2 <- sapply(1:11,function(x)stop_loss(z[x,])))

##iv
kappa <- c(0.1,0.5,0.9,0.99,0.999,0.9999)
r <- sort(X)
(VaR_exp <- r[kappa*m] )
##v
sapply(1:6,function(x) sum(X[X>VaR_exp[x]]/m * 1/(1-kappa[x])))



    
    
    
    
    
    
    
    
    


###------------------------Numero 5-------------------------------------------------
rm(list=ls())
##a)
lambda <- 2
alpha <- 1.5
beta <- 50
EM <- lambda
VM <- lambda
EB <- beta/(alpha-1)
VB <- Inf
(EX <- EM*EB)
varX <- Inf
##b)
library(actuar)
set.seed(2018)
m <- 1000000
M <- numeric(m)
X <- numeric(m)
for(i in 1:m)
{
  M[i] <- qpois(runif(1),lambda)
  if(M[i]>0)
    X[i] <- sum(qpareto(runif(M[i]),alpha,beta))
}  

#i 
mean(X)
var(X) ### devrait etre linfinie
#ii
Fx <- 100*c(0:10)
Fxexp <- function(x) sum(X>x)/m
(q <- sapply(Fx,function(x) Fxexp(x)))
#iii
stop_loss <- function(z)
{
  sum(z[z>0]/m)
}
z <-matrix(sapply(Fx, function(x) X-x),nrow=11,byrow=T) 
(stoploss2 <- sapply(1:11,function(x)stop_loss(z[x,])))

##iv
kappa <- c(0.1,0.5,0.9,0.99,0.999,0.9999)
r <- sort(X)
(VaR_exp <- r[kappa*m] )
##v
sapply(1:6,function(x) sum(X[X>VaR_exp[x]]/m * 1/(1-kappa[x])))

