######## numero 1

rm(list=ls())

k <- c(0.0001,0.001,0.01,0.1,0.5,0.9,0.99,0.999,0.9999)

## Les distributions

## 1 poisson lamda = 5
## 2 BN r=0.5, 

## Var pois

1000*qpois(k,lambda=5)

## Var BN 1

1000*qnbinom(k,0.5,10/11)

## Var BN 2
1000*qnbinom(k,5,0.5)

### Verification des grandeurs dechantillons avec 50
sum(dpois(0:50,5))
sum(dnbinom(0:50,0.5,10/11))
sum(dnbinom(0:50,5,0.5))
### cest ok
## TVar pois
## tableau

z <- data.frame(valeur=x1,densite=x2,repartition=x3)
z
pos <- qpois(k,5)
pos
x1 <- c(0:50)
x2 <- dpois(0:50,5)
x3 <- ppois(0:50,5)

Fun <- function(x)
{
  sum(x1[-c(0:(x+1))]*x2[-c(0:(x+1))])
}
Pois <- (1/(1-k))*(sapply(pos,function(x) Fun(x)) + pos*(ppois(pos,lambda=5)-k))*1000

## Nb1
pos <- qnbinom(k,0.5,10/11)
pos
x1 <- c(0:50)
x2 <- dnbinom(0:50,0.5,10/11)
x3 <- pnbinom(0:50,0.5,10/11)
x1
x2
x3

Fun <- function(x)
{
  sum(x1[-c(0:(x+1))]*x2[-c(0:(x+1))])
}
NB1 <- (1/(1-k))*(sapply(pos,function(x) Fun(x)) + pos*(pnbinom(pos,0.5,10/11)-k))*1000

## NB2
pos <- qnbinom(k,5,0.5)
pos
x1 <- c(0:50)
x2 <- dnbinom(0:50,5,0.5)
x3 <- pnbinom(0:50,5,0.5)
x1
x2
x3

Fun <- function(x)
{
  sum(x1[-c(0:(x+1))]*x2[-c(0:(x+1))])
}
NB2 <- (1/(1-k))*(sapply(pos,function(x) Fun(x)) + pos*(pnbinom(pos,5,0.5)-k))*1000
NB1
NB2

### LTVAR

## Pois


(5000-(1-k)*Pois)*(1/k)

## NB1
(5000-(1-k)*NB1)*(1/k)

## NB2
(5000-(1-k)*NB2)*(1/k)


######### Numeros 2  Gamma
rm(list=ls())

### a)

## Test de mon range
integrande <- function(x) dgamma(x,shape=0.25,rate=0.25)
integrande1 <- function(x) x*dgamma(x,shape=0.25,rate=0.25)
integrande2 <- function(x) (x^2)*dgamma(x,shape=0.25,rate=0.25)
integrate(integrande,0,Inf)

## E[x]
integrate(integrande1,0,Inf)

## E[x] =1 

## E^2[x] =5
integrate(integrande2,0,Inf)
## Var(x) = 5-1 =4

### b)

## VaR 0.9995

V <- qgamma(0.9995,0.25,0.25)

### c) 
stop_loss <- function(d)
{
integrande <- function(x) (x-d)*dgamma(x,shape=0.25,rate=0.25)
integrate(integrande,d,Inf)[[1]]
}
stop_loss(V)

### d) 

(1/(1-0.9995))*stop_loss(V) + V


######## numero 3

rm(list=ls())

##a)
## sigma= sqr(ln(5))  u= -ln(5)/2

sigma<- sqrt(log(5))
variance <- log(5)
u <-  -log(5)/2

## b)

V <- qlnorm(0.9995,u,sigma)

##c)

# test de range
integrande <- function(x) dlnorm(x,u,sigma)

integrate(integrande,0,Inf)

## stop-loss

stop_loss <- function(d)
  {
  integrande1 <- function(x) (x-d)*dlnorm(x,u,sigma)
  integrate(integrande1,d,Inf)[[1]]
}
    
 stop_loss(V)   
    

## d)

(1/(1-0.9995))*stop_loss(V)+V

########## Numero 4

rm(list=ls())
mu1<- 0.1
sigma1 <- 0.2
mu2 <- -0.3
sigma2 <- 0.1

esperance <- 0.8*mu1+0.2*mu2
esp2 <- 0.8*(mu1^2 + sigma1^2) +0.2*(mu2^2 + sigma2^2)
sd <- sqrt(esp2-esperance^2)


##b)
0.8*pnorm(0,mu1,sigma1) + 0.2*pnorm(0,mu2,sigma2)
z <-function(x) (0.8*pnorm(x,mu1,sigma1) + 0.2*pnorm(x,mu2,sigma2))

## c) 
kappa <- c(0.0001,0.001,0.01,0.1,0.5,0.9,0.99,0.999,0.9999)
VaR_optim <- sapply(kappa,function(t) optimize(function(x) abs(z(x)-t),c(-2,2),maximum=F)[[1]])

##d) 

Fun <-function(x) 0.8*dnorm(x,mu1,sigma1) + 0.2 * dnorm(x,mu2,sigma2)

esp_tronque <- sapply(VaR_optim,function(y) integrate(function(x) Fun(x)*x,y,Inf)[[1]])

(TVaR <- (1/(1-kappa))*esp_tronque)
lower_esp <- sapply(VaR_optim,function(y) integrate(function(x) Fun(x)*x,-Inf,y)[[1]])
(LTVaR <- (1/kappa)*lower_esp)












  










  
  
  
  
  
  
  
  
  

  
  



























