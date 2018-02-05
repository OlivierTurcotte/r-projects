###### Exercice 2.1.2 Intro II 

###-------------------------------Numero 1 -------------------------------------------
rm(list=ls())
##a) sur papier
##b) sur papier
##c)
curve(dgamma(x,1,1/20),0,100,ylim=c(0,0.1),col="blue")
curve(dgamma(x,2,2/20),0,100,add=T)
curve(dgamma(x,5,5/20),0,100,add=T)
curve(dgamma(x,10,10/20),0,100,add=T)
curve(dgamma(x,20,20/20),0,100,add=T)

##d)
curve(pgamma(x,1,1/20),0,100,ylim=c(0,1),col="blue")
curve(pgamma(x,2,2/20),0,100,add=T)
curve(pgamma(x,5,5/20),0,100,add=T)
curve(pgamma(x,10,10/20),0,100,add=T)
curve(pgamma(x,20,20/20),0,100,add=T)
curve(pgamma(x,100,100/20),0,100,add=T)
## e) 
curve(qgamma(x,1,1/20),0,1,col="blue")
curve(qgamma(x,2,2/20),0,1,add=T)
curve(qgamma(x,5,5/20),0,1,add=T)
curve(qgamma(x,10,10/20),0,1,add=T)
curve(qgamma(x,20,20/20),0,1,add=T)
curve(qgamma(x,100,100/20),0,1,add=T)
## f)
TVaRsn <- function(x,n)
{
  (1/(1-x))* 20 * (1-pgamma(qgamma(x,n,n/20),n+1,n/20))
}

plot(function(x) TVaR(x,1),col="green")
plot(function(x) TVaR(x,2),add=T)
plot(function(x) TVaR(x,5),add=T)
plot(function(x) TVaR(x,10),add=T)
plot(function(x) TVaR(x,20),add=T)
plot(function(x) TVaR(x,100),add=T)

##g)
BME <- function(x,n)
{
  n*qexp(x,1/20) - qgamma(x,n,1/20)
}

plot(function(x) BME(x,2),ylim=c(0,5000),col="green",main="Bénéfice de mutualisation")
plot(function(x) BME(x,10),add=T)
plot(function(x) BME(x,100),add=T)

## h) 
TVaRexp <- function(x)
{
  qexp(x,1/20) + 20
}

BMK <- function(x,n)
{ 
  n*TVaRexp(x) + TVaRsn(x,n)
}

plot(function(x) BMK(x,2),ylim=c(0,5000),col="green",main="bénéfice de mutualisation")
plot(function(x) BMK(x,10),add=T)
plot(function(x) BMK(x,100),add=T)


###----------------------------------Numero 2---------------------------------------
rm(list=ls())
## a) 

# x1
b <- 1/2000000
VaRx1 <- qgamma(0.995,1,b)

#x2

VaRx2 <- qgamma(0.995,2,b)
VaRx1 + VaRx2

#totaux
VaRs <- qgamma(0.995,3,b)
## b)

#x1
TVaR <- function(x,shape,rate)
{
  (1/(0.005))* shape/rate*(1-pgamma(x,shape+1,rate))
}
TVaRx1 <- TVaR(VaRx1,1,b)
TVaRx2 <- TVaR(VaRx2,2,b)
TVaRs <- TVaR(VaRs,3,b)

###-----------------------------------Numero 3------------------------------------

## S suit une  1000*bin(n=400,p=0.008)

#a)
rm(list=ls())
kappa <- c(0.9,0.95,0.99,0.995)
VaRS <- 1000*qbinom(kappa,size=400,0.008)

VaRS - 1000*400*0.008

##b)

VaRS/1000
n <- 400
p <- 0.008
x1 <- 7:n - 6
x2 <- 7:n - 6
x3 <- 9:n - 8 
x4 <- 10:n -9
y1 <- dbinom(7:400,n,p)
y2 <- dbinom(7:400,n,p)
y3 <- dbinom(9:400,n,p)
y4 <- dbinom(10:400,n,p)
stop_loss1 <- sum(x1*y1)
stop_loss2 <- sum(x2*y2)
stop_loss3 <- sum(x3*y3)
stop_loss4 <- sum(x4*y4)

TVaR1 <- ((1/0.1)*stop_loss1 + 6)*1000
TVaR2 <- ((1/0.05)*stop_loss2 + 6)*1000
TVaR3 <- ((1/0.01)*stop_loss3 + 8)*1000
TVaR4 <- ((1/0.005)*stop_loss4  + 9)*1000

TVaR1 - 1000*400*p
TVaR2- 1000*400*p
TVaR3 - 1000*400*p
TVaR4 - 1000*400*p

## c)



BM <- 400*c(0,0,0,1000) - VaRS
BM
## avantageux seulement pour k = 0.995

## d)

TVaRX1 <-1000* (1/0.1)*0.008
TVaRX2 <- 1000*(1/0.05)*0.008
TVaRX3 <- 1000*(1/0.01)*0.008
TVaRX4 <- 1000*(1/0.005)*0.008 + 1000

400*c(TVaRX1,TVaRX2,TVaRX3,TVaRX4) - 1000*TVaRS

##--------------------------Numero 4---------------------------------------------
rm(list=ls())



















