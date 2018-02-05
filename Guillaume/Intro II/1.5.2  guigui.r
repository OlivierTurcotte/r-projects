### EXERCICE DEPANNAGE INTRO II

###---------------------------NUMERO 1-----------------------------------------
##a)
alpha <- 1.5
lambda <- 1.5/100
#E[x]
esperance <- alpha/lambda


#Var(x)
variance <- alpha/lambda^2
#sqrt(var(x))
sd <- sqrt(variance) #=81.64966

##b)

#VaR.99(x)

VaR <- qgamma(0.99,shape=alpha,rate=lambda)

##c
stop_loss <- (alpha/lambda)*(1-pgamma(VaR,alpha+1,lambda))-VaR*(1-pgamma(VaR,alpha,lambda))


##d)

(TVaR <- (1/(1-0.99))*stop_loss + VaR)

##e)

#stop_loss
stoploss <- expression((alpha/lambda)*(1-pgamma(x,alpha+1,lambda))-x*(1-pgamma(x,alpha,lambda)))
curve(eval(stoploss,envir = list(x=x)),from=0,to=1000)
abline(v=VaR,col="purple")
abline(v=100,col="green")
#density
f <- curve(dgamma(x,shape=alpha,rate=lambda),add=F,col="blue",xlim=c(0,1000))
abline(v=VaR,col="purple")
abline(v=100,col="green")
#repartition
repar <- curve(pgamma(x,shape=alpha,rate=lambda),add=F,from=0,to=1000,col="yellow")
abline(v=VaR,col="purple")
abline(v=100,col="green")


### -------------------------NUMERO 2-----------------------------------------------

## a)
rm(list=ls())
mu <- log(100) - 1/2
sd <- 1
#E[x]
esperance <- exp(mu+sd^2/2) #100

#Var(x)
variance <- exp(2*mu+sd^2)*(exp(sd^2)-1)
#sqrt(var(x))
sigma <- sqrt(variance) #131

## b)

VaR <- qlnorm(0.99,meanlog=mu,sdlog=sd) #621.1161

## c)
(stop_loss <- esperance*(1-pnorm((log(VaR)-mu)/sd,sd,1))-VaR*(1-pnorm((log(VaR)-mu)/sd,0,1)))

## d)
(TVaR <- (1/(1-0.99))*stop_loss + VaR) ## 923.6225

##e 
#Stop_loss
stoploss <- expression(esperance*(1-pnorm((log(VaR)-mu)/sd,sd,1))-VaR*(1-pnorm((log(VaR)-mu)/sd,0,1)))
curve(eval(stoploss,envir=list(x=x)),xlim=c(0,1000))
abline(v=VaR,col="purple")
abline(v=esperance,col="green")
# Density

f <- curve(dlnorm(x,meanlog=log(100)-1/2,sdlog=1),add=F,col="blue",xlim=c(0,1000))
abline(v=VaR,col="purple")
abline(v=esperance,col="green")
#Repartion

repar <- curve(plnorm(x,meanlog=log(100)-1/2,sdlog=1),add=F,from=0,to=1000,col="yellow")
abline(v=VaR,col="purple")
abline(v=esperance,col="green")

###---------------------------------NUMERO 3---------------------------------------

rm(list=ls())
repartition <- expression(0.8*(1-exp(-x/75)) + 0.2*(1-exp(-x/200)))


## a)

# density
(fx <- D(repartition,"x"))

## d)

esperance <-  0.8*(75)+ 0.2*(200)
esperance2 <- 0.8*(2*(75)^2) + 0.2*(2*(200)^2)
sigma <- sqrt(esperance2 - esperance^2)

Fun_exp<- function(y) 0.8*pexp(y,1/75) + 0.2*pexp(y,1/200)
kappa <- c(0.5,0.99,0.9999)

VaR <- sapply(kappa,function(t) optimize(function(y) abs(Fun_exp(y)-t),c(0,10000),maximum=F)[[1]])
esperance_tronque <- function(x,b) x*exp(-1*x*b) + exp(-1*x*b)/b
esp_tronq_fun<- sapply(VaR, function(y) (0.8*esperance_tronque(y,1/75) + 0.2*esperance_tronque(y,1/200)))
(TVaR <- esp_tronq_fun*(1/(1-kappa)))
(LTVaR <- (1/kappa)*esperance - ((1-kappa)/kappa)*TVaR)

### e)

curve(0.8*dexp(x,1/75)+0.2*dexp(x,1/200),0,1000)
abline(v=esperance,col="blue")
abline(v=VaR[2],col="green")

curve(0.8*pexp(x,1/75)+0.2*pexp(x,1/200),0,1000)
abline(v=esperance,col="blue")
abline(v=VaR[2],col="green")


esp_tronq <- function(y) 0.8*esperance_tronque(y,1/75) + 0.2*esperance_tronque(y,1/200)
stop_loss <- function(x)
{
  esp_tronq(x)*(0.8*(1-pexp(x,1/75)) + 0.2*(1-pexp(x,1/200)))
}
plot(function(x) stop_loss(x),xlim=c(0,1000))
abline(v=esperance,col="blue")
abline(v=VaR[2],col="green")
   
  


###------------------------numero 4----------------------------------------------
rm(list=ls())
r <- 2
q <- 1/2
##a)

#E[x]
esperance <- r*(1-q)/q

#Var(x)
variance <- r*(1-q)/q^2
sigma <- sqrt(variance)

##b

VaR <- qnbinom(0.99,r,q)

##c)

x <- dnbinom(10:10000,r,q)
y <- c(10:10000) - 9
sum(x*y)


stop_loss <-  esperance*(1-pnbinom(VaR-1,r+1,q))-VaR*(1-pnbinom(VaR,r,q))

##d)

TVaR <- stop_loss*(1/(1-0.99))+VaR 
##e)
y <- sapply(1:1000,function(x) dnbinom(x,r,q))
length(y)
length(x)
x <- c(1:1000)
plot(x,y,type="h",xlim=c(0,10))
##f)

distribution <- expression(pnbinom(x,r,q))
plot(eval(distribution,envir=list(x=x)),type="l",xlim=c(0,10))
abline(v=esperance,col="blue")
abline(v=VaR,col="green")


stoploss <- expression(esperance*(1-pnbinom(x-1,r+1,q))-x*(1-pnbinom(x,r,q)))
plot(eval(stoploss,envir=list(x=x)),type="l",xlim=c(0,100))
abline(v=esperance,col="blue")
abline(v=VaR,col="green")


## g)
stop_loss <- function(x) esperance*(1-pnbinom(qnbinom(x,r,q)-1,r+1,q)) - qnbinom(x,r,q)*(1-pnbinom(qnbinom(x,r,q),r,q))
stop_loss(0.99)
TVaR <- function(x)
{
  stop_loss(x)*(1/(1-x))+qnbinom(x,r,q)
}
  TVaR(0.01)
plot(TVaR)
curve(qnbinom(x,r,q),add=T)

###---------------------------------Numero 5--------------------------------------

rm(list=ls()) 

u <- 1/1000

sigma <- optimize(function(x) abs(x*qnorm(0.99) - x^2/2 -log(1/1000 * qexp(0.99,1/1000))),c(0,50),maximum=F)[[1]]
mu <- log(1000) - (sigma^2)/2

##c)
curve(dexp(x,u),0,5000)
curve(dlnorm(x,mu,sigma),add=T)
abline(v=1000,col="green")
abline(v=qexp(0.99,1/1000),col="blue")
##d)
curve(pexp(x,u),0,5000)
curve(plnorm(x,mu,sigma),add=T)
abline(v=1000,col="green")
abline(v=qexp(0.99,1/1000),col="blue")
##e)

















































