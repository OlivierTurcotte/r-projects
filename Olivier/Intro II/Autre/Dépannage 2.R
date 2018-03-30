# Numéro 1
{
rm(list=ls())
alpha <- 1.5
lambda <- 1.5/100

density <- function(x) dgamma(x,shape = alpha, rate = lambda)
distribution <- function(x) pgamma(x,shape = alpha, rate = lambda)
quantile <- function(x) qgamma(x,shape = alpha,rate = lambda)
stopLoss <- function(d) {
    integrate(function(x) (x-d)*density(x),lower = d,upper = Inf)[[1]]
}
#A)
esperance <- alpha/lambda #integrate(function(x) x*density(x), lower = 0, upper = Inf)[[1]]
variance <- alpha/lambda^2 #integrate(function(x) (x-esperance)^2*density(x),lower = -Inf,upper = Inf)[[1]]
standardDev <- sqrt(variance)
#B)
VaR0.99 <- quantile(0.99)
#C)
SL <- stopLoss(VaR0.99)
#D)
TVaR0.99 <- 1/(1-0.99)*SL+VaR0.99
#E)
par(mfrow=c(1,3))
plot(density,0,800,ylab="f(x)")
abline(v=VaR0.99,col="purple")
abline(v=esperance,col="green")
plot(distribution,0,800,ylab="F(x)")
abline(v=VaR0.99,col="purple")
abline(v=esperance,col="green")
plot(Vectorize(stopLoss),0,1000,ylab="Stop-Loss(x)")
abline(v=VaR0.99,col="purple")
abline(v=esperance,col="green")
}
# Numéro 2
{
rm(list=ls())
mu <- log(100)-0.5
sigma <- 1

density <- function(x) dlnorm(x,mu,sigma)
distribution <- function(x) plnorm(x,mu,sigma)
quantile <- function(x) qlnorm(x,mu,sigma)
stopLoss <- function(d) {
    exp(mu+sigma^2/2)*(1-pnorm(log(d),mean = mu+sigma^2,sd = sigma))-d*(1-pnorm(log(d),mean = mu,sd=sigma))
}
#A)
esperance <- exp(mu+sigma^2/2)
variance <- (exp(sigma^2)-1)*exp(2*mu+sigma^2)
standardDev <- sqrt(variance)
#B)
VaR0.99 <- quantile(0.99)
#C)
SL <- stopLoss(VaR0.99)
#D)
TVaR0.99 <- 1/(1-0.99)*SL+VaR0.99
#E)
par(mfrow=c(1,3))
plot(density,0,1000,ylab="f(x)")
abline(v=VaR0.99,col="purple")
abline(v=esperance,col="green")
plot(distribution,0,1000,ylab="F(x)")
abline(v=VaR0.99,col="purple")
abline(v=esperance,col="green")
plot(Vectorize(stopLoss),0,1000,ylab="Stop-Loss(x)")
abline(v=VaR0.99,col="purple")
abline(v=esperance,col="green")
}
# Numéro 3
{
rm(list=ls())

Fx <- expression(0.8*(1-exp(-x/75))+0.2*(1-exp(-x/200)))
fx <- D(Fx,"x")
lambda <- c(1/75,1/200)
prob <- c(0.8,0.2)

# d)  : E[X] & sqrt(Var(x))
distribution <- function(x) eval(Fx)
density <- function(x) eval(fx)
stopLoss <- function(d) sum(prob/lambda*exp(-d*lambda))
esperance <- sum(prob/lambda)
var <- 2*(sum(prob/lambda^2))-esperance^2

(list("esperance" =esperance,"sigma" = sqrt(var)))

# e) Var (opti), TVaR et LTVaR à k = 0.5,0.99,0.9999
kappa <- c(0.5,0.99,0.9999)
VaR <- sapply(seq(kappa),function(i) uniroot(function(x) distribution(x)-kappa[i],c(0,10000))[[1]])
TVaR <- sapply(seq(kappa),function(i) 1/(1-kappa[i])*stopLoss(VaR[i])+VaR[i])
LTVaR <- sapply(seq(kappa),function(i) 1/kappa[i]*quad(function(x) x*density(x),0,VaR[i]))
# Et voilà !
data.frame(kappa,VaR,TVaR,LTVaR)

# f)
par(mfrow=c(1,3))
plot(density,0,1000,ylab="f(x)")
abline(v=VaR[2],col="purple")
abline(v=esperance,col="green")
plot(distribution,0,1000,ylab="F(x)")
abline(v=VaR[2],col="purple")
abline(v=esperance,col="green")
plot(Vectorize(stopLoss),0,1000,ylab="Stop-Loss(x)")
abline(v=VaR[2],col="purple")
abline(v=esperance,col="green")
}
# Numéro 4
{
rm(list=ls())
param <- c(2,1/2)
density <- function(x) dnbinom(x,param[1],param[2])
distribution <- function(x) pnbinom(x,param[1],param[2])
quantile <- function(x) qnbinom(x,size = param[1],prob = param[2])
stopLoss <- function(d) esperance*(1-pnbinom(d-1,param[1]+1,param[2]))-d*(1-distribution(d))
TVaR_f <- function(k) 1/(1-k)*stopLoss(quantile(k))+quantile(k)
TVaR_f(0.99)
# a)
esperance <- param[1]*param[2]/(1-param[2])
var <- param[1]*param[2]/(1-param[2])^2
# b)
VaR <- quantile(0.99)[[1]]
# c)
SL <- stopLoss(VaR)
# d)
TVaR <- 1/(1-0.99)*SL+VaR
# e)
x <- 2:20
values <- sapply(x,function(i) density(i))
plot(x,values,type= "h")
# f)
par(mfrow=c(1,2))
plot(distribution,0,20,ylab = "Fx(x)")
abline(v=VaR,col="purple")
abline(v=esperance,col="green")
plot(stopLoss,0,20,ylab = "Stop-Loss(x)")
abline(v=VaR,col="purple")
abline(v=esperance,col="green")

# g)
par(mfrow=c(1,2))
plot(quantile,0,1,ylab = "VaRk(x)")
abline(v=VaR,col="purple")
abline(v=esperance,col="green")
plot(TVaR_f,0,1,ylab = "TVaRk(x)")
abline(v=VaR,col="purple")
abline(v=esperance,col="green")
}
# Numéro 5
{
VaRExp <- function(k)-log(1-k)/(1/1000)

# b)   
lambda <- 1/1000
sigma <- optimize(function(x) abs(x*qnorm(0.99) - x^2/2 -log(VaRExp(0.99)/1000)),c(0,50),maximum=F)[[1]]

# c)
densityExp <-function(x) dexp(x,lambda)
densityLN <- function(x) dlnorm(x,log(1000)-sigma^2/2,sigma)
par(mfrow=c(1,1))
plot(densityExp,0,5000,col = "red")
plot(densityLN,0,5000,add = T,col = "blue")
# d)
distributionExp <- function(x) pexp(x,lambda)
distributionLN <- function(x) plnorm(x,log(1000)-sigma^2/2,sigma)
plot(distributionExp,0,5000,col = "red")
plot(distributionLN,0,5000,add = T,col = "blue")
# e)
VaRLN <- function(k) exp(qnorm(k)*sigma+log(1000)-sigma^2/2)
plot(VaRExp,0,1,col = "red")
plot(VaRLN,0,1,add = T,col = "blue")
# Pour ce qui est des autres, fuck that, ça revient au même.
} 