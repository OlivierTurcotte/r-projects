############ Exercice 4.3 Integration Monte-Carlo ###########################
############ Numero 1 #########
rm(list=ls())
alpha1 <- 2
beta1 <- 1/1000
alpha2 <- 2
lambda2 <- 200000
t2 <- 1.5
t3 <- 4
lambda3 <- 2000
mu4 <- 9.5
sigma4 <- 1
q4 <- 0.1
n5 <- 1000
q5 <- 0.97

## a)

(EX1 <- alpha1/beta1)
(pi1 <- 1.25*EX1)

(EX2 <- lambda2^(1/t2) * gamma(1+1/t2) * gamma(alpha2 - 1/t2) * 1/gamma(alpha2))
(pi2 <- 1.10*EX2)

(EX3 <- lambda3*gamma(1+1/t3)*gamma(1-1/t3))
(pi3 <- 1.2*EX3)

(EX4 <- q4*exp(mu4 + (sigma4^2)/2))
(pi4 <- 1.3*EX4)

(EX5 <- 2*n5*q5)
(pi5 <- 0.995*EX5)

EXi <- c(EX1,EX2,EX3,EX4,EX5)
pii <- c(pi1,pi2,pi3,pi4,pi5)

## b)  E[L] <-  E[sum(PIi- PIi)] ... <-  sum(E[Xi]) - Sum(E[PIi]) i=1..4  sinon Pi5-X5 pour L5

sum(sapply(1:4,function(i) EXi[i]-pii[i])) + (pii[5]- EXi[5]) ## négatif ? 

#c)

n <- 100000
set.seed(20130402)
m <- matrix(runif(5*n),ncol=5,byrow=T)
m[1,]
X1 <- qgamma(m[,1],alpha1,beta1)
X2 <- (lambda2/((1-m[,2])^(1/alpha2)) - lambda2)^(1/t2)
X3 <- (lambda3^(t3) * m[,3] / (1-m[,3]))^(1/t3)
z <- numeric(n)
X4 <- numeric(n)
sum(m[,4]>0.9) ### comment on on utilise seulement 1 X par U ...
X4[1:10035] <- qlnorm(m[1:10035,4],mu4,sigma4) ## Pas tellement legit 
X5 <- 2* qbinom(m[,5],n5,q5)

data.frame("X"=c(X1[1],X2[1],X3[1],X4[1],X5[1]), "L"= c(X1[1],X2[1],X3[1],X4[1],pi5) - c(pi1,pi2,pi3,pi4,X5[1]))

## d)
X11 <- X1 - pi1
X22 <-  X2-pi2
X33 <- X3-pi3
X44 <-  X4-pi4
X55 <- pi5 - X5
 Ltot <- X11 + X22 + X33 + X44 + X55
 mean(Ltot)
 
## e)
 sum(Ltot>0)/n

 ## f) 
 
Ltot_sorted <- sort(Ltot)
Ltot_sorted[0.99*n]

## g) 
sum(Ltot_sorted[(0.99*n):n])/(0.01*n)







############ Numero 2 ###################
rm(list=ls())
r <- 1.2
q <- 1/4
mu <- 2
sigma <- 0.9
lambda <- 3
t <- 0.5
beta <- 1/4
set.seed(20130402)
n <- 100000
m <- numeric(n)
N <- numeric(n)
X <- numeric(n)
Y <-numeric(n)
library(actuar)
for(i in 1:n)
{
m[i] <- qnbinom(runif(1),r,q)
if(m[i]>0)
  X[i] <- sum(qlnorm(runif(m[i]),mu,sigma))
N[i] <-  qpois(runif(1),lambda)
if(N[i]>0)
  Y[i] <- sum(qweibull(runif(N[i]),t,1/beta))
}
## a) 

data.frame("X"=X[1:3],"Y"=Y[1:3])

## b)

S <- X+Y

S[1:3]

## c)
#Théorique : 
(EX <- r*(1-q)/q * exp(mu+sigma^2/2))
(EY <- lambda*1/beta * gamma(1+1/t))
(ES <- EX+EY)

varX <- r*(1-q)/q * exp(2*mu + sigma^2) * (exp(sigma^2)-1) + exp(mu+sigma^2/2)^2 * r*(1-q)/q^2
varY <- lambda*(1/beta^2 * gamma(1+2/t) - (1/beta * gamma(1 + 1/t))^2) + (1/beta * gamma(1+1/t))^2*lambda 
(varS <- varX + varY)


## d)

# Expérimentale:
mean(S) ### close enough
var(S) ### close enough

## e) 

valeur <- c(100,200,300,400,500)

sapply(valeur, function(x) sum(S>x)/n)

## f)

S_sorted <- sort(S)
kappa <- c(0.5,0.75,0.99,0.999,0.9999)

(VaR <- S_sorted[kappa*n])

## g)
(TVaR <- sapply(kappa,function(x) sum(S_sorted[(x*n):n])/((1-x)*n)))

data.frame("Kappa"=kappa,"VaR"=VaR,"TVaR"=TVaR)














  

############ Numero 3 Même chose que numéro 2 mais avec différentes lois...
