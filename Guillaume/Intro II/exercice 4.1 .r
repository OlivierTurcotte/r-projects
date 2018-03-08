########## Chapitre 4.1  Exercice Informatique #####################

###### Numero 1 ##########
rm(list=ls())
r <- 0.5
q <- 5/6
alpha <- 0.5
beta <- 1/10000

#a) r* = n*r q*=q
#b)

(EX <- r*(1-q)/(q)*alpha/beta)
(ES <- n*r*(1-q)/(q)*alpha/beta )

#c)

repartitionx <- function(x)
{
  dnbinom(0,r,q) + sum(sapply(1:10000,function(i) dnbinom(i,r,q)*pgamma(x,alpha*i,beta)))
}
repartitionx(10000)

n <- 200

repartitions <- function(x)
{
  dnbinom(0,n*r,q) + sum(sapply(1:10000,function(i) dnbinom(i,n*r,q)*pgamma(x,alpha*i,beta)))
}
repartitions(n*10000)

#d)

#VaR0.5 = 0 
VaRX0.5 <- 0
(VaRX0.99 <- uniroot(function(x) repartitionx(x)-0.99,c(0,10000000))[[1]])

#e)

(VaRS0.5 <- uniroot(function(x) repartitions(x)-0.5,c(0,100000000000))[[1]])
(VaRS0.99 <- uniroot(function(x) repartitions(x)-0.99,c(0,100000000000))[[1]])

#f)

TVaRX <- function(x,k)
{

  sum(sapply(1:10000,function(i) dnbinom(i,r,q)*alpha*i/beta * (1-pgamma(x,alpha*i+1,beta))))/(1-k)
}

TVaRX(VaRX0.5,0.5)
TVaRX(VaRX0.99,0.99)

#g)

TVaRS <- function(x,k)
{
  
  sum(sapply(1:10000,function(i) dnbinom(i,n*r,q)*alpha*i/beta * (1-pgamma(x,alpha*i+1,beta))))/(1-k)
}

TVaRS(VaRS0.5,0.5)
TVaRS(VaRS0.99,0.99)

#h)

200*VaRX0.5 <= VaRS0.5

#i)

200*VaRX0.99 >= VaRS0.99

#j)

 200 * TVaRX(VaRX0.5,0.5) >= TVaRS(VaRS0.5,0.5)
 
#k)
  200 * TVaRX(VaRX0.99,0.99) >= TVaRS(VaRS0.99,0.99)
  
# l)  : VaR pas toujours sous-additive , TVaR toujours sous-additive

  
  
  
  


  
###### Numero 2 #########
rm(list=ls())
n <- 1000
lambda <- 0.005
beta <- 1/1000

#a)
#-
repartionx <- function(x)
{
  dpois(0,lambda) + sum(sapply(1:10000,function(i) dpois(i,lambda)*pgamma(x,i,beta)))
}
#-
repartionx(5) ## ok 
repartionx(0)
repartionx(10)
#-
## Optimize, vérifier si k > fm(0)

#- 
0.99<dpois(0,lambda) 
VaRX0.99 <- 0
#- esperance de x / 0.01

#-
TVaRX0.99 <- lambda*1/beta / 0.01

##b)
#- Wn suit une poiscomp(lambda*n, FB) ou FB suit une expo(lambda*n)
#- 
  
repartionW <- function(x)
{
  dpois(0,n*lambda) + sum(sapply(1:10000,function(i) dpois(i,n*lambda)*pgamma(x,i,n*beta)))
}

#-
repartionW(5) ## ok 
sapply(c(0,10),function(x) repartionW(x))

#- Optimize , mais vérifier si k> fm(0)
#-
0.99>repartionW(0) ## ok

(VaR0.99 <- uniroot(function(x) repartionW(x)-0.99,c(0,10000000))[[1]])

#- somme(esperance(x * 1{x>d} | M=M) * fm(m))

#-
(TVaRS0.99 <- sum(sapply(1:100000,function(i) i/(n*beta)*dpois(i,n*lambda)*(1-pgamma(VaR0.99,i+1,n*beta))))/0.01)

## c)

VaRX0.99-VaR0.99 ## négatif ce qui permet d'observer que la VaR n'est pas sous additive : pas positiver de mutualiser selon la VaR
TVaRX0.99-TVaRS0.99 ## Bénéfice positif à cause de la sous-additivité





  

###### Numero 3 ##########
rm(list=ls())
alpha <- 2
beta <- 1/5000
r <- 2
q <- 0.997

#a)
(EX <- r*(1-q)/q * alpha/beta)
repartitionx <- function(x)
{
  dnbinom(0,r,q) + sum(sapply(1:10000,function(i) dnbinom(i,r,q)*pgamma(x,alpha*i,beta)))
}


0.99>repartitionx(0)
VaRX0.99 <- 0
(TVaRX0.99 <- EX/0.01)
#b)
n <- c(10,100,1000)
(ES <- EX*n)
# S suit une Bin N comp(n*r,q,FB)

repartitions1 <- function(x)
{
  dnbinom(0,n[1]*r,q) + sum(sapply(1:10000,function(i) dnbinom(i,n[1]*r,q)*pgamma(x,alpha*i,beta)))
}
repartitions2 <- function(x)
{
  dnbinom(0,n[2]*r,q) + sum(sapply(1:10000,function(i) dnbinom(i,n[2]*r,q)*pgamma(x,alpha*i,beta)))
}
repartitions3 <- function(x)
{
  dnbinom(0,n[3]*r,q) + sum(sapply(1:10000,function(i) dnbinom(i,n[3]*r,q)*pgamma(x,alpha*i,beta)))
}
(VaRS0.99 <- c(uniroot(function(x) repartitions1(x)-0.99,c(0,10000000))[[1]],uniroot(function(x) repartitions2(x)-0.99,c(0,10000000))[[1]],uniroot(function(x) repartitions3(x)-0.99,c(0,10000000))[[1]]))

TVaR <- function(x,n)
{
  sum(sapply(1:1000,function(i) alpha*i/beta*(1-pgamma(x,alpha*i+1,beta))*dnbinom(i,n*r,q)))/0.01
}

(TVaRS0.99 <- sapply(1:3,function(i) TVaR(VaRS0.99[i],n[i])))

#c)

n*VaRX0.99-VaRS0.99 ## On voit qu'il n'est pas positive de mutualiser le risque selon la VaR puisqu'elle n'est pas sous-additive

#d)
n*TVaRX0.99-TVaRS0.99 ##On voit qu'il est  positif de mutualiser le risque selon la TVaR puisqu'elle est sous-additive. Plus n augmente , plus BM grand

##e)

#i) Par l'homogénéité : 

(PrimeA <- 1/n * VaRS0.99)
# ii) Par l'homogénéité :
(PrimeB <- 1/n * TVaRS0.99) ## Prime plus élevé selon la TVaR...

###### Numero 4 ###########
rm(list=ls())
p <- 0.2
beta <- 1/2500
#a)
(EX <- 0.2*1/beta)
0.2*2500^2 +2500^2 * 0.2 * 0.8

#b)

repartitionx <- function(x)
{
  
  0.8 + 0.2*pexp(x,beta)
}
repartitionx(0)
VaRX0.5 <- 0
EX/0.5

## c)

VaRX0.99 <- uniroot(function(x) repartitionx(x)-0.99,c(0,10000))[[1]]

0.2* (exp(-beta*VaRX0.99)/beta + VaRX0.99*exp(-beta*VaRX0.99))/0.01 ## ok

## d) Développer la forme de la Fx : on voit que ca forme des gammas ...
prob <- c(0.384,0.096,0.008)
sum(sapply(1:3,function(i) i/beta * (1-pgamma(11658.566,i+1,beta))*prob[i]))/0.01 ## ok !!







###### Numero 5 ############
rm(list=ls())
#a) S suit un Bincomp(6,1/2,FB)
#b)
n <- 6
p <- 1/2
alpha <- 1.2
lambda <- 1/10
(ES <- n*p*alpha/lambda)
(VarS <- n*p*alpha/lambda^2 + (alpha/lambda)^2 * n*p*(1-p))
#c)

repartitions <- function(x)
{
  dbinom(0,n,p) + sum(sapply(1:6,function(i) dbinom(i,n,p)*pgamma(x,alpha*i,lambda)))
}
sapply(c(0,10,50,100),function(x) repartitions(x))
kappa <- c(0.5,0.9,0.99,0.999,0.9999)
(VaRS <- sapply(kappa,function(x) uniroot(function(y) repartitions(y)-x,c(0,10000000))[[1]]))


TVaR <- function(x,k)
{
  sum(sapply(1:6,function(i) dbinom(i,n,p)*alpha*i/lambda*(1-pgamma(x,alpha*i+1,lambda))))/(1-k)
}
VaRS
(TVaRS <- sapply(1:5,function(i) TVaR(VaRS[i],kappa[i])))  ## ca semble correct

  
  
  







###### Numero 6 #############
rm(list=ls())
n <- 2
p <- 1/2
alpha <- 1.2
beta <- 1/10
#a) M1 suit M2 qui suit M3 qui suit une binominale(2,1/2)
#b) S suit une bincomp(2,1/2,FB) ou FB suit une gamma(3*1.2,beta)
#c)

repartitions <- function(x)
{
  dbinom(0,n,p) + dbinom(1,n,p)*pgamma(x,3*alpha,beta) + dbinom(2,n,p)*pgamma(x,6*alpha,beta)
}
sapply(c(0,10,50,100),function(x) repartitions(x))
kappa <- c(0.5,0.9,0.99,0.999,0.9999)
(VaRS <- sapply(kappa,function(x) uniroot(function(y) repartitions(y)-x,c(0,10000000))[[1]]))

TVaR <- function(x,k)
{
  sum(sapply(1:2,function(i) dbinom(i,n,p)*3*alpha*i/beta*(1-pgamma(x,3*alpha*i+1,beta))))/(1-k)
}
VaRS
(TVaRS <- sapply(1:5,function(i) TVaR(VaRS[i],kappa[i]))) ## Ca semble ok




###### Numero 7 #############
rm(list=ls())
alpha <- 3.6
beta <- 1/10
#a) M1 suit M2 qui suit M3 qui suit une binominale(2,1/2)
#b) S suit une gamme(alpha,beta)
#c)
ES <- alpha/beta
VarS <- alpha/beta^2
pgamma(c(0,10,50,100),alpha,beta)
kappa <- c(0.5,0.9,0.99,0.999,0.9999)
VaRS <- qgamma(kappa,alpha,beta)
VaRS
(TVaRS <- sapply(1:5,function(i) alpha/beta * (1-pgamma(VaRS[i],alpha+1,beta))/(1-kappa[i])))