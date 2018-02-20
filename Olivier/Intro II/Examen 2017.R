# Question 1 ----
rm(list=ls())
r <- 0.4
q <- 10/11
alpha <- 1.5
beta <- 1.5/1000
# a) ii
Fx <- function(x){
    dnbinom(0,r,q) +sum(sapply(1:1000,function(k) dnbinom(k,r,q)*pgamma(x,alpha*k,beta)))
}
sapply(c(0,50,100),Fx)

# a) iii 
# Il faut utiliser uniroot ou optimize afin de calculer la VaR.

# a) iv
VaR0.95 <- optimize(function(x) abs(Fx(x) - 0.95), c(0,100))$minimum
# En analysant le résultat, on se rend comtpe qu'en réalité la VaR0.95 = 0.
VaR0.95 <- 0

# a) v
# Voir onenote

# a) vi
TVaR0.95 <- 1/(1-0.95)*r*(1-q)/q*alpha/beta

# b) Voir onenote

# c) i
# Voir onenote

# c) ii
n <- 200

Fs <- function(x){
    dnbinom(0,r*n,q)+sum(sapply(1:1000,function(k) dnbinom(k,r*n,q)*pgamma(x,alpha*k,beta)))
}
Fs(200*30) # Vérification correcte. Challenge completed biatch.
# c) iii 
# Uniroot ou optimize

# c) iv
VaR0.99 <- optimize(function(x) abs(Fs(x) - 0.99), c(0,100000))$minimum/n

# c) v
TVar0.99 <- 1/(1-0.99)*sum(sapply(1:1000,function(k) dnbinom(k,r*n,q)*alpha*k/beta*(1-pgamma(VaR0.99*n,alpha*k+1,beta))))/n


# Question 2 ----
library(actuar)
r <- 1.5
q <- 1/3
a <- 1.5
lambda <- 50
set.seed(20170222)
n <- 100000
x <- matrix(numeric(n*2),ncol = 2)
for(i in seq(n)){
    m <- qnbinom(runif(1),r,q)
    x[i,1] <- m
    if(m == 0) {
        x[i,2] <- 0
        next
    }
    x[i,2] <- sum(qpareto(runif(m),a,lambda))
}
x[2,]
x[n,] # YEAH !

mean(x[,2]>1200)

sum((x[,2][x[,2]>1200]-1200))/n


# Question 3 ----
rm(list=ls())
a3 <- 2.5
lambda <- c(1/50,20,100)
t <- c(0.5,2.5,2)
q1 <- function(u) (-log(1-u))^(1/t[1])/lambda[1]
q2 <- function(u) (lambda[2]^(t[2]) *u/(1-u))^(1/t[2])
q3 <- function(u) (lambda[3]* (1 - (1-u)^(1/a3))/(1-u)^(1/a3))^(1/t[3])
set.seed(20160419)
u <- runif(3)

q1(u[1])
x1 <- q2(u[2]) 
x2 <- q3(u[3])
(x1^t[2]/(lambda[2]^t[2]+x1^t[2])) / u[2] 
# TEST QUI PROUVE QUE MA QUANTILE EST BONNE ET PAS LA TIENNE TABARNAK DE CALICE
# FUCK YOU, 45 MINUTES À CHERCHER LE PROBLÈME QUAND C'EST TOÉ LE PROBLÈME !


# Question 4 ----
VaR <- function(k) 300*sqrt((k-0.95)/0.05)
VaR(0.999999)
# Pour le reste, c'est pas mal tout du traditionnel

# Question 5 ----
