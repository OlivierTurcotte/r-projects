rm(list=ls())

# Paramètres ----
lambda <- 2
a <- 0.5
b <- 1/200
tol <- 25
kappa <- c(0.1, 0.5, 0.9, 0.99, 0.999, 0.9999)
    
# Fonctions ----
fm <- function(x) dpois(x,lambda)
Fb <- function(x,a,b) pgamma(x,a,b)
Fx <- function(x) fm(0)+sum(sapply(seq(tol),function(k) fm(k)*Fb(x,a*k,b)))
ETronque <- function(d) sum(sapply(seq(0,tol),function(k) fm(k)*a*k/b*(1-pgamma(d,a*k+1,b)) ))
StopLossX <- function(d) ETronque(d) - d*(1-Fx(d))
VaR <- function(k) ifelse(k <= Fx(0), 0,uniroot(function(x) Fx(x)-k,c(0,5000))$root)

# Réponses ----

# a) 
lambda*a/b
lambda*a/b^2+(a/b)^2*lambda

# b)
no_b <- 1- sapply(seq(0,1000,100),Fx)

# c)
no_c <- sapply(seq(0,1000,100),StopLossX)

# d)
sapply(seq(0,10),function(i) 1/(1-Fx(100*i))*StopLossX(100*i)+100*i)
# ou bien
no_c/no_b+100*0:10

# e)
sapply(0:10,function(i) ETronque(100*Fx(100*i)))

# f)
sapply(kappa,VaR)

# g)
sapply(kappa,function(k) ETronque(VaR(k))/(1-k))

# h)
set.seed(2018)
n <- 1e6
M <- numeric(0)
B <- numeric(n)
for(i in seq(n)){
    M[i] <- qpois(runif(1),lambda)
    if(M[1] > 0){
        B[i] <- sum(qgamma(runif(M[i]),a,b))
    }
}
# a2)
mean(B)
var(B)

# b2)
f <- sapply(0:10,function(k) mean(B>100*k))
1-ecdf(B)(100*0:10)
f
# équivalent mais plus lent. 
1-sapply(0:10,function(k) ecdf(B)(100*k))


# c2)
s <- sapply(0:10,function(k) sum(B[B>100*k]-100*k)/n)

# d2)
s/f+100*0:10

# e2)

