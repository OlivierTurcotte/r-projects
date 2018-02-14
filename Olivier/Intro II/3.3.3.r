rm(list = ls())

# Paramètres ----
r <- 0.5 ; q <- 0.2     # NBinom
a <- 0.5 ; b <- 1/200   # Gamma
tol <- 1000
kappa <- c(0.1,0.5,0.9,0.99,0.999,0.9999)
# Fonctions ----

Fx <- function(x){
    dnbinom(0,r,q)+ sum(sapply(seq(tol),function(k) dnbinom(k,r,q)*pgamma(x,a*k,b)))
}
ETronque <- function(d){
    sum(sapply(seq(tol),function(k) dnbinom(k,r,q)*a*k/b*(1-pgamma(d,a*k+1,b))))
}
StopLoss <- function(d){
    ETronque(d) -d*(1-Fx(d))
}
# Réponses ----
x <- list()
M <- list()
B <- list()

M$esperance <- r*(1-q)/q   ; M$variance     <- r*(1-q)/q^2
B$esperance <- a/b         ; B$variance     <- a/b^2 


x$esperance <- M$esperance*B$esperance
x$variance  <- M$variance*B$esperance^2 + B$variance*M$esperance

no_b <- 1-sapply(seq(0,1000,100),Fx)
no_c <- sapply(seq(0,1000,100),StopLoss)
no_d <- no_c/no_b+100*0:10
no_e <- sapply(sapply(seq(0,1000,100),Fx),function(i) ETronque(100*i))
no_f <- sapply(kappa,function(k) ifelse(k <= Fx(0),0,uniroot(function(x) Fx(x)-k,c(0,10000) )$root))
no_g <- sapply(seq(kappa),function(i) StopLoss(no_f[i])/(1-kappa[i])+no_f[i])

n <- 1e6
set.seed(2018)
M_s <- numeric(0)
B_s <- numeric(n)
for(i in seq(n)){
    M_s[i] <- qnbinom(runif(1),r,q)
    if(M_s[i]>0){
        B_s[i] <- sum(qgamma(runif(M_s[i]),a,b))
    }
}

StopLoss_s <- function(d){
    sum(B_s[B_s>d]-d)/n
}
ETronque_s <- function(d) sum(B_s[B_s>d])/n


no_a2 <- c(mean(B_s),mean(B_s^2)-mean(B_s)^2)
no_b2 <- 1-sapply(seq(0,1000,100),function(i) ecdf(B_s)(i))
no_c2 <- sapply(seq(0,1000,100),StopLoss_s)
no_d2 <- no_c2/no_b2+100*0:10
no_e2 <- sapply(no_b2,function(i) ETronque_s(100*(-i+1)))
no_f2 <- quantile(B_s,kappa)
no_g2 <- sapply(seq(kappa),function(i) StopLoss_s(no_f2[i])/(1-kappa[i])+no_f2[i])

