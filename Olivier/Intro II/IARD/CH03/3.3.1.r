# 3.3 : Exercices de dépannages 

# 1) ----
rm(list=ls())
# Paramètres : 
n <- 50  ;   p <- 0.04   ;   a <- 0.5   ;   b <- 1/200
kappa <- c(0.1, 0.5, 0.9, 0.99, 0.999, 0.9999)

# Fonctions :
fmx <- function(x){
    dbinom(x,n,p)
}

Fx <- function(x){
    0.1298858 + sum(sapply(seq(n), function(i) fmx(i)*pgamma(x,a*i,b)))
}

etronque <- function(d){
    sum(sapply(seq(n),function(i) fmx(i)*(a*i)/b*(1-pgamma(d,a*i+1,b))))
}

stopLoss <- function(x){
    etronque(x) - x*(1 - Fx(x))
}

VaR <- function(k,interval = c(0,5000)){
    x <- optimize(function(x) abs(Fx(x) - k), interval)$minimum
    ifelse(x <= fmx(0),0,x)
}


TVaR <- function(k,interval= c(0,5000)){
    var <- VaR(k)
    1/(1-k)*stopLoss(var)+var
}

# *********         Réponses  :     **************

# a)
e_x <- (n*p)*(a/b)
v_x <- (n*p)*(a/b^2)+(n*p*(1-p))*(a/b)^2

# b)
sapply(0:10,function(i) 1-Fx(100*i))

# c)
sapply(0:10,function(i) stopLoss(100*i))

# d)
k <- sapply(0:10,function(i) Fx(100*i))
tvar <- sapply(seq(k), function(i) TVaR(k[i]))

# e)
sapply(seq(k), function(i) etronque(100*k[i]))

# f)
var <- sapply(seq(kappa),function(i) VaR(kappa[i]))

# g)
tvar2 <- sapply(seq(kappa),function(i) 1/(1-kappa[i])*stopLoss(var[i])+var[i])

# h)
set.seed(2018)
m <- 1000000
x <- numeric(0)

# Fuck Deniz pis ses fonctions fuck all.
#           - Olivier Turcotte
for(i in seq(m)){
    M <- qbinom(runif(1),n,p)
    if( M == 0){
        x[i] <- 0
        next
    } 
    x[i] <- sum(qgamma(runif(M),a,b))
}
(e_x_sim <- mean(x))
(v_x_sim <- mean(x^2)-e_x_sim^2)
data.frame(e_x,e_x_sim,v_x,v_x_sim)

Fx_sim <- function(y){
    mean(x<=y)
}

VaR_sim <- function(k) {
    sort(x)[m*k]
}
TVaR_sim <- function(k){
    mean(x[x>VaR_sim(k)])
}



# 2) ----
rm(list=ls())

# Paramètres : 
lambda <- 2 ; a <- 1/2 ; b <- 1/200
up <- 20 ; sum(dpois(0:up,lambda)) # Jusqu'où dois-je sommer pour ma poisson.
k <- 0:10


# Fonctions : 
fmx <- function(x){
    dpois(x,lambda)
}

Fx <- function(x){
    0.1353353+sum(sapply(seq(k),function(i) fmx(i)*pgamma(x,a*i,b)))
}

e_tronque <- function(d) {
    sum(sapply(seq(k),function(i) fmx(i)*a*i/b*(1-pgamma(d,a*i+1,b))))
}

stop-loss <- function(d){
    etronque(d)+d*(1-Fx(d))
}

VaR <- function(k,interval = c(0,10000)){
    if(k == 0) return(0)
    else if(k==1) return(Inf)
    else optimize(function(x) abs(Fx(x)-k),interval = interval)$minimum
}


# a)
e_x <- lambda * a/b
v_x <- lambda*a/b^2+lambda*(a/b)^2

# b)
(sapply(seq(k),function(i) 1-Fx(100*k[i])))
