rm(list=ls())

## Classe vAleatoire : 
## 
## @values          : Valeurs de l'échantillon
## @distribution    : Distribution de l'échantillon
vAleatoire <- function(values,distribution){
    me <- list(values = values,
               distribution = distribution,
               esperance = sum(values*distribution))
    me[["variance"]] <- sum(values^2*distribution)-me$esperance^2
    
    class(me) <- append(class(me),"vAleatoire")
    return(me)
}

## @x   : Objet vAleatoire
## @d   : Valeur de tronquage
getE_Tronque <- function(x,d){
    sum(x$values[x$values>d]*x$distribution[x$values>d])
}
## @x   : Objet vAleatoire
## @d   : Valeur filtre
getStopLoss <- function(x,d){
    getE_Tronque(x,d)-d*sum(x$distribution[x$values>d])
}

## @x       : Objet vAleatoire
## @kappa   : Valeur de kappa
## @return  : Retourne la VaR_k(x) de la vAleatoire.
getVaR <- function(x,kappa){
    x$values[min(which(cumsum(x$distribution) >= kappa))]
}

## @x       : Objet vAleatoire
## @kappa   : Valeur de kappa
## @return  : Retourne la TVaR_k(x) de la vAleatoire.
getTVaR <- function(x,kappa){
    var <- getVaR(x,kappa)
    1/(1-kappa)*getStopLoss(x,var)+var
}

values <- 0:50
lambda <- 5
kappa <- c(0.0001, 0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999, 0.9999)

r <- c(0.5, 5)
q <- c(10 / 11, 0.5)

#List initiation
listVA <- list()
listVA[[1]] <- vAleatoire(values,dpois(values,lambda))
listVA[[2]] <- vAleatoire(values,dnbinom(values, r[1], q[1]))
listVA[[3]] <- vAleatoire(values,dnbinom(values, r[2], q[2]))

# Vérification du bon nombre de valeurs : 
sapply(seq(listVA),function(x) sum(listVA[[x]]$distribution))

# ----------------   Numéro 1 ----------------------

# VaR de X pour les hypothèses : 
VaR <- 1000*sapply(seq(listVA),function(i) 
    sapply(seq(kappa),function(j) getVaR(listVA[[i]],kappa[j])))
VaR
##TEST 
1000*qpois(kappa,lambda = lambda)

# TVaR de X pour les hypothèses:
TVaR <- 1000*sapply(seq(listVA),function(i) 
    sapply(seq(kappa),function(j) getTVaR(listVA[[i]],kappa[j])))

do.call(getVaR, list(listVA,kappa))
f <- function(x,y){
    print(x+y)

}
do.call(f,list(c(1,2),c(3,4,5)))
  res <- data.frame(kappa, VaR, TVaR)
colnames(res) <- c("Kappa", "VaR Pois", "VaR BN1", "VaR BN2", 
                   "TVaR Pois", "TVaR BN1", "TVaR BN2")
res

# ----------------   Numéro 2 ----------------------

a <- 1/4
b <- 1/4
gamma <- list()
gamma[["distribution"]] <- function(x) dgamma(x,shape=a,rate=b)
gamma[["esperance"]] <- integrate(function(x) x*gamma$distribution(x),lower=0,upper = Inf)[[1]]
gamma[["variance"]] <- integrate(function(x) x^2*gamma$distribution(x),lower = 0, upper=Inf)[[1]]-gamma$esperance^2
gamma[["quantile"]] <- function(x) qgamma(x,shape=0.25,rate =0.25)
gamma[["VaR"]] <- gamma$quantile
gamma[["stoploss"]] <- function(d) integrate(function(x) (x-d)*gamma$distribution(x),lower=d,upper=Inf)[[1]]
gamma[["TVaR"]] <- function(k) 1/(1-k)*gamma$stoploss(gamma$VaR(k))+gamma$VaR(k) 

# A)
gamma$esperance
gamma$variance

# B)
gamma$VaR(0.9995)
# C)
gamma$stoploss(gamma$VaR(0.9995))
# D)
gamma$TVaR(0.9995)


# ----------------   Numéro 3 ----------------------

#Formule de la variance en fonction de sigma afin de trouver ça racine.
f <- function (x) (exp(x^2)-1)*exp(2*(-0.5*x^2)+x^2)-4

sigma <- uniroot(f, c(0,3) ,tol=0.00001)[[1]]
mu <- -0.5*sigma^2

density <- function(x) dlnorm(x,meanlog = mu,sdlog = sigma)
esperance <- integrate(function(x) x*density(x),lower = -Inf, upper = Inf)[[1]]
variance_test <- integrate(function(x) x^2*density(x),lower = -Inf,upper = Inf)[[1]]-esperance^2
# Donc, on a le bon sigma !

#VaR0.9995(X) = Fx^-1(0.9995)
k <- 0.995
v <- qlnorm(k,meanlog = mu, sdlog = sigma)
v
#Stop-Loss(VaR0.9995(x))

integrate(function(x) max(x-v,0)*density(x),lower = -Inf,upper = Inf)[[1]]
sl <- integrate(function(x) (x-v)*density(x),lower = v,upper = Inf)[[1]]
# Weird, pas le même résultat, mais c'est logiquement la même opération
# Je vais garder le deuxième, il fait plus de sens.
(tvar <- 1/(1-k)*sl+v)

## Numéro 4
rm(list=ls())

library(pracma)

balance <- c(0.8,0.2)
mu <- c(0.1,-0.3)
sigma <- c(0.2,0.1)
distribution <- function(x){
    0.8*pnorm(x,mu[1],sigma[1])+0.2*pnorm(x,mu[2],sigma[2])
}

density <- function(x){
    #sum(balance*dnorm(x,mu,sigma))  Cette expression bug, mais lorsqu'exécuté séparément fonctionne
    0.8*dnorm(x,mu[1],sigma[1])+0.2*dnorm(x,mu[2],sigma[2])
}
# a)
esperance <- integrate(function(x) x*density(x),lower = -Inf,upper = Inf)[[1]]
variance <- integrate(function(x) (x-esperance)^2*density(x), lower = -Inf,upper = Inf)[[1]]

# b)
distribution(0)

# c)
kappa <- c(0.0001, 0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999, 0.9999)
opti <- function(x,k) distribution(x) - k
VaR <- sapply(seq(kappa),function(i) uniroot(function(x) opti(x,kappa[i]),c(-50,50),tol =0.000001)[[1]])

TVaR <- sapply(seq(kappa),function(i) 1/(1-kappa[i])*quad(function(x) x*density(x),VaR[i],2))
LTVaR <- sapply(seq(kappa),function(i) 1/kappa[i]*quad(function(x) x*density(x),-2,VaR[i]))
