####
###----------------------------------Numero 1-----------------------------------------
integrate(function(x) log(5*x + 4),0,1) #vrai valeur

monte_carlo <- function(n) (1/n)*sum(log(5*runif(n)+ 4))
monte_carlo(1e8)#close enough

###----------------------------------Numero 2----------------------------------------

rm(list=ls())
monte_carlo <- function(n)
  {
  x <- runif(n)
  y <- runif(n)
  mean(exp(2*x*y)*log(3*x + y^2))
}

monte_carlo(1e7) ## seems legit

###-----------------------------------Numero 3------------------------------------





###-----------------------------------Numero 4------------------------------------


x <- replicate(10000,function(x) rnorm(25))
mean(sort(x)[[5]])