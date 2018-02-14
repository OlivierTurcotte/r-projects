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
## u = exp(-x/2) du = -1/2 exp(-x/2)dx  ca devient  -2*ln(u)^2
rm(list=ls())
monte_carlo <- function(n)
{
  u <- runif(n)
  mean(2*(-log(u^2))^2 * sin(pi*(-1*log(u^2))))
}
monte_carlo(1e8)

  




###-----------------------------------Numero 4------------------------------------


x <- replicate(10000,function(x) rnorm(25))
mean(sort(x)[[5]])