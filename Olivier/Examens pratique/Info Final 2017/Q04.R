##### Question 4 #####
rm(list = ls())
n <- 35
age <- 45
g <- 200
d <- 0.03

v <- function(t) exp(-d*t)
annuity <- function(t) (1-v(t))/(1-v(1))

# A)
qy <- function(y) exp(-9.9+0.09*y)/(1+exp(-9.9+0.09*y))

# B)
FBarre <- function(k) prod(sapply(age:(age+k-1),function(i) 1-qy(i)))
FBarre(20)

# C)
200*sum(sapply(0:(n-1) , function(i) v(i)*FBarre(i)))
EX <- g*sum(sapply(0:(n-1),function(i) annuity(i+1)*(FBarre(i)-FBarre(i+1))))+g*annuity(n)*FBarre(n)

# D)
EX2 <- g^2*(sum(sapply(0:(n-1),function(k) annuity(k+1)^2*(FBarre(k)-FBarre(k+1))))+annuity(n)^2*FBarre(n))
sqrt(EX2 - EX^2)

# E)
g*annuity(1)
g*annuity(10)
g*annuity(35)

(1-FBarre(10))-(1-FBarre(9))

FBarre(n)

log(1-EX/g*(1-v(1)))/log(v(1))
annuity(28)*g- EX
## Il faut donc etre suppérieur à 29
FBarre(29)
