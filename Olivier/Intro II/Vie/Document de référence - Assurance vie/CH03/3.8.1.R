x <- 50
n <- 50
b <- 100000
d <- 3/100
beta <- 0.000035
ga <- 0.095

## Versions fonctions
b_x <- function(x) beta*exp(ga*x)
kPx <- function(x,t) exp(-b_x(x)/ga*(exp(ga*t)-1))
ftx <- function(x,t) b_x(x+t)*kPx(x,t)

## Versions expressions
ux <- expression(beta*exp(ga*x))
tPx <- expression(exp(-beta*exp(ga*x)/ga*(exp(ga*t)-1)))
ftx <- D(tPx,"t")


v <- function(t) exp(-d*t)

## Personnellement j'utilise les expressions car ça me permet de trouver le mode
## plus aisément en dérivant ftx. Aussi, une fois à l'aise avec cet objet, ça 
## revient au même. 

## 3    ----
b*integrate(function(t) v(t)*-eval(ftx),0,n)[[1]]

## 5    ----
d <- 4/100
eval(tPx,envir = list(x = x,t = -log((4*b/5)/b)/d))


