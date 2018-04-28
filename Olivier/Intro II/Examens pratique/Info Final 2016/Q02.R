# Q02

rm(list = ls())
n <- 60
x <- 40
b <- 100000
v <- function(t) exp(-0.04*t)

beta <- 0.00003
ga <- log(1.1)

ux <- function(x) beta*exp(x*ga)
# A)
F_Tx <- expression(exp(-beta*exp(ga*x)/ga*(exp(ga*t)-1)))
ftx <- D(F_Tx,"t")

# B) & C)
EZ <- b*integrate(function(t) -eval(ftx)*v(t),0,60)[[1]]

# D)
# E)
# z = 0
eval(F_Tx,envir = list(t = n))
# z = b/2
eval(F_Tx,envir = list(t = -log(0.5)/0.04))

# F) et G)
b*v(1/ga*log(1-ga/ux(x)*log(1-(1-0.95))))

# H)
## Simulation avec prng congruentiel. Fuck this.
