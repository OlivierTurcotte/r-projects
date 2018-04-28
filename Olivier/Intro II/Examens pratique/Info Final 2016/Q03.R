# Q03
rm(list = ls())
# A)
n <- 40
x <- 60
g <- 10000
delta <- 0.03

v <- function(t) exp(-delta*t)

annuity <- function(t) (1-v(t))/(1-v(1))

t2060 <- cbind(0:110, c(0.00533, 0.00029, 0.00022, 0.00018, 0.00015, 0.00013, 0.00011, 1e-04, 1e-04, 1e-04,
                        1e-04, 0.00011, 0.00013, 0.00016, 0.00022, 3e-04, 0.00042, 0.00054, 0.00064, 0.00071, 0.00078, 0.00082,
                        0.00083, 0.00082, 0.00079, 0.00075, 0.00073, 0.00072, 0.00073, 0.00074, 0.00077, 0.00081, 0.00086, 9e-04,
                        0.00096, 0.00101, 0.00108, 0.00115, 0.00123, 0.00131, 0.00141, 0.00151, 0.00163, 0.00176, 0.00191, 0.00207,
                        0.00225, 0.00245, 0.00268, 0.00293, 0.00322, 0.00354, 0.00389, 0.00427, 0.0047, 0.00516, 0.00568, 0.00624,
                        0.00686, 0.00755, 0.0083, 0.00914, 0.01005, 0.01106, 0.01217, 0.0134, 0.01475, 0.01624, 0.01788, 0.01969,
                        0.02168, 0.02388, 0.0263, 0.02898, 0.03193, 0.03518, 0.03877, 0.04273, 0.04711, 0.05193, 0.05726, 0.06314,
                        0.06963, 0.0768, 0.08471, 0.09345, 0.10311, 0.11378, 0.12556, 0.13858, 0.15297, 0.16852, 0.1849, 0.20204,
                        0.21988, 0.2344, 0.25251, 0.27115, 0.2902, 0.30953, 0.32903, 0.34856, 0.36799, 0.38718, 0.40601, 0.42436,
                        0.44214, 0.45925, 0.47562, 0.4912, 1))


qx <- t2060[,2]
#SurvieT <- function(t,age = x) prod(1-qx[(age+1):(age+t)]) 
# Cette fonction est certes plus compacte et fonctionnelle, 
# mais dans un scénario où elle doit être utilisé
# à plusieurs reprises, son alternative en bas est plus rapide .

Fx_barre <- cumprod(1-qx)
FT_barre <- function(t) Fx_barre[x+t]/Fx_barre[x]

# B)
EZ <- sum(sapply(0:(n-1),function(k) v(k)*FT_barre(k)))*g

# C)
# D)

EZ2 <- g^2*(sum(sapply(0:(n-1),function(k) annuity(k+1)^2*(FT_barre(k) - FT_barre(k+1)))) + annuity(n)^2*FT_barre(n))
Var2 <- EZ2 - EZ^2

Z <- g*annuity(1:n)
fz <- c(sapply(1:(n-1), function(i) FT_barre(i-1)-FT_barre(i)),FT_barre(n-1))
sum(fz)

sum(sapply(1:n,function(k) Z[k]^2*fz[k]))-EZ^2

Fz <- cumsum(fz)
# E)
Z[1]
Z[2]

Z[n-1]
Z[n]
# F)
fz[1]

# G)
fz[40]

# H)
x <- 1
while(Fz[x] < 0.05){
    x <- x+1
}
VaR0.05 <- Z[x]
Fz[x]
# Ou
cbind(Z,Fz)# Et regarder

x <- 1
while(Fz[x] < 0.95){
    x <- x+1
}
VaR0.95 <- Z[x]
Fz[x]

# I)
#TVaR0.95
(sum(sapply(38:40, function(k) Z[k]*fz[k]))+Z[37]*(Fz[37]-0.95))/0.05

