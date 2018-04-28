## Question 3

rm(list = ls())
p_binom <- c(5,0.3)
p_pois <- 0.1

Fm <- function(x) sum(sapply(0:5,function(i) dbinom(i,5,0.3)*ppois(x,0.1*(i+1))))

# A)
#E[X]
EX <- 100 * (prod(p_binom)+1)

#VaR0.9(X)
x <- 0
while(Fm(x)<0.9){
    x <- x+1
}
x*1000

#TVaR0.9(X)
E <- numeric(0)
for(i in 0:5){
    E <- c(E,sum(sapply(2:100,function(j) dbinom(i,5,0.3)*j*dpois(j,0.1*(i+1)))))
}
TVaR0.9X <- (sum(E) + Fm(1)-0.9)/0.1*1000

# B)
#E[W]
EW <- 100*(prod(p_binom)+1)

#VaR0.9(W)
Ft <- function(x) sum(sapply(0:5,function(i) dbinom(i,5,0.3)*ppois(x,10*(i+1))))
## P(W/10 < X/10|theta = k) ~ Pois(n*0.1(k+1))
x <- 0
while(Ft(x/10)<0.9){
    x <- x+1
}
x

#TVaR0.9(W)

## Pour trouver ça, (Poser S/1000 | theta) = (Sum(M)| theta). Ainsi, on a S/1000 suit
## une poisson (n*lambda). J'ai posé T = S/1000 = W/10 et j'ai tout converti mes W
## en T pcq c'est plus facile ainsi.

E <- numeric(0)
for(i in 0:5){
    E <- c(E,sum(sapply(41:1000,function(j) dbinom(i,5,0.3)*j*dpois(j,10*(i+1)))))
}
TVaR0.9W <- (sum(E) + 40*(Ft(40)-0.9))*100

# C)
# i) plus grand que espérance
# ii) sous-additif
# iii)
1-(TVaR0.9W-EW)/(TVaR0.9X-EX)
## Ça confirme ma valeur de TVaR de X .

# D)
# iii) 
zk <- sapply(0:5,function(i) 1000*0.1*(i+1)) # E[X|theta = i]
pZk <- sapply(0:5,function(i) dbinom(i,5,0.3))
min(zk[cumsum(pZk) > 0.9]) ## VaR0.9Z