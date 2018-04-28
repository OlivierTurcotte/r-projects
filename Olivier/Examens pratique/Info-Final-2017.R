##### Question 1  ####
rm(list = ls())


# A) 

Qx1 <- function(u) 1/sqrt(-log(u))*1000
Qx2 <- function(u) 1/sqrt(1/u-1)*1000
Qx3 <- function(u) log(-2*(exp(log(2)*(u-1))-1))*-2000

# B)
m <- 100000
set.seed(20160419)
values <- matrix(0,ncol = 3, nrow = m)

for(i in seq_len(m)){
    values[i,1] <- Qx1(runif(1))
    values[i,2] <- Qx2(runif(1))
    values[i,3] <- Qx3(runif(1))
}
values[3,]

# C)
S <- apply(values,1,sum)
sum(S>20000)/m

# D)

TVaR <- function(k) mean(S[S > quantile(S,k,type = 1)] )
TVaR(0)
TVaR(0.9999)

# E)
apply(values[S > quantile(S,0.9999,type = 1),],2,mean)

# F)
noF <- apply(cbind(values[,1],values[,3]),1,sum)
sum(values[,2][noF <= 1500 | noF > 150000])/m

##### Question 2 #####
rm(list= ls())
al <- 0.001
be <- 0.00004
ga <- log(1.09)

prime <- 1000
x <- 30

b <- function(x) be*exp(ga*x)
Qyx <- function(x,u) log(log(1-u)*ga/-b(x)+1)/ga

v <- function(t) exp(-0.03*t)

# B)
set.seed(20160419)
m <- 100000
values <- matrix(0,ncol = 2, nrow = m)
for(i in seq(m)){
    values[i,1] <- qexp(runif(1),al)
    values[i,2] <- Qyx(x,runif(1))
}
Tx <- apply(values,1,min)

# C)
mean(Tx)

# D)
sum(values[,1] < values[,2])/m

# E)
Z <- prime*sapply(Tx,v)
Z[1]

# F)
mean(Z)

##### Question 3 #####
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

##### Question 4 #####
rm(list = ls())
n <- 35
age <- 45
