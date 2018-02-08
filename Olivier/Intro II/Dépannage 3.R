# Mesure de risque 

# 2.1.1 d) -----------------------------------------------------------------
l <- 1/180
a <- 1/9
VaR <- function(k,n) 1/n*qgamma(k,a*n,l)
VaR(0.05,1)-VaR(0.05,10)
VaR(0.95,1)-VaR(0.95,1000)

# 2.1.1 e) ----------------------------------------------------------------
TVaR <- function(k,n) {
    a/l*(1-pgamma(qgamma(k,a*n,l),a*n+1,l))
}
TVaR(0.05,1)-TVaR(0.05,10)
TVaR(0.95,1)-TVaR(0.95,1000)
# Pas les bonnes réponses cependant.þ



# 2.1.4 ----

# i, ii ,iii, iv, v

n <- 10
lambda <- seq(1/n,1,1/n)

fx <- function(x){
    sum(sapply(seq(n),function(j) prod(lambda[-j]/(lambda[-j]-lambda[j]))*dexp(x,lambda[j])))
}

Fx <- function(x){
    sum(sapply(seq(n),function(j) prod(lambda[-j]/(lambda[-j]-lambda[j]))*pexp(x,lambda[j])))
}
op <- par(mfrow = c(1,2))
plot(Vectorize(fx),0,100,main = "Densité", ylab = "fx(x)")
plot(Vectorize(Fx),0,100,main = "Distribution", ylab = "Fx(x)")

VaR <- function(k){
    ifelse(k >= 0.99999, Inf,uniroot(function(x) Fx(x)- k, c(0,1000000))[[1]])
}
TVaR <- function(k){
    v <- VaR(k)
    if(v == Inf) return(Inf) #Uniroot de VaR ne peut donner des valeurs près de 1, donc je output Inf
    1/(1-k) * sum(sapply(seq(n),function(j) prod(lambda[-j]/(lambda[-j]-lambda[j]))*(dexp(lambda[j],v)+exp(-lambda[j]*v)/lambda[j])))
}
plot(Vectorize(VaR),0,1, main = "VaRk(x)")
plot(Vectorize(TVaR),0,1,main = "TVaRk(x)")

# vi
BM <- function(k){
    v <- VaR(k)
    if(v == Inf) return(-Inf)
    sum(sapply(rep(n),function(i) qexp(k,lambda[i])-v))
}
plot(Vectorize(BM),0,1,main = "Bénéfice Mutualisation")

# vii
BM2 <- function(k){
    t <- TVaR(k)
    if(t == Inf) return(-Inf)
    sum(sapply(rep(n),function(i) qexp(k,lambda[i])+1/lambda[i]-t))
}
BM2(0.5)
plot(Vectorize(BM2),0,1, main = "Bénéfice Mutualisation TVaR")

# viii
µ <- sum(1/lambda)
o <- sum(1/lambda^2)/n^2
plot(Vectorize(fx),0,1000)
curve(dnorm(x,µ,sqrt(o)),0,1000,add = T, col = "red")


# 2.3.3 ----

# Initialisation
mu <- log(10) - 0.18
sd <- 0.6
be_exp <- 1/10
al_ga <- 2
be_ga <- 1/5
m <- 1000000
set.seed(20160419)
U <- runif(3*m)
kappa <- c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)

x1 <- sapply(seq(1,3*m,3),function(i) qlnorm(U[i],mu,sd))
x2 <- sapply(seq(2,3*m,3),function(i) qexp(U[i],be_exp))
x3 <- sapply(seq(3,3*m,3),function(i) qgamma(U[i],al_ga,be_ga))
S <- x1 + x2 + x3


# e)
e_x1 <- mean(x1)
e_x2 <- mean(x2)
e_x3 <- mean(x3)

v_x1 <- mean(x1^2)-e_x1^2
v_x2 <- mean(x2^2)-e_x2^2
v_x3 <- mean(x3^2)-e_x3^2

s_x1 <- sort(x1)
s_x2 <- sort(x2)
s_x3 <- sort(x3)


VaR_x1 <- s_x1[m*kappa]
VaR_x2 <- s_x2[m*kappa]
VaR_x3 <- s_x3[m*kappa]

TVaR_x1 <- sapply(seq(kappa),function(i) mean(s_x1[s_x1 > VaR_x1[i]]))
TVaR_x2 <- sapply(seq(kappa),function(i) mean(s_x2[s_x2 > VaR_x2[i]]))
TVaR_x3 <- sapply(seq(kappa),function(i) mean(s_x3[s_x3 > VaR_x3[i]]))

# f)

sorted_S <- sort(S)
esperance_S <- mean(S)
variance_S <- mean(S^2)-esperance_S^2
VaR_S <- sorted_S[m*kappa]
TVaR_S <- sapply(seq(kappa), function(i) mean(sorted_S[sorted_S > VaR_S[i]]))

# g)

BM1 <- VaR_x1+VaR_x2+VaR_x3-VaR_S
BM2 <- TVaR_x1+TVaR_x2+TVaR_x3-TVaR_S
