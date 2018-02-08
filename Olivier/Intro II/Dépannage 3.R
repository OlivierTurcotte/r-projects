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
m <- 1000000
set.seed(20160419)
U <- matrix(runif(m*3),ncol = 3, byrow = T)
X <- matrix(c(rlnorm(m,log(10)-0.18,0.6),rexp(m,0.1),rgamma(m,2,1/5)),ncol = 3)
S <- apply(X,1,sum)
kappa <- c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)

# e)

sorted_X <- apply(X,2,sort)
esperance_X <- apply(X,2,mean)
variance_X <- apply(X,2,function(x) mean(x^2)) - esperance_X^2
VaR_X <- sorted_X[m*kappa,]
TVaR_X <- sapply(seq(kappa),function(i) apply(sorted_X[(m*kappa[i]+1):m, ],2,mean))

# f)

sorted_S <- sort(S)
esperance_S <- mean(S)
variance_S <- mean(S^2)-esperance_S^2
VaR_S <- sorted_S[m*kappa]
TVaR_S <- sapply(seq(kappa), function(i) mean(sorted_S[(m*kappa[i]+1):m]))
# g)
BM <- sapply(seq(kappa),function(i) sum(TVaR_X[,i])-TVaR_S[i])
BM


