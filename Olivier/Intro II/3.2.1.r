## 3.2.1
r <- 5
q <- 1/5
a <- 2
b <- 1/5000
tol <- 150
x_e <- r*(1-q)/q*a/b
x_v <- r*(1-q)/q*a/b^2+(a/b)^2*r*(1-q)/q^2

fm <- function(x) {
    dnbinom(x,r,q)
}
Fb <- function(x,k,i = 0){
    pgamma(x,k*a + i,b)
}
Fx <- function(x){
    fm(0) + sum(sapply(seq(tol),function(k) fm(k)*Fb(x,k)))
}

VaR <- function(u) uniroot(function(x) Fx(x)-u,c(0,1e6))$root

TVaR <- function(k){
    v <- VaR(k)
    1/(1-k)*sum(sapply(seq(tol),function(k) fm(k) * a*k/b*(1-Fb(v,k,1))))
    
}


