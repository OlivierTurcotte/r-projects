# Q04
q <- 0.1
a <- 0.5
b <- 0.05

# A) ####
F_x <- function(x) 0.9 + 0.1*pgamma(x,a,b)
F_x(0)
F_x(40)

VaR_x <- function(k) ifelse(k < 0.9, 0 , uniroot(function(x) F_x(x)-k,c(0,100000))$root)
VaR_x(0.8)
VaR_x(0.99)

TVaR_x <- function(k) q*a/b*(1-pgamma(VaR_x(k),a+1,b))/(1-k)
TVaR_x(0.8)
TVaR_x(.99)

# B) ####
n <- 200
F_s <- function(x) dbinom(0,n,q) + sum(sapply(seq(n),function(i) dbinom(i,n,q) * pgamma(x,a*i,b)))
F_w <- function(x) F_s(n*x)

VaR_w <- function(k) uniroot(function(x) F_s(x)-k,c(0,10000))$root/n
VaR_w(0.8)
VaR_w(0.99)

TVaR_w <- function(k){
    v <- VaR_w(k)*n
    sum(sapply(seq(n),function(i) dbinom(i,n,q)*a*i/b*(1-pgamma(v,a*i+1,b))))/(1-k)/n
}
TVaR_w(0.8)
TVaR_w(0.99)

# C) ####

BVaR <- sapply(c(0.8,0.99),function(k) VaR_x(k) - VaR_w(k))
BTVaR <- sapply(c(0.8,0.99),function(k) TVaR_x(k)-TVaR_w(k))

dbinom(1,2,0.3)
0.42*(1-0.594)+0.09*(1-0.1429)
