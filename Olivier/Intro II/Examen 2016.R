
# Question 1 ----
rm(list = ls())
n <- c(10,400,1000,2000)
a <-  0.5 ; b <- 0.5

psi <- function(u,n,prime) {
    1 - pgamma(u+n*prime,a*n,b)
}

# b)
pgamma(c(10,100,1000),a*c(10,100,1000),b)

# c)
e_x <- a/b

# d)
psi(20,n,1.1)
# n = 0.1
# Au départ, la probabilité de ruine est plus petite, puis elle augmente 
# pour finalement devenir plus petite qu'initialement.

# e)
psi(20,n,0.9)
# n = -0.1
# La probabilité de ruine ne fait qu'augmenter, jusqu'à devenir presque certaine.

# f)
root <- uniroot(function(x) psi(20,1000,x) - 0.01,c(-2,2))$root
n_grec <- root - 1

# Question 2 ----
rm(list =ls())
library(actuar)
n <- 1000
lambda <- 0.006
a1 <- 1.5 ; b1 <- 1/(1500)

fmx <- function(x){
    dpois(x,lambda)
}

Fx <- function(x,n = 10){
    fmx(0) + sum(sapply(seq(n),function(i) fmx(i)*pgamma(x,a1 * i,b1)))
}

# a)
# b)
Fx(0)
Fx(40)
Fx(10000)

# c)
optimize(function(x) abs(Fx(x) - 0.99), c(0,100))$minimum
# La VaR 0.99 = 0
# d)
# e)
1/(1-0.99)*lambda*a1/b1

# f)
fmw <- function(x){
    dpois(x,1000*lambda)
}

Fw <- function(x){
    fmw(0) + sum(sapply(seq(25),function(i) fmw(i) *pgamma(x,i*a1,b1*n)))
}
Fw(0)
Fw(40)

# g)
root <- uniroot(function(x) Fw(x) -0.99,c(0,40))$root
Fw(root) # Vérif

# h)
k <- 0.99
1/(1-k)*sum(sapply(seq(25),function(i) fmw(i)*a1*i/(b1*n)*(1-pgamma(root,a1*i+1,b1*n))))

# i)



# Question 3 ----
ai <- c(4,2)
bi <- c(1/100,1/200)

f <- function(x){
    b <- 1/100
    a <- 6
    o <- 0.25
    p_k <- function(k){
        
        l_k <- function(k){
            sum(sapply(1:2,function(i) ai[i]/k*(1-bi[i]/b)^k))
        }
        e_k <- function(k) {
            if(k == 0) return(1)
            1/k*sum(sapply(seq(k),function(i) i*l_k(i)*e_k(k-i)))
        }
        o*e_k(k)
    }
    sum(sapply(0:12,function(k) p_k(k)*pgamma(x,a+k,b)))
}
Ft <- function(x){
    0.75*pgamma(x,4,1/100)+0.25*f(x)
}
# J'ai fait 12 itérations car à 100, ÇA CRASH TABARNAK. 
#                       - Olivier Turcotte 
sapply(c(100,500,800,1000),Ft)

# Question 4 ----

n <- 3
a <- 1
b <- 3
# Pour savoir comment je suis arrivé à cette distribution
# consulté le onenote.
fN <- function(k){
    choose(3,k)*beta(k+a,n-k+b)/beta(a,b)
}
# c)
fN(0:3)

# d)
1000*fN(3)

fN2 <- function(k){
    dbinom(k,3,a/(a+b))
}
# c2 )
fN2(0:3)

# d2)
1000*fN2(3)
