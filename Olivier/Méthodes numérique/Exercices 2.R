# 1. 
{
# a)
u1 <- runif(1000)
u2 <- runif(1000)
x1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
x2 <- sqrt(-2*log(u1))*sin(2*pi*u2)
warning <- 0
for (i in seq(1000)){
    if (x1[i] == x2[i]) {
        stop(paste("Paire identique à ", strtoi(i)))
    }
}
# Aucune erreur, donc c'est bon.

# d)
u1 <- runif(100)
u2 <- runif(100)
x1 <- outer(u1,u2, function(u,v) sqrt(-2*log(u))*cos(2*pi*v))
x2 <- outer(u1,u2, function(u,v) sqrt(-2*log(u))*sin(2*pi*v))
hist(x1,prob = T)
curve(dnorm,add = T)
}
# 2.
{
    laplace <- function(n,lambda) {
        values <- runif(n)
        x <- numeric(n)
        for(i in seq(n)){
            x[i] <- ifelse(values[i]<0.5,log(2*values[i])/lambda, -log(2*(1-values[i]))/lambda)
        }
        x
    }
}
# 4.
{
    gammaF <- function(n,alpha){
        
        x <- numeric(n) # Init
        i <- 0
        while(i < n){
            u <- runif(2)
            v <- (alpha - 1/(6 * alpha)) * u[1] / 
                ((alpha - 1) * u[2])
            
            if((2 * (u[2] - 1)/(alpha - 1) +
                  v + 1/v <= 2) |
                    (2 * log(u[2])/(alpha - 1) -
                         log(v) + v <= 1))
            x[i <- i + 1] <- (alpha - 1) * v
        }
        x   
        
    }
    
    x <- gammaF(10000,5)

    hist(x,prob = T)
    curve(dgamma(x,shape = 5,rate = 1), add = T)
    curve(dgamma(x,5,5),add = T)
}
# 6.
{
    library(actuar)
    rpareto <- function(n,alpha,lambda){
        rexp(n,rgamma(n,alpha,lambda))
    }
    x <- rpareto(1000,3,5)
    hist(x,prob = T)
    curve(dpareto(x,3,5),add = T)
}
# 7.
{
    prng <- function(n,a,c,seed,m){
        x <- numeric(n+1)
        x[1] <- seed
        for(i in seq(n)){
            x[i+1] <- (a * x[i] + c) %%m
        }
        x[-1]
    }
    x <- prng(3,65,1,12,2048)/2048
    values <- 1000/ (1-x)^(1/2)
}
# 14.
{
    gammaF <- function(n,alpha){
        c <- (1/alpha + exp(-1))^-1
        x <- numeric(n)
        gx <- function(x) ifelse(x <= 1 , c*x^(alpha-1),c*exp(-x))
        Ginv <- function(u) ifelse(u <= c/alpha,(u*alpha/c)^(1/alpha),log(c/(1-u)))
        i <- 0
        while(i<n){
            u <- runif(2)
            y <- Ginv(u[1])
            if(c*gx(u[2]) <= dgamma(y,alpha,1)){
                x[i <- (i+1)] <- y
            }
        }
        x
    }

    

    set.seed(1)
    x <- gammaF(10000,0.5)
    hist(x, prob = T)

    curve(dgamma(x,0.5,1),add = T)
    
}
# 15.
{
    # LOL
}
# 17.
{
    # a)
    n <- 1000
    n1 <- rbinom(1,n,0.3)
    x <- c( rexp(n1,2), rgamma(n-n1,3,1/2))
    hist(x, prob = T)
    curve(0.3*dexp(x,2)+0.7*dgamma(x,3,1/2),add = T)
    
    # b)
    
    Fx <- function(x) {
        0.5647 + 0.4353*exp(-2)*sum(sapply(0:floor(x),function(i) 2^i/factorial(i)))
    }
    Fx(1.5)
    quantile <- function(u){
        x <- 0
        while(Fx(x) < u){
            x <- x +1
            if(Fx(x) > u){
                x <- x-1
                break
            }
        }
        x
    }
    
    library(actuar)
    n1 <- rbinom(1, n, 0.6236)
    table(c(rep(0, n1), rpois(n - n1, 2)))

    n <- 1000
    u <- runif(n)
    x <- sapply(u,quantile)
    table(x)
    hist(x,prob = T) # Seems legit, good job bro :D
    
    dzmpois(2,2,0.623611449)
    dzmpois(1,2,0.623611449)

    y <- rzmpois(n, 2, p0 = 0.6236)
    
    table(y)
    hist(y,prob = T)
    
    # c)
    mu <- 2
    sigma <- 1
    x <- rpois(n,rinvgauss(n,mu,sigma))
    x
    
    # d)
    n <- 1000
    sapply(rpois(n,2),function(N) sum(rlnorm(N,log(2000),1)))
    
    # e)
    sapply(rnbinom(n,2,0.3), function(N) sum(rlnorm(N,log(2000),1)))
    }
