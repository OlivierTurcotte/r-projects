# 1.
{
    n <- 1e4
    u <- runif(n)
    x <- mean(log(5*u+4))
}

# 2.
{
    n <- 1e6
    u <- runif(n)
    v <- runif(n)
    mean(exp(2*u*v)*log(3*u+v^2))

}


# 3.
{
    x <- runif(1e6)
    mean(x^2*sin(pi*x)*exp(-x/2)+sin(pi/x)*exp(-1/(2*x))/x^2)
    
    # Alternative
    x <- rgamma(1e6,3,1/2)
    2^4*mean(sin(x*pi))
}

# 4.
{
    (mean(replicate(10000,sort(rnorm(25))[5]))) # gucci gang
    
}

