# No.1
{
    #a) ----
    sample <- rexp(100)
    skewness <- function(x) mean((x-mean(x))^3)/sd(x)^3
    exp_skew <- skewness(sample)
    hist(sample,prob = T)
    
    #b) ----
    
    lambda <- 1/mean(sample)
    skew <- sapply(1:50,function(x) skewness(rexp(100,lambda)))
    var_skew <- var(skew)
    IC <- sapply(c(-1,1),function(i) exp_skew + i*qnorm(0.975)*sd(skew))
    IC
    
    #c) ----
    
    real_skew <- 2
    real_skew > IC[1] && real_skew < IC[2] ## Verif
    
}



# No.2

{
    #a
    prob <- c(0.25,0.35,0.5,0.6,0.75,0.85)
    u <- sapply(prob,function(x) qexp(x,1))
    limited_loss <- function(data,min) sapply(min,function(y) 1/length(data) *sum(sapply(data, function(x)  min(y,x))))
    lim_exp <- limited_loss(sample,u)
    
    #b
    bootstrap_IC <- function(val){
        data <- lapply(1:50,function(x) rexp(100,lambda))
        simul <- sapply(1:50,function(x) limited_loss(data[[x]],val))
        IC <- sapply(c(-1,1),function(i) limited_loss(sample,val) + i*qnorm(0.975)*sd(simul))
        IC
    }
    lapply(c(qexp(0.5),qexp(0.75)),bootstrap_IC)
}
