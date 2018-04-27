beta <- 0.000036
ga <- 0.095418

b <- function(x) beta*exp(ga*x)

Survie_tx <- function(x,t) exp(-b(x)/ga*(exp(ga*t)-1))

## E_k60
E_k60 <- sum(sapply(seq(100000),function(x) Survie_tx(60,x)))

## Sd_k60
sd_k60 <- sqrt(sum(sapply(seq(10000), function(x) (2*x-1)*Survie_tx(60,x)))-E_k60^2)


## E[T60]
SFtx <- expression(exp(-beta*exp(ga*x)/ga*(exp(ga*t)-1)))
E_T60 <- integrate(function(t) t*-eval(D(SFtx,"t"),envir = list(x = 60)),0,130)[[1]]

## Var(T60)
V_T60 <- integrate(function(t) (t^2) * -eval(D(SFtx, "t"), envir = list(x = 60)), 0,130)[[1]] - E_T60^2

## TVaR(T60)
TVaR_T60 <- function(k)
{
    (1/(1-k)/ga) * integrate(function(u) log(1 - log(1 - u) * ga / b(60)), k, 1)[[1]]
}

TVaR_T60(0.99)

#####
## E_k60(12)
E_k60_12 <- sum(sapply(seq(10000), function(x) Survie_tx(60,x/12)))/12

## Sd_k60(12)
sqrt(sum(sapply(seq(10000),function(x) (2*x-1)*Survie_tx(60,x/12)))/144-E_k60_12^2)

