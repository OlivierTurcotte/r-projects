stopLoss <- function(d,density,n = 1e5){
    sum(density((d+1):n)*1:(n-d))
}

stopLoss(2,function(x) dbinom(x,4,0.5),4)

