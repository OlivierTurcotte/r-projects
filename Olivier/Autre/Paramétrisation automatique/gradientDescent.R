## Gradient Descent.


gradientDescent <- function(x,y,theta, alpha, n, hypothesis, paramChar = "t"){
    source("costFunction.R")
    J <- costFunction(hypothesis,paramChar)
    m <- length(y)
    temp <- numeric(0)
    
    for(i in seq_len(n)){
        for(i in seq_along(theta)){
            assign(J$Param[i],theta[i])
        }
        for(j in seq_along(J$Param)){
            
            temp[j] <- theta[j] - alpha * eval(parse(text = J$CF_prime[[j]]))
        }
        theta <- temp
    }
    theta
}

