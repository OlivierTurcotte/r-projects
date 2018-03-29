## Exercices 

# 5.1 ----

secante <- function(f,x0,x1,echo= F,e= 10e-6,MAX.ITER = 100){
    n <- 1
    while(n < MAX.ITER){
        X <- x1 - f(x1)*(x1-x0)/(f(x1)-f(x0))
        if(abs(X-x1)/abs(X) < e) return(list(root = X, nb.iter = n))
        if(echo) print(paste(n,X,sep =" : "))
        n <- n +1
        x0 <- x1
        x1 <- X
    }
    stop("Maximum number of iterations without convergence.")
    
}

secante(function(x) x^3 + 4*x^2-10,1,2)
## There you go



# 5.2 ----
rm(list = ls())



