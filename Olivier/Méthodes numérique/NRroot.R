## Newton-Raphson

NRroot <- function(expr, x_1, variable = "x",e = 10e-6 , nmax = 1000){
    n <- 1
    l <- list() ## Environment used in the evaluation
    derive <- D(expr,variable)
    
    while(n < nmax){
        l[[variable]] <- x_1 ## Setting the value at which I need to eval the functions
        x_2 <- x_1 - eval(expr,l)/eval(derive,l)
        if(abs(x_2-x_1)/abs(x_2) < e) return(list(root = x_2, nb.iter = n))
        x_1 <- x_2
        n <- n +1
    }
    stop("Aucune convergence")
}

NRroot(expression(x^3 + 4*x^2-10), 1.5)
NRroot(expression((1-(1+i)^-10)/i - 8.2218),0.1,"i")

H <- expression(exp(0.001312 * x + 2.778538) - exp(-0.001431 * x + 2.016340) - 9)
NRroot(H,15)
