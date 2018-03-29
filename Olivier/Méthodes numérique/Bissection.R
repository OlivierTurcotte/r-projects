## Bissection


# L'expression doit être en fonction de X. Je pourrais la rendre dynamique mais 
# fuck that.
Bissection <- function(nmax,expr,a,b,e = 10e-6){
    n <- 2
    y <- (a+b)/2
    ifelse(eval(expr,list(x=a))*eval(expr,list(x = y)) > 0, a <- y, b <- y)
    
    while(n < nmax){
        x <- (a+b)/2
        ifelse(eval(expr,list(x=a))*eval(expr) > 0, a <- x, b <- x)
        
        if(abs(x-y)/abs(y) < e) return(list(root = x, nb.iter = n))
        n <- n+1
        y <- x
    }
    print("Aucune convergence")
    
}

expr <- expression(x^3+4*x^2-10)
Bissection(10000,expr,-1,2)

