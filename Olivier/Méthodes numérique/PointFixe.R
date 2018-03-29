#Point Fixe

PointFixe <- function(nmax,expr,x_1,e = 10e-6){
    n <- 1
    while(n < nmax){
        x_2 <- eval(expr,list(x = x_1))
        if(abs(x_1-x_2)/abs(x_1) < e) return(list(root = x_2, nb.iter = n))
        x_1 <- x_2
    }
    print("Aucune convergence")
}
PointFixe(100,expression((1-(1+x)^-10)/8.2218),0.035)

H <- expression((log(9 + exp(-0.001431*x+2.016340)) - 2.778538) / 0.001312)
PointFixe(100,H,31)
