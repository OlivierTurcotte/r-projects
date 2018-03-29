
costFunction <- function(hypothesis,paramChar)
{
    ## Parser afin de déterminer le nombre de paramètres dans notre hypothèse.
    ## Sera utilisé lorsque l'hypothèse sera déterminé à l'aide d'un réseau de
    ## neurones.
    ## @hypothese  :  String, cost function à minimiser
    ## @paramChar  :  Char, charactère attribué aux paramètres à trouver.
    hypothesis <- "t1 * exp(-t1 * x)"
    paramChar <- "t"
    paramFinder <- function(hypothesis, paramChar){
        pattern = paste(paramChar,"\\d+",sep = "")
        param <- sort(unique(regmatches(hypothesis,gregexpr(pattern, hypothesis))[[1]]))## greg : Tous les matchs du pattern avec l'hypothèse.
        
        param
    }
    param <- paramFinder(hypothesis,paramChar)
    param
    derivande <- paste("(",hypothesis,"- y )")
    derivande_expr <- parse(text = derivande)
    derivative <- list()
    
    ## DON'T FUCKING TOUCH THIS
    for(i in seq_along(param)){
        derivative[[param[i]]] <- paste("sum(",derivande,"* (",deparse(D(derivande_expr,param[i]),width.cutoff = 500),") )/m")
    }
    
    me <- list(
        Hypothesis = hypothesis,
        Param = param,
        CF = paste("sum(",derivande, "^2)/(2*m)", sep = " "),
        CF_prime = derivative
    )
    
    return(me)
}

lambda <- 10
n <- 10
m <- 10
x <- numeric(0)
y <- numeric(0)
for (i in seq_len(m)){
    x_temp <- rexp(n,1/(lambda+i))
    y_temp <- dexp(x_temp,1/(lambda+i))
    x <- append(x,x_temp)
    y <- append(y,y_temp)
}

plot(x,y)

CF <- costFunction("t1*exp(-t1*x)","t")
f <- function(t) {
    assign("t1",t)
    eval(parse(text = CF$CF_prime$t1),list(x=x, y=y, m = length(y)))
}

uniroot(function(x) f(x)-0,c(0,1))$root

## Essayer avec uniroot au lieu de gradient descent.
## Aussi, essayer avec abs()
