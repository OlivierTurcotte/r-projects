# ---- Créations des données
lambda <- 1/10
n <- 100
x <- rexp(n,lambda)
x2 <- rexp(n,lambda+0.01)
y <- dexp(x,lambda)
y2 <- dexp(x2,lambda+0.01)
x <- append(x,x2)
y <- append(y,y2)
plot(x,y)

# ---- Fonctions d'estimations

# Mon algorithme basé sur l'algorithme du gradient, où j'ai cherché à minimisé
# la loss function résultant de la distance quadratique entre mon hypothèse H_theta(x)
# et y. -> 
monAlgo <- function(x,y){
    uniroot(function(a) sum((a*exp(-a*x)-y)*(x*a-1)*exp(-a*x)) - 0, c(0,10))$root
}

# Algorithme du gradient. 
gradient <- function(x,y,theta,alpha,n){
    m <- length(y)
    for(i in seq_len(n)){
        theta <- theta - alpha/m * sum((x*theta-1)*exp(-2*x*theta)*(y*exp(theta*x)-theta))
    }
    theta
}

gradient2 <- function(x,y,theta,alpha,n){
    m <- length(y)
    for(i in seq_len(n)){
        theta <- theta - alpha/m *sum( () )
    }
}

# Régression linéaire. Opérations basés sur :https://www.gerad.ca/Sebastien.Le.Digabel/MTH2302D/12_regression.pdf
regLineaire <- function(x,y){
    n <- length(y)
    x_b <-  sum(x)/n
    y_b <- sum(log(y))/n
    a <- sum(x*log(y))-n*x_b*y_b
    b <- sum(x^2)-n*x_b^2
    t1 <- a/b
    t0 <- y_b-t1*x_b
    c(t0,t1)
}
(hyp1 <- gradient(x,y,1,0.1,10000))
(hyp2 <- monAlgo(x,y))

curve(dexp(x,hyp1),add = T,col = "red")
curve(dexp(x,hyp2),add = T, col = "green")


t <-  regLineaire(x,y)
hyp3 <- exp(t[1])
hyp4 <- -t[2]
data.frame(hyp1,hyp2,hyp3,hyp4)

curve(dexp(x,hyp3),0,100, col = "blue")
curve(dexp(x,hyp4),0,100, col = "red" , add = T)
curve(dexp(x,hyp2),0,100, col = "green" , add = T)
curve(dexp(x,hyp1),0,100, col = "yellow" , add = T)


