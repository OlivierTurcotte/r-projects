
# Fonction générale de calcul de la stop-loss
# pour des densités discrète.
stopLoss <- function(d, density.exp){
    density <- substitute(density.exp)
    # Idée, sommer test jusqu'à ce que ce soit égale à 1, puis prendre le nombre d'éléments
    n <- 0
    density$x <- n
    s <- eval(density)
    while(s < 1){
        n <- n+1 
        density$x <- n
        s <- s + eval(density)
    }
    density$x <- (d+1):n
    sum(eval(density)*seq_len(n-d))
}

# Exemples
stopLoss(2,dbinom(4,0.5))
stopLoss(20,dnbinom(5,0.5))

