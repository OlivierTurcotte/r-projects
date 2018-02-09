
# Fonction générale de calcul de la stop-loss
# pour des densités discrète.
stopLoss <- function(d, density.exp,n){
    density <- substitute(density.exp)
    density$x <- (d+1):n
    sum(eval(density)*seq_len(n-d))
}

# Exemples
stopLoss(2,dbinom(4,0.5),4)
stopLoss(20,dnbinom(5,0.5),1000)

