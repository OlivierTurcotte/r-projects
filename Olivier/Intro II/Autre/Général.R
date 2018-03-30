# À savoir :

# Définir fm()

# Fx(x)
fm(0) + sum(sapply(seq(k0),function(i) fm(i) + F_sommeX(x) ))

# VaR : 
ifelse(k <= fm(0),0,uniroot(function(x) Fx(x)-k,c(a,b))$root)

# TVaR:
v <- VaR(k) ; sum(sapply(seq(k0),function(i) fm(i)* E_tronque(x) )) / (1-k)



# En simulation : 

# Création des données: 
n
set.seed()
M <- numeric(0)
X <- numeric(n)
for(i in seq_len(n)){
    M[i] <- qwhatever(runif(1),param)
    if(M[i] > 0) X[i] <- sum(qwhatever2(runif(M[i]),param))
}

# Fx :
ecdf(X)(u)

# Moyenne :
mean(X)

# Var :
var(X)

# Stop-Loss :
sum(X[X>d]-d)/n

# VaR / Quantile :
quantile(X,k,type = 1)

#TVaR : 
mean(X[X > VaR])

