
# 1.    Calculer cinq nombres pseudo-aléatoires avec chacun des générateurs
#       congruentiels ci-dessous. Dans tous les cas, m𝑚 = 64. Choisir l’amorce.
#       a) a = 29, c = 17
#       b) 𝑎 = 9, 𝑐 = 1
#       c) 𝑎 = 13, 𝑐 = 0
#       d) 𝑎 = 11, 𝑐 = 0
prng <- function(n,a,c,m,seed) {
    x <- numeric(n+1) # Initialisation du vecteur
    x[1] <- seed
    for (i in seq(n)){
        x[i+1] <- (a * x[i] + c) %% m
    }
    x[-1]
}
prng(5,29,17,64,19)
#...

# 2.
x <- prng(500,17,0,2^13-1,19)
y <- x[-1]
x <- x[-length(x)]
plot(x,y)

x <- prng(500,85,0,2^13-1,19)
y <- x[-1]
x <- x[-length(x)]
plot(x,y)

# 3.

values <- prng(20002,65539,0,2^31,19)*2^-31
matrice <- matrix(c(values[1:20000],values[2:20001],values[3:20002]),ncol = 3)
matrice <- matrice[matrice[,2] >= 0.5 & matrice[,2] <= 0.51,c(1,3)]

plot(matrice[,1],matrice[,2])

library(rgl)
plot3d(matrice[,1],matrice[,2],matrice[,3])

