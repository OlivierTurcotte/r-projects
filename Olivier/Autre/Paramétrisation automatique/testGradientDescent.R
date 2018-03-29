# TEST

source("gradientDescent.R")

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
t <- gradientDescent(x,y,c(1),0.1,2000,"t1*exp(-t1*x)")
curve(dexp(x,t),col = "blue", add = T)

hist(x, prob =T)
curve(dexp(x,t),col = "blue" , add =T)

## Test concluant.
## /!\ Mon algorithme ne fonctionne pas sur les densité ayant une asymptote lorsque
## /!\ dérivé. En effet, ça explose et on obtient l'infini