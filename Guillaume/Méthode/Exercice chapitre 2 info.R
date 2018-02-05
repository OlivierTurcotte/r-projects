###--------------- 2.1----------------

## a) à refaire
rm(list=ls())
u1 <- runif(200)
u2 <- runif(200)
x <- (-2*log(u1))^(1/2) * cos(2*pi*u2)
y <- (-2*log(u1))^(1/2) * sin(2*pi*u2)
plot(x,y)

## b)
#preuve faite sur papier

##c)

#seulement a prouver que leur densite sont separable

## d)
u1 <- runif(200)
u2 <- runif(200)
donne<-
hist(donne,prob=T)
curve(dnorm,add=T)
donne2 <- outer(u1,u2,function(x,y) (-2*log(x))^(1/2) * sin(2*pi*y))
hist(donne2,prob=T)
curve(dnorm,add=T)


### ----------------------numero 2--------------------------
## preuve faite sur papier

###-----------------------numero 3-------------------------

## preuve faite sur papier

###-----------------------numero 4------------------------- à refaire
rm(list=ls())

gamma_aleatoire <- function(n,shape,rate=1)
{
  i <- 0
  x <- numeric(n)
  while((i+1)<=n)
  {
  u1 <- runif(1)
  u2 <- runif(1)
  v <- (shape-(1/(6*shape)))*u1/((shape-1)*u2)
  if((2*(u2-1)/(shape-1) + v + 1/v) <= 2)
  {
    i <- i+1
    x[i] <- (shape-1)*v
  }
  else if(((2*log(u2)/(shape-1))-log(v)+v) <=1)
  {
    i <- i+1
    x[i] <- (shape-1)*v
  }
  }
  x
}
x <- gamma_aleatoire(10000,10)
hist(x,prob=T)
curve(dgamma(x,10),add=T)
###--------------------numero 5-----------------------------------------------
## Sur papier
###---------------------------------numero 6-------------------------------------
## a) Preuve sur papier
## b)
#x represente la taille de lechantillon
rm(list=ls())
library(actuar)

gen_pareto <- function(x,shape,rate) qexp(runif(x),rate=rgamma(x,shape,rate))

#Test de validite
y <- gen_pareto(1000,2,1/4)
y
hist(y,prob=T)
curve(dpareto(x,2,1/4),add=T)
#Ok

###-------------------Numero 7----------------------------------------
rm(list=ls())
random <- function(size,seed)
{
  x <- numeric(size+1)
  x[1] <- seed
  for (i in seq_len(size))
  {
    x[i+1]<- (65*x[i]+1)%% 2048
  }
  x[-1]
}
x <- random(3,12)/2048
gen_pareto <- function(u,shape,rate)
{
  (rate/(1-u)^(1/shape))
}
sapply(x,function(x) gen_pareto(x,shape=2,rate=1000))

###---------------------------Numero 8--------------------------------------------
## preuve sur papier 


###---------------------------Numero 9--------------------------------------------
##
#a) preuve faite sur papier
#b)
#c)
rm(list=ls())
x <- rgamma(10000,3)
y <- rgamma(10000,5)
z <- x/(x+y)
hist(z,prob=T)
curve(dbeta(x,3,5),add=T)
###--------------------------Numero 10--------------------------------------------

## Fait sur papier


###--------------------------Numero 11----------------------------------------------
## fait sur papier


###---------------------------Numero 12--------------------------------------------
rm(list=ls())
## a) sur papier
##b) sur papier
##c)

g <- function(y)
{
  if((y>= 0)&&(y<0.8))
  {
    return(2.5*y)
  }
  if((y>= 0.8)&&(y<1))
    return(10-10*y)
  else
    warning("hors du range")
}
ginv <- function(x)
{
  if((x>=0)&&(x<=0.8))
    sqrt(0.8*x)
  else
    1-sqrt(0.2-0.2*x)
}

gen_beta <- function(n)
{
 x <- numeric(n)
 i <- 0
 while(i+1 <= n)
 {
   u1 <- runif(1)
   u2 <- runif(1)
   y <- ginv(u1)
   if(u2 <= (dbeta(u1,3,2)/(1.2*g(y))))
     i <- i+1
   x[i] <- y
 }
 x
}
x <- gen_beta(100000)
hist(x,prob=T)
curve(dbeta(x,3,2),add=T)
## pas tout á fait

###---------------------------Numero 13-------------------------------------------

## Fait sur papier


###----------------------------Numero 14------------------------------------------
##


###------------------------------Numero 15-----------------------------------------



###-----------------------------Numero 16-----------------------------------------
##ok 
library(actuar)
?rmixture


###-----------------------------Numero 17-----------------------------------------

#a)
modele_1 <- function(n)
{
  n1 <- rbinom(n,0.3)


}
?rbinom

optimize(function(x) abs(0.8*pexp(x,1/1000) + 0.2*pexp(x,1/6000) - 0.995),c(0,100000),maximum=F)
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  


















