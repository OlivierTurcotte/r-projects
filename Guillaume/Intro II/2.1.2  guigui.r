###### Exercice 2.1.2 Intro II 

###--------------------------Numero 1 -------------------------------------------
rm(list=ls())
##a) sur papier
##b) sur papier
##c)
curve(dgamma(x,1,1/20),0,100,ylim=c(0,0.1),col="blue")
curve(dgamma(x,2,2/20),0,100,add=T)
curve(dgamma(x,5,5/20),0,100,add=T)
curve(dgamma(x,10,10/20),0,100,add=T)
curve(dgamma(x,20,20/20),0,100,add=T)

##d)
curve(pgamma(x,1,1/20),0,100,ylim=c(0,1),col="blue")
curve(pgamma(x,2,2/20),0,100,add=T)
curve(pgamma(x,5,5/20),0,100,add=T)
curve(pgamma(x,10,10/20),0,100,add=T)
curve(pgamma(x,20,20/20),0,100,add=T)
curve(pgamma(x,100,100/20),0,100,add=T)
## e) 
curve(qgamma(x,1,1/20),0,1,col="blue")
curve(qgamma(x,2,2/20),0,1,add=T)
curve(qgamma(x,5,5/20),0,1,add=T)
curve(qgamma(x,10,10/20),0,1,add=T)
curve(qgamma(x,20,20/20),0,1,add=T)
curve(qgamma(x,100,100/20),0,1,add=T)
## f)
TVaRsn <- function(x,n)
{
  (1/(1-x))* 20 * (1-pgamma(qgamma(x,n,n/20),n+1,n/20))
}

plot(function(x) TVaRsn(x,1),col="green")
plot(function(x) TVaRsn(x,2),add=T)
plot(function(x) TVaRsn(x,5),add=T)
plot(function(x) TVaRsn(x,10),add=T)
plot(function(x) TVaRsn(x,20),add=T)
plot(function(x) TVaRsn(x,100),add=T)

##g)
BME <- function(x,n)
{
  n*qexp(x,1/20) - qgamma(x,n,1/20)
}

plot(function(x) BME(x,2),ylim=c(0,2000),col="green",main="Bénéfice de mutualisation")
plot(function(x) BME(x,10),add=T)
plot(function(x) BME(x,100),add=T)

## h) 
TVaRexp <- function(x)
{
  qexp(x,1/20) + 20
}

BMK <- function(x,n)
{ 
  n*TVaRexp(x) + TVaRsn(x,n)
}

plot(function(x) BMK(x,2),ylim=c(0,5000),col="green",main="bénéfice de mutualisation")
plot(function(x) BMK(x,10),add=T)
plot(function(x) BMK(x,100),add=T)


###--------------------------Numero 2---------------------------------------
rm(list=ls())
## a) 

# x1
b <- 1/2000000
VaRx1 <- qgamma(0.995,1,b)

#x2

VaRx2 <- qgamma(0.995,2,b)
VaRx1 + VaRx2

#totaux
VaRs <- qgamma(0.995,3,b)
## b)

#x1
TVaR <- function(x,shape,rate)
{
  (1/(0.005))* shape/rate*(1-pgamma(x,shape+1,rate))
}
TVaRx1 <- TVaR(VaRx1,1,b)
TVaRx2 <- TVaR(VaRx2,2,b)
TVaRs <- TVaR(VaRs,3,b)

###--------------------------Numero 3------------------------------------

## S suit une  1000*bin(n=400,p=0.008)

#a)
rm(list=ls())
kappa <- c(0.9,0.95,0.99,0.995)
VaRS <- 1000*qbinom(kappa,size=400,0.008)

VaRS - 1000*400*0.008

##b)

VaRS/1000
n <- 400
p <- 0.008
x1 <- 7:n - 6
x2 <- 7:n - 6
x3 <- 9:n - 8 
x4 <- 10:n -9
y1 <- dbinom(7:400,n,p)
y2 <- dbinom(7:400,n,p)
y3 <- dbinom(9:400,n,p)
y4 <- dbinom(10:400,n,p)
stop_loss1 <- sum(x1*y1)
stop_loss2 <- sum(x2*y2)
stop_loss3 <- sum(x3*y3)
stop_loss4 <- sum(x4*y4)

TVaR1 <- ((1/0.1)*stop_loss1 + 6)*1000
TVaR2 <- ((1/0.05)*stop_loss2 + 6)*1000
TVaR3 <- ((1/0.01)*stop_loss3 + 8)*1000
TVaR4 <- ((1/0.005)*stop_loss4  + 9)*1000
TVaRS <- c(TVaR1,TVaR2,TVaR3,TVaR4)
TVaR1 - 1000*400*p
TVaR2- 1000*400*p
TVaR3 - 1000*400*p
TVaR4 - 1000*400*p

## c)



BM <- 400*c(0,0,0,1000) - VaRS
BM
## avantageux seulement pour k = 0.995

## d)

TVaRX1 <-1000* (1/0.1)*0.008
TVaRX2 <- 1000*(1/0.05)*0.008
TVaRX3 <- 1000*(1/0.01)*0.008
TVaRX4 <- 1000

400*c(TVaRX1,TVaRX2,TVaRX3,TVaRX4) -TVaRS

###--------------------------Numero 4 a corriger---------------------------------------------
rm(list=ls())

##hypothese A
m1 <- c(1/5,1/5,1/5,1/5,1/5)
xm1 <- c(1,2,3,4,5)
m2 <- c(1/5,1/5,1/5,1/5,1/5)
xm2 <- c(5,1,2,3,4)
#a)
#Marginale de x1 unif(1,5), marginale de x2 unif(1,5)
#b)
es_x1 <-sum(m1*xm1)
es_x2 <- sum(m2*xm2)
es_x1x2 <- sum(xm1*xm2*m1)
es_x1x2-es_x1*es_x2
#donc independant

#c) S=m1+m2
(xS <- xm1+xm2)
data.frame("Probs"=c(0.2,0.4,0.6,0.8,1),"value"=sort(xS))
#d)
rm(list=ls())
es_x1 <- 15*20
es_x2 <- 15*20
#i
(1/5)*(100*500+200*100+300*200+400*300+500*400)-es_x1*es_x2
#ii
#x1 suit x2 <- ga(15,1/20)
#iii
#x1+x2 <- ga(30,1/20)
pgamma(1000,30,1/20)
qgamma(0.99,30,1/20)

### hypothese B
rm(list=ls())

m1 <- c(1/5,1/5,1/5,1/5,1/5)
xm1 <- c(1,2,3,4,5)
m2 <- c(1/5,1/5,1/5,1/5,1/5)
xm2 <- c(4,3,2,1,5)
#a)
#Marginale de x1 unif(1,5), marginale de x2 unif(1,5)
#b)
es_x1 <-sum(m1*xm1)
es_x2 <- sum(m2*xm2)
es_x1x2 <- sum(xm1*xm2*m1)
es_x1x2-es_x1*es_x2
#donc independant

#c) S=m1+m2
(xS <- xm1+xm2)
data.frame("Probs"=c(0.8,1),"value"=c(5,10))
#d)
rm(list=ls())
es_x1 <- 15*20
es_x2 <- 15*20
#i
(1/5)*(100*400+200*300+300*200+400*100+500*500)-es_x1*es_x2
#ii
#x1 suit x2 <- ga(15,1/20)
#iii
#x1+x2 <- ga(30,1/20)
pgamma(1000,30,1/20)
qgamma(0.99,30,1/20)

##hypothese C
rm(list=ls())

m1 <- c(1/10,1/10,1/10,1/10,1/10,1/10,1/10,1/10,1/10,1/10)
xm1 <- c(1,2,3,4,5,1,2,3,4,5)
xm2 <- c(5,1,2,3,4,4,3,2,1,5)
#a)
#Marginale de x1 unif(1,5), marginale de x2 unif(1,5)
#b)
es_x1 <-sum(m1*xm1)
es_x2 <- sum(m1*xm2)
es_x1x2 <- sum(xm1*xm2*m1)
es_x1x2-es_x1*es_x2
#donc independant

#c) S=m1+m2
(xS <- xm1+xm2)
sort(xS)
data.frame("Probs"=c(1/10,6/10,7/10,8/10,9/10,1),"value"=c(3,5,6,7,9,10))
#d)
rm(list=ls())
es_x1 <-30*10
es_x2 <- 30*10
#i
(1/10)*(100*500+200*100+300*200+400*300+500*400+100*400+200*300+300*200+400*100+500*500)-es_x1*es_x2
#ii
#x1 suit x2 <- ga(30,1/10)
#iii
#x1+x2 <- ga(60,1/20)
pgamma(1000,60,1/10)
qgamma(0.99,60,1/10)

























###--------------------------Numero 5 a corriger----------------------------------
rm(list=ls())
set.seed(20170206)
n1 <- runif(1e1)
n2 <- runif(1e2)
n4 <- runif(1e4)
n8 <- runif(1e8)
mean(n1)
mean(n2)
mean(n4)
mean(n8)

###--------------------------Numero 6 a corriger------------------------------------
rm(list=ls())
set.seed(20170206)
n1 <- runif(1e1)
n2 <- runif(1e2)
n4 <- runif(1e4)
n8 <- runif(1e8)
exp1 <- -log(1-n1)
exp2 <- -log(1-n2)
exp4 <- -log(1-n4)
exp8 <- -log(1-n8)
mean(exp1)
mean(exp2)
mean(exp4)
mean(exp8)


###--------------------------Numero 7 a corriger----------------------------------
rm(list=ls())
set.seed(20170206)
m1 <- 1e1 ; m2 <- 1e2; m4 <- 1e4; m8 <- 1e8
n1 <- runif(m1)
n2 <- runif(m2)
n4 <- runif(m4)
n8 <- runif(m8)
exp1 <- sort(-log(1-n1))
exp2 <- sort(-log(1-n2))
exp4 <- sort(-log(1-n4))
exp8 <- sort(-log(1-n8))
VaR <- -log(0.5)
VaR1 <- exp1[0.5*m1]
VaR2 <- exp2[0.5*m2]
VaR4 <- exp4[0.5*m4]
VaR8 <- exp8[0.5*m8]
sum(exp1>VaR)/m1
sum(exp2>VaR)/m2
sum(exp4>VaR)/m4
sum(exp8>VaR)/m8
###--------------------------Numero 8 a corriger----------------------------------
## x suit un LN(9,1)
rm(list=ls())
set.seed(343463463)
m <- 100000
n <- 5
kappa <- c(0.001,0.01,0.99,0.999)
z <- matrix(runif(m*n),ncol=n,byrow=T)
x1 <- qlnorm(z[,1],9,1)
x2 <- qlnorm(z[,2],9,1)
x3 <- qlnorm(z[,3],9,1)
s <- x1+x2+x3
s_sort <- sort(s)
(VaRS <- s_sort[kappa*m])
(TVaRS <- sapply(VaRS,function(x) mean(s_sort[s_sort>x])))









###--------------------------Numero 9 a corriger----------------------------------
rm(list=ls())
set.seed(343463463)
m <- 100000
z <- matrix(runif(2*m),ncol=2,byrow=T)
x1 <- qexp(z[,1],1/100)
x2 <- qexp(z[,2],1/200)
kappa <- c(0.001,0.01,0.05,0.5,0.95,0.99,0.999)
mean(x1)
mean(x2)
s <- sort(x1+x2)
t <- sort(x1-x2)
mean(s)
mean(t)
#a)
(VaRS <- s[kappa*m])
#b)
(VaRT <- t[kappa*m])
#c)
sapply(VaRS,function(x) mean(s[s>x]))
#d)
sapply(VaRT,function(x) mean(t[t>x]))



###--------------------------Numero 10 a corriger-----------------------------------
rm(list=ls())
random <- function(n)
{
  x <- numeric(n+1)
  x[1] <- 20150309
  i <- 1
  while(i<= (n))
  {
    x[i+1] <- (x[i]*41358)%%2147483647
    i <- i+1
  }
  x[-1]/2147483647
}
z <- matrix(random(2000),ncol=2,byrow=T)
x1 <- qexp(z[,1],1/10)
x2 <- qexp(z[,2],1/25)
#a)
c(x1[1],x2[1])
c(x1[2],x2[2])
#b)
s <- x1+x2
s[1]
s[2]
which.min(s)
s[496]
which.max(s)
s[747]
#c)
#i
sum(s<10)/1000
sum(s<50)/1000
sum(s<200)/1000
#ii
z1 <- s-10
z2 <- s-50
z3 <- s-200
sum(z1[z1>0])/1000 # vrai reponse theorique: 25.47747231
sum(z2[z2>0])/1000 # vrai reponse theorique: 5.594050488
sum(z3[z3>0])/1000 # vrai reponse theorique: 0.013977596









###--------------------------Numero 11 a corriger---------------------------------
rm(list=ls())
random <- function(n)
{
  x <- numeric(n+1)
  x[1] <- 20150309
  i <- 1
  while(i<= (n))
  {
    x[i+1] <- (x[i]*41358)%%2147483647
    i <- i+1
  }
  x[-1]/2147483647
}
z <- matrix(random(2000),ncol=2,byrow=T)
#a)
x1 <- qlnorm(z[,1],log(200)-0.32,0.8)
x2 <- qlnorm(z[,2],2,1/100)
x1[c(1,2)]
x2[c(1,2)]
#b)
s <- x1+x2
s[c(1,2)]
s[which.min(s)]
s[which.max(s)]
#c)
#i
sum(s<=10)/1000
sum(s<=50)/1000
sum(s<=200)/1000

#ii
s1 <- s-10
s2 <- s-50
s3 <- s-200

sum(s1[s1>0])/1000
sum(s2[s2>0])/1000
sum(s3[s3>0])/1000

##ENFIN FINI PUTAIN
### CEST FINI PUTAIN