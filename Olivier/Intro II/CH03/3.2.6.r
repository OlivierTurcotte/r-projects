# 3.2.6
rm(list=ls())
m <- 1e6
set.seed(20160419)
v <- matrix(runif(2*m),ncol = 2,byrow = T)
sigma <- qlnorm(v[,1],-0.5,1)
poisson <- qpois(v[,2],2*sigma)
mean(poisson)
mean(poisson^2)-mean(poisson)^2
