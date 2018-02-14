rm(list=ls())
set.seed(2013)
M <- numeric(100000)
y <- numeric(100000)
for(i in 1:100000)
{
  M[i] <- qnbinom(runif(1),0.6,1/11)
  if(M[i]>0)
  {
    y[i] <- sum(qlnorm(runif(M[i]),2,0.9))
  }
}
mean(M)
mean(y)

mean(y^2)-mean(y)^2
sum(y>100)/100000
sum(y>200)/100000
sum(y>300)/100000
sum(y>400)/100000
sum(y>500)/100000

#e
kappa <- c(0.5,0.75,0.99,0.999,0.9999)
q <- sort(y)
(VaR <- q[100000*kappa])

#f

(TVaR <-  sapply(VaR, function(x) mean(q[q>x])))
    
  



  
                
  




