## Question 5

# A)
Fs <- function(s) 0.5*pgamma(s,1.5,1/100)+0.5*pgamma(s,4.5,1/100)
# B)
v <- uniroot(function(x) Fs(x)-0.9,c(0,10000))$root

# C) & D)
(0.5*1.5*100*(1-pgamma(v,1.5 + 1,1/100))+0.5*4.5*100*(1-pgamma(v,4.5 + 1, 1/100)))/0.1

