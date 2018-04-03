rm(list=ls())
n <- 100000
S <- numeric(n)
M <- numeric(0)
set.seed(20180403)

for(i in seq_len(n))
{
    M[i] <- qpois(runif(1),10)
    if(M[i] > 0)
    {
        temp <- numeric(0)
        for(j in 1:M[i])
        {
            p <- runif(1)
            if( p < 1/10)
            {
                temp[j] <- sum(qlnorm(runif(1),2,0.9))
            }
            else if ( p >= 1/10 && p<4/10)
            {
                temp[j] <- sum(qweibull(runif(1),0.5,4))
            }
            else
            {
                temp[j] <- sum(qpareto(runif(1),4,15))
            }
        }
        S[i] <- sum(temp)
    }
}

## E[S]
exp(2+0.9^2/2)+3*4*gamma(1+1/0.5)+6*5
## E[S_simulé]
mean(S)

# Ainsi, nos simulations sont bonnes. Pour le reste, c'est exactement comme au 4.2.13
