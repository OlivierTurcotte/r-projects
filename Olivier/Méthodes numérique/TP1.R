# Équipe 24 :  Michael Lavigne, Vincent Bergeron, Guilllaume Michel, Olivier Turcotte

library(actuar)
lambda <- c(0.003, 0.05, 0.08)
mu1 <- 7
sd1 <- 1
a1 <- 4
l1 <- 4500
a2 <- 2.5
l2 <- 0.01
lambda_p <- sum(lambda)
primePure  <- lambda[1] * exp(mu1 + sd1 ^ 2 / 2)  +
    lambda[2] * l1 / (a1 - 1) + lambda[3] * a2 / l2
prob <- lambda / lambda_p

s       <- rcomppois(1e6, lambda_p, rmixture(
    probs = prob,
    models =
        expression(rlnorm(mu1, sd1),
                   rpareto(a1, l1),
                   rgamma(a2, l2))
))
VaR     <- quantile(s, 0.9)
TVaR    <- mean(s[s > VaR])
data.frame("Prime pure théorique" = primePure,"Prime pure simulé" = mean(s), "Prime sécurité" = TVaR)
