# fit a piecewise Poisson regression model 
# with autocorrelated errors 

fit_brandt_poisson_f <- function(y, t) {
  require(runjags)
  T <- length(y)
  P <- 2
  beta1 <- mean(y[1:t], na.rm = T)
  beta2 <- mean(y[(t + 1):T], na.rm = T)
  
  model <- "model {
    x[1] <- 0
    lambda[1] <- exp(mu[1])
    y[1] ~ dpois(lambda[1])
    
    for (i in 2:T) {
      dummy[i] <- step(t - i) # dummy == 1 if i in baseline phase, and dummy == 0 if i in intervention
      x[i] <- dummy[i] * mu[1] + (1 - dummy[i]) * mu[2] 
      lambda[i] <- ifelse(t == i + 1, exp(x[i]), exp(x[i]) + rho * (y[i - 1] - lambda[i - 1]))   
      y[i] ~ dpois(lambda[i])
    }
    mu[1] ~ dnorm(7.5, prec[1])
    mu[2] ~ dnorm(3, prec[2])
    for (i in 1:P){
      prec[i] ~ dgamma(1, 1)
      sigma[i] <- 1/sqrt(prec[i])
      #prec is the epsilon
    }

  rho ~ dunif(-1, 1)
 # muratio <- exp(mu[1]- mu[2])
#change to 2 - 1 for cases 7-10

}"

  results <- autorun.jags(
    model = model,
    data = list(y = y, T = T, P = P, t = t),
    monitor = c("rho",  "rho", "mu", "sigma", 
                # "muratio", 
                "prec"), 
    n.chains = 4,
    startsample = 50000,
    method = "rjparallel"
  )
  
  
  # combine all chains into a single chain for convenience
  results$draws <- combine.mcmc(results$mcmc)
  
  results
}
