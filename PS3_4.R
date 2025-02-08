##  author: Patrick Addona
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

set.seed(1)  # for reproducibility

N <- 1e6  # 1,000,000 monte carlo samples
X_prior <- runif(N, min = 0, max = 182)
likelihood <- dnorm(34, mean = X_prior, sd = 20)

weights_unnorm <- likelihood * 1/182 # multiply by constant 1/182 since prior is uniform
weights_norm <- weights_unnorm / sum(weights_unnorm) # normalize weights

#importance sampling using normalized weights
idx <- sample(seq_len(N), size = N, replace = TRUE, prob = weights_norm)
X_posterior <- X_prior[idx]

plot(density(X_posterior),
     main = "Bayes Monte Carlo Posterior: Fuel in Tank",
     xlab = "Fuel (liters)",
     lwd  = 2,
     col  = "blue")

set.seed(2)  # for reproducibility

N <- 1e6  # 1,000,000 monte carlo samples
X_prior <- runif(N, min = 0, max = 182)
likelihood <- dnorm(34, mean = X_prior, sd = 20)

weights_unnorm <- likelihood * 1/182 # multiply by constant 1/182 since prior is uniform
weights_norm <- weights_unnorm / sum(weights_unnorm) # normalize weights

idx <- sample(seq_len(N), size = N, replace = TRUE, prob = weights_norm)
X_posterior <- X_prior[idx]

lines(density(X_posterior),
      main = "Bayes Monte Carlo Posterior: Fuel in Tank",
      xlab = "Fuel (liters)",
      lwd  = 2,
      col  = "green")

set.seed(3)  # for reproducibility

N <- 1e6  # 1,000,000 monte carlo samples
X_prior <- runif(N, min = 0, max = 182)
likelihood <- dnorm(34, mean = X_prior, sd = 20)

weights_unnorm <- likelihood * 1/182 # multiply by constant 1/182 since prior is uniform
weights_norm <- weights_unnorm / sum(weights_unnorm) # normalize weights

idx <- sample(seq_len(N), size = N, replace = TRUE, prob = weights_norm)
X_posterior <- X_prior[idx]

lines(density(X_posterior),
      main = "Bayes Monte Carlo Posterior: Fuel in Tank",
      xlab = "Fuel (liters)",
      lwd  = 2,
      col  = "yellow")

curve(dnorm(x, mean=34, sd=20), from=-40, to=220,
      add=TRUE, col="red", lwd=2, lty=2)

legend("topright", 
       legend=c("Posterior (Monte Carlo 1)", "Posterior (Monte Carlo 2)", "Posterior (Monte Carlo 3)", "Naive Normal(34,20^2)"),
       col=c("blue", "green", "yellow", "red"), lty=c(1,2), lwd=2)