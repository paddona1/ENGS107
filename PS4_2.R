##  author: Patrick Addona
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

set.seed(1)  # for reproducibility

true_fuel <- 40
n_reps <- 50
# generate a single noisy sensor reading from N(50, 20^2)
y_obs <- rnorm(1, mean = true_fuel, sd = 20)
n_iter <- 10000
sigma <- 5
n_accept <- 0          # initialize to later calcuate acceptance prob
posterior_means <- numeric(n_reps)

# prior: Uniform(0, 182)
prior <- function(x) {
  if (x >= 0 && x <= 182) {
    return(1/182)
  } else {
    return(0)
  }
}

# likelihood: N(y=34 | X, sd=20)
likelihood <- function(x) {
  dnorm(y_obs, mean = x, sd = 20, log = FALSE)
}

for (rep in 1:n_reps){
  y_obs <- rnorm(1, mean = true_fuel, sd = 20)
  X <- numeric(n_iter)
  X[1] <- 30
  
  # begin Metropolis-Hastings
  for (t in 1:(n_iter-1)) {
    # current state
    x_curr <- X[t]
    
    # propose x_star ~ N(x_curr, sigma^2)
    x_star <- rnorm(1, mean = x_curr, sd = sigma)
    
    # compute acceptance ratio
    p_star <- prior(x_star) * likelihood(x_star)
    p_curr <- prior(x_curr) * likelihood(x_curr)
    
    # ratio, set alpha to 1 if p_curr = 0 to avoid division by 0 and encourage movement towards high-density regions
    # alpha simplifies to p_star / p_curr because the likelihood is Gaussian
    alpha <- min(1, p_star / p_curr)
    if (p_curr == 0) {
      alpha = 1
    }
    
    # accept/reject
    if (runif(1) < alpha) {
      X[t+1] <- x_star
      n_accept <- n_accept + 1
    } else {
      X[t+1] <- x_curr
    }
  }
  
  # remove burn-in
  burn_in <- 0 #set to 0 to show convergence
  X_post  <- X[(burn_in+1):n_iter]
  
  accept_rate <- n_accept / (n_iter - 1)
  
  posterior_means[rep] <- mean(X_post)
}

cat("Mean of posterior means =", mean(posterior_means), "\n")

# trace plot of chain values
plot(posterior_means, type = "l", col="blue",
     main="Mean Fuel Level from MH",
     xlab="Iteration", ylab="Fuel (liters)")