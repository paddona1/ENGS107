##  author: Patrick Addona
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

set.seed(1)  # for reproducibility

# initial settings
n_iter <- 10000        # total iterations
sigma    <- 30            # proposal standard deviation
y      <- 34           # observed sensor reading
n_accept <- 0          # initialize to later calcuate acceptance prob

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
  dnorm(y, mean = x, sd = 20, log = FALSE)
}

# initialize the chain
X <- numeric(n_iter)
X[1] <- 300  # choose value outside prior range to demonstrate convergence

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
cat("Acceptance rate:", accept_rate, "\n")

# check the mode convergence
dens <- density(X_post)
mode_est <- dens$x[which.max(dens$y)]
cat("Posterior Mode estimate:", mode_est, "\n")

# plot the chain and the posterior density
par(mfrow=c(2,1))

# trace plot of chain values
plot(X_post, type = "l", col="blue",
     main="Fuel Level (MH Sampler)",
     xlab="Iteration", ylab="Fuel (liters)")

# posterior density estimate (after burn-in)
plot(dens, main="Posterior Distribution of Fuel",
     xlab="Fuel (liters)", col="blue", lwd=2)
abline(v=mode_est, col="red", lty=2, lwd=2)

legend("topright", legend=c("Posterior Density","Mode"), 
       col=c("blue","red"), lty=c(1,2), lwd=2)
