##  author: Patrick Addona
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

rm(list = ls())
graphics.off()

#parameters
sample_sizes <- c(1:4)
n_simulations <- 10
n_seeds <- 9

#pre-allocate results
pi_estimates <- numeric(n_simulations)
pi_mean <- numeric(length(sample_sizes))
pi_uncertainty <- numeric(length(sample_sizes))

seed_colors <- rainbow(n_seeds)

#initialize graph
plot(NULL, xlim = c(1, length(sample_sizes)), ylim = c(3, 3.5),
     xlab = "Sample Size (log10 scale)", ylab = "Estimated π ± Uncertainty",
     main = "Monte Carlo Estimation of π with Uncertainty",
     xaxt = "n")
axis(1, at = 1:length(sample_sizes), labels = paste0("10^", sample_sizes))
#line showing expected value of pi
abline(h = pi, col = "gray", lty = 2)

set.seed(0)
#for each seed, for each sample size, run 10 simulations and calculate mean approximation of pi
for (k in 1:n_seeds) {
  for (j in sample_sizes) {
    for (i in 1:n_simulations) {
      #generate random points with coordinates from U[0,1]
      x <- runif(10^j, 0, 1)
      y <- runif(10^j, 0, 1)
      
      #count points inside the quarter circle
      inside_circle <- (x^2 + y^2) <= 1
      #ratio of points inside the circle to total number approximates π/4
      pi_estimates[i] <- 4 * sum(inside_circle) / (10^j)
    }

    pi_mean[j] <- mean(pi_estimates)
    pi_uncertainty[j] <- sd(pi_estimates)
  }
  
  points(1:length(sample_sizes), pi_mean, type = "b", col = seed_colors[k], pch = 19, lwd = 1.5)
  arrows(1:length(sample_sizes), pi_mean - pi_uncertainty,
         1:length(sample_sizes), pi_mean + pi_uncertainty,
         angle = 90, code = 3, length = 0.1, col = seed_colors[k])
  set.seed(k)
}
