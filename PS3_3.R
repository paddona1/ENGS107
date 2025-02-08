##  author: Patrick Addona
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

x_naive  <- seq(-40, 220, length.out = 1001)
X_grid <- seq(0, 182, length.out = 1001) # 1001 points from 0 to 182
dx <- X_grid[2] - X_grid[1] # spacing between grid points

prior  <- rep(1/182, length(X_grid)) # uniform prior

likelihood <- dnorm(34, mean = X_grid, sd = 20) # likelihood function

posterior_unnorm <- prior * likelihood # before normalization
normalizing_const <- sum(posterior_unnorm) * dx
posterior <- posterior_unnorm / normalizing_const

# plot posterior vs. naive normal
plot(X_grid, posterior, type = "l", lwd = 2, col = "blue",
     xlab = "Fuel in tank (liters)", ylab = "Density",
     main = "Posterior with Uniform(0,182) Prior and Gaussian Likelihood")

# naive normal from before
lines(x_naive, naive_pdf, col = "red", lty = 2, lwd = 2)

legend("topright", legend = c("Posterior (bounded)", "Naive Normal"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)