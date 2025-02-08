##  author: Patrick Addona
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

set.seed(123)  # for reproducibility

N <- 1e6  # 1,000,000 monte carlo samples
X_prior <- runif(N, min = 0, max = 182)
likelihood <- dnorm(34, mean = X_prior, sd = 20)

weights_unnorm <- likelihood * 1/182 # multiply by constant 1/182 since prior is uniform
weights_norm <- weights_unnorm / sum(weights_unnorm) # normalize weights

#importance sampling using normalized weights
idx <- sample(seq_len(N), size = N, replace = TRUE, prob = weights_norm)
X_posterior <- X_prior[idx]

fuel_rate <- rnorm(N, mean = 18, sd = 2) # fuel rate pdf

time   <- 60 * X_posterior / fuel_rate # flight time in minutes

plot(density(time),
     xlab = "Flight time (minutes)",
     main = "Distribution of Available Flight Time",
     lwd  = 2, col  = "blue")

# For reference, add vertical lines for 100 min and 130 min
abline(v = 100, col = "red", lty = 2)
abline(v = 130, col = "black", lty = 2)

legend("topright",
       legend=c("Flight time density","100 min","130 min"),
       col=c("blue","red","black"), lty=c(1,2,2), lwd=2)


# a) probability of having enough flight time to go 100 min + 30 min reserve: 
# P(time >= 130)
p_enough_reserve <- mean(time >= 130) #calculates logical array and finds probability
#by calculating mean

# b) Probability that you run out of fuel trying to make it to an airport 
#    that is 100 min away: P(time < 100)
p_run_out <- mean(time < 100)

cat("Probability( time >= 130 ) =", p_enough_reserve, "\n")
cat("Probability( time < 100 )   =", p_run_out, "\n")
