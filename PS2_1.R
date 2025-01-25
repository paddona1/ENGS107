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
means <- numeric(n_simulations)
percentiles_95 <- numeric(n_simulations)
mean_mean <- numeric(length(sample_sizes))
mean_percentile <- numeric(length(sample_sizes))
mean_uncertainty <- numeric(length(sample_sizes))
percentile_uncertainty <- numeric(length(sample_sizes))

seed_colors <- rainbow(n_seeds)

windows()
#initialize graph for mean
plot(NULL,xlim = c(1,length(sample_sizes)),ylim=c(-1,1),
     xlab = "Sample Size (log10 scale)",ylab = "Mean ?? Uncertainty",
     main = "Mean and Uncertainty for each seed",
     xaxt = "n")
axis(1,at=1:length(sample_sizes),labels = paste0("10^",sample_sizes))
#line showing expected value of mean
abline(h=0,col="gray",lty=2)

#set first seed
set.seed(0)

#for each seed, then for each sample size, run 10 simulations and calculate mean and 95th percentile
#store the standard deviation of mean and 95th percentile for each sample size
for (k in 1:n_seeds) {
  for (j in sample_sizes) {
    for (i in 1:n_simulations) {
      samples <- rnorm(10^j, mean = 0, sd = 1)
      
      means[i] <- mean(samples)
      percentiles_95[i] <- quantile(samples, 0.95)
    }
    
    mean_mean[j] = mean(means)
    mean_percentile[j] = mean(percentiles_95)
    mean_uncertainty[j] <- sd(means)
    percentile_uncertainty[j] <- sd(percentiles_95)
  }
  
  points(1:length(sample_sizes),mean_mean,type="b",col=seed_colors[k],pch=19,lwd=1.5)
  arrows(1:length(sample_sizes),mean_mean-mean_uncertainty,
         1:length(sample_sizes),mean_mean+mean_uncertainty,
         angle=90,code=3,length=0.1,col=seed_colors[k])
  set.seed(k)
}

windows()
# initialize plot for 95th percentile
plot(NULL, xlim = c(1, length(sample_sizes)), ylim = c(-1, 3),
     xlab = "Sample Size (log10 scale)", ylab = "95th Percentile ?? Uncertainty",
     main = "95th Percentile and Uncertainty for each seed",
     xaxt = "n")
axis(1, at = 1:length(sample_sizes), labels = paste0("10^", sample_sizes))
#line showing expected value of 95th percentile
abline(h = qnorm(0.95, mean = 0, sd = 1), col = "gray", lty = 2)

# Loop for calculating and plotting Percentiles
for (k in 1:n_seeds) {
  for (j in sample_sizes) {
    for (i in 1:n_simulations) {
      samples <- rnorm(10^j, mean = 0, sd = 1)
      percentiles_95[i] <- quantile(samples, 0.95)
    }
    
    mean_percentile[j] <- mean(percentiles_95)
    percentile_uncertainty[j] <- sd(percentiles_95)
  }
  
  points(1:length(sample_sizes), mean_percentile, type = "b", col = seed_colors[k], pch = 19, lwd = 1.5)
  arrows(1:length(sample_sizes), mean_percentile - percentile_uncertainty,
         1:length(sample_sizes), mean_percentile + percentile_uncertainty,
         angle = 90, code = 3, length = 0.1, col = seed_colors[k])
  set.seed(k)
}