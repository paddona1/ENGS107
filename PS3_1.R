##  author: Patrick Addona
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

x_naive  <- seq(-40, 220, length.out = 1001)
naive_pdf <- dnorm(x_naive, mean = 34, sd = 20)

plot(x_naive, naive_pdf,
     type = "l", lwd = 2, col = "red",
     xlab = "Fuel (liters)",
     ylab = "Density",
     main = "Naive Normal(34, 20^2) ignoring physical constraints")