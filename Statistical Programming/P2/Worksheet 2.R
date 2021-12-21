#Q1
f1 <- function(x){
  list(mean(x), median(x), var(x), range(x))
}

f1(c(3,7,8,35,765,24,86,2,45,6))

sum <- 0
f2 <- function(x, n){
  for (i in 1:n){
  sum <- sum + exp(-x) * (x^i) * (1 / factorial(i)) 
  }
  print(sum)
}

# f2 <- function(x, n) {
  # sq <- seq(from = 0, to = n)
  # sum(exp(-x) * x^sq / factorial(sq)) # elementwise multiplication and division
# }


f3 <- function(list) {
  for (i in list) {
    if (is.character(i)) {
      print(i)
    }
  }
}

f3(list(2, "cat", 45, TRUE, "dog"))

f4 <- function(k) {
  walk <- 0
  sum <- 0
  while (sum < k && sum > -k) {
  sum <- sum + sample(c(1, -1), 1, prob = c(0.5, 0.5))
  walk <- c(walk, sum)
  }
  walk
}

f4(8)


#Q2
hares <- read.table("desktop/Statistical Programming/hares.txt", header = TRUE)
plot(hares[, "hare"], type = "l", lwd = 2, col = "#E69F00", xlab = "Year", ylab = "Pelt number (in thousands)", main = "Pelts recovered by Hudson's Bay Company", axes = FALSE, ylim = c(0, 200))
lines(hares[, "lynx"], type = "l", lwd = 2, col = "#56B4E9")
axis(1, at = 1:47, lab = hares[, "year"])
axis(2)
legend("topright", c("Hare", "Lynx"), lwd = c(2, 2), col = c("#E69F00", "#56B4E9"))


#Q3
library(MASS)
data(beav1)
head(beav1$temp)

ma3 <- function(x, n = 3){
  cs <- c(0,cumsum(x))
  (cs[(n + 1):length(cs)] - cs[1:(length(cs) - n)]) / n
}
# ma3 <- function(x, n = 3){stats::filter(x, rep(1/n, n), sides = 2)}
ma3(beav1$temp, 3)

plot(beav1[, "temp"], type = "l", ylab = "Temperature", col = "#E69F00")
points(2:(n - 1), ma3(beav1[, "temp"], 3), type = "l", col = "#56B4E9")


mak <- function(x, k){
  n <- length(x)
  ma <- rep(0, n - (k - 1))
  for (i in 0:(k - 1)){
    ma <- ma + x[((1 + i):(n - (k - 1) + i))] / k
  }
  ma 
}

plotMA <- function (x, k){
  cbPalette <- c(
    "#999999", "#E69F00", "#56B4E9", "#009E73", 
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
  )
  plot(x, type = "l", lwd = 2, ylab = "Temperature", col = cbPalette[i], main = "Moving Averages")
  k <- k[k != 1]
  m <- length(k)
  for (i in 1:m){
  points(seq(from = (k[i] - 1) / 2, length = length(mak(x, k[i])), by = 1), mak(x, k[i]), type = "l", lwd = 2, col = cbPalette[i + 1])
  legend("topleft", c("x", paste("MA-", k)), lwd = c(2, 2), col = cbPalette, cex = 0.75)
  } 
}

plotMA(beav1[, "temp"], k = c(5, 10))


#Q4
phi <- function(x){
  x >= 3 & x <= 5
}
set.seed(123)
x <- rcauchy(10)
cbind(x, phi(x))

# count the number of elements in x in [3, 5]
Monte_Carlo_CA <- function(n){
  x <- rcauchy(n)
  cumsum(phi(x)) / (1:n)
}

Importance_Sampling_CA <- function(n){
  y <- runif(n, min = 3, max = 5)
  w <- 2 * dcauchy(y)
  cumsum(phi(y) * w) / (1:n)
}

set.seed(99)
plot(Monte_Carlo_CA(100), type = "l", lwd = 2, col = "#E69F00", xlab = "Number of data points", ylab = "Running Averages", main = "Monte Carlo and Importance Sampling Running Averages")
lines(Importance_Sampling_CA(100), type = "l", lwd = 2, col = "#56B4E9")
abline(1 / pi * (atan(5) - atan(3)), 0, lwd = 2)
legend("topright", c("Monte Carlo", "Importance Sampling", "Truth"), lwd = c(2, 2, 2), col = c("#E69F00", "#56B4E9", "black"))

# the importance sampler looks better
# it converges to a value closer to the truth faster

plot_CA <- function(MC, IS){
  cbPalette <- c(
    "#999999", "#E69F00", "#56B4E9", "#009E73", 
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
  )
  plot(MC, type = "l", lwd = 2, col = cbPalette[2], xlab = "Number of data points", ylab = "Running Averages", main = "Monte Carlo and Importance Sampling Running Averages")
  lines(IS, type = "l", lwd = 2, col = cbPalette[3])
  abline(1 / pi * (atan(5) - atan(3)), 0, lwd = 2, col = cbPalette[4])
  legend("topright", c("Monte Carlo", "Importance Sampling", "Truth"), lwd = c(2, 2, 2), col = cbPalette[2:4])
}

simulation_and_plot <- function(n){
  plot_CA(Monte_Carlo_CA(100), Importance_Sampling_CA(100))
}

par(mfrow = c(1, 2))
simulation_and_plot(100)
simulation_and_plot(200)

