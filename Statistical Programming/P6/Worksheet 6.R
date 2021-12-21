# Q1
library("parallel")

# fork
draws <- mclapply(1:100, mc.cores = 4, function(x) mean(rnorm(10000)))

# socket
cl <- makeCluster(4)
draws <- parSapply(cl, X = 1:100, FUN = function(x) mean(rnorm(10000)))
stopCluster(cl)

# fork
# mclapply(1:ncol(mtcars), mc.cores = 4, function(x) mean(mtcars[, x]))
unlist(mclapply(mtcars, mc.cores = 4, mean))

# socket
cl <- makeCluster(4)
# parSapply(cl, X = 1:ncol(mtcars), FUN = function(x) mean(mtcars[, x]))
parSapply(cl, mtcars, mean)
stopCluster(cl)



# Q2
autompg <- read.csv("autompg_clean.csv", row.names = 1, header = TRUE)
str(autompg)
training_samples <- sample(1:nrow(autompg), round(0.8 * nrow(autompg)), replace = FALSE)
train <- autompg[training_samples, ]
test <- autompg[-training_samples, ]

linear_regression <- function(){
  train_bootstrapped = train[sample(1:nrow(train), nrow(train), replace = TRUE), ]
  lm(mpg ~ . - car.name, data = train_bootstrapped)
}

# fork
fork_results <- mclapply(1:100, mc.cores = 4, function(x) linear_regression())

# socket
cl <- makeCluster(4)
# export the lm function and the train dataset
clusterExport(cl = cl, "linear_regression")
clusterExport(cl = cl, "train")
socket_results <- parLapply(cl, X = 1:100, function(x) linear_regression())
stopCluster(cl)

bagged_estimate <- function(){
  rowMeans(sapply(socket_results, function(x) predict(x, test)))
}
cor(bagged_estimate, test[, "mpg"])

normal_regression <- lm(mpg ~ .-car.name, data = train)
normal_estimate <- predict(normal_regression, test)
cor(normal_estimate, test[, "mpg"])



# Q3
sample_with_replacement <- function(x) {
  n <- length(x)
  x[ceiling(runif(n, 0, n))]
}

all_n <- seq(10 ** 2, 10 ** 5, length.out = 20)
running_times <- sapply(all_n, function(n){
        x <- runif(n)
        system.time(for (i in 1:10){
          sample_with_replacement(x)})["elapsed"] / 10
})

plot(all_n, running_times, xlab = "n", ylab = "Time elasped", type = "b")

# looks linear


bootstrap_mean_test <- function(x, y, B) {
  n <- length(x)
  m <- length(y)
  x_bar <- mean(x)
  x_sigma <- sd(x)
  y_bar <- mean(y)
  y_sigma <- sd(y)
  t <- (x_bar - y_bar) / sqrt(
    x_sigma ** 2 / n +
      y_sigma ** 2 / m
  )
  z_bar <- mean(c(x, y))
  x_1 <- x - x_bar + z_bar
  y_1 <- y - y_bar + z_bar
  
  ind <- 0
  for(i in 1:B) {
    x_star <- x_1[ceiling(runif(n, 0, n))]
    y_star <- y_1[ceiling(runif(m, 0, m))]
    stat <- (mean(x_star) - mean(y_star)) / sqrt(
      sd(x_star) ** 2 / n +
        sd(y_star) ** 2 / m
    )
    if (abs(stat) > abs(t)) {
      ind <- ind + 1
    }
  }
  p <- ind / B
  return(p)
}

# we can quickly verify this makes sense on some distributions

# expect to be the same
bootstrap_mean_test(runif(100), runif(100), B = 100)
bootstrap_mean_test(rnorm(100), rnorm(100), B = 100)

# expect to have a low p-value
bootstrap_mean_test(runif(100) + 10, runif(100), B = 100)
bootstrap_mean_test(runif(100), runif(100) + 10, B = 100)


all_n <- seq(10 ** 2, 10 ** 5, length.out = 20)
running_times <- sapply(all_n, function(n){
  x <- runif(n)
  y <- runif(100)
  B <- 100
  system.time(for (i in 1:10){
    bootstrap_mean_test(x, y, B)})["elapsed"] / 10
})

plot(all_n, running_times, xlab = "n", ylab = "Time elasped", type = "b")

# looks linear 
# can do largelly the same thing for B and m

