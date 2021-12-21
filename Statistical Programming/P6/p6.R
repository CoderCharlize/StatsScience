##
## Q1
##
library("parallel")

fork_results <- unlist(mclapply(1:100, mc.cores = 2, function(x) {
    mean(rnorm(10000))
}))

cl <- makeCluster(2)
socket_results <- parSapply(
    cl = cl, X = 1:100, FUN = function(x) {
        mean(rnorm(10000))
    }
)
stopCluster(cl)


## fork
fork_results <- unlist(mclapply(mtcars, mc.cores = 2, mean))

## sockets
cl <- makeCluster(2)
socket_results <- parSapply(cl = cl, X = mtcars, FUN = mean)
stopCluster(cl)







##
## Q2
##
autompg <- read.csv("autompg_clean.csv", header = TRUE)
training_samples <- sample(
    1:nrow(autompg), round(nrow(autompg) * 0.80), replace = FALSE
)
train <- autompg[training_samples , ]
test <- autompg[-training_samples, ]

one_linear_regression <- function() {
    train_bootstrapped <- train[
        sample(1:nrow(train), nrow(train), replace = TRUE), 
        ]
    lm(
        mpg ~ cylinders + displacement + horsepower + weight + 
            acceleration + model.year + origin, 
        data = train_bootstrapped
    )
}

fork_results <- mclapply(1:100, mc.cores = 2, function(x) {
    one_linear_regression()
})

## sockets
cl <- makeCluster(2)
clusterExport(cl = cl, "one_linear_regression")
clusterExport(cl = cl, "train")
socket_results <- parLapply(cl = cl, X = 1:100, fun = function(x) {
    one_linear_regression()
})
stopCluster(cl)

bagged_prediction <- rowMeans(
    sapply(fork_results, function(x) {
        predict(x, test)
    })
)

cor(bagged_prediction, test[, "mpg"])

normal_model <- lm(
    mpg ~ cylinders + displacement + horsepower + weight + 
        acceleration + model.year + origin, 
    data = train
)

normal_prediction <- predict(normal_model, test)

cor(normal_prediction, test[, "mpg"])







##
## Q3
##
sample_with_replacement <- function(x) {
    n <- length(x)
    x[ceiling(runif(n, 0, n))]
}


## for equal log spacing
ns <- 10 ** seq(5, 7, length.out = 10)

## for equal normal spacing
ns <- seq(10 ** 5, 10 ** 7, length.out = 10)

nReps <- 5
results <- t(sapply(ns, function(n) {
    x <- runif(n)
    return(
        c(
            n,
            system.time({
                for(i in 1:nReps) {
                    y <- sample_with_replacement(x)
                }
            })["elapsed"] / nReps
        )
    )
}))
plot(
    x = results[, 1], 
    y = results[, 2], 
    xlab = "n", 
    ylab = "Time elapsed (s)", 
    type = "b", 
    col = "blue", 
    lwd = 2
)
## looks linear 


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

## we can quickly verify this makes sense on some distributions

## expect to be the same
bootstrap_mean_test(runif(100), runif(100), B = 100)
bootstrap_mean_test(rnorm(100), rnorm(100), B = 100)

## expect to have a low p-value
bootstrap_mean_test(runif(100) + 10, runif(100), B = 100)
bootstrap_mean_test(runif(100), runif(100) + 10, B = 100)
bootstrap_mean_test(runif(100), runif(100) - 10, B = 100)
bootstrap_mean_test(runif(100) - 10, runif(100), B = 100)


xs <- seq(10**3, 10**5, length.out = 12)
results <- t(sapply(xs, function(n) {
    x <- runif(n)
    y <- runif(1000)
    B <- 100
    nReps <- 3 
    return(c(
        length(x),
        system.time(
            for(i in 1:nReps) {
                bootstrap_mean_test(x, y, B)
            }
        )["elapsed"] / nReps
    ))
}))

plot(
    x = results[, 1],
    y = results[, 2],
    xlab = "n", 
    ylab = "Time elapsed (s)", 
    type = "b", 
    col = "blue", 
    lwd = 2
)
## looks linear 
## can do largelly the same thing for B and m
