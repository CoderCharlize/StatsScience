# Q1
seq(1, 21, by = 2)
seq(50, 11, by = -3)
2^seq(1, 10, by = 1)
matrix(seq(1, 16, by = 1), nrow = 4, ncol = 4)


# Q2
vector <- rep(1:4, 5)
sample(vector)
names(vector) <- rep(c("A","B", "C", "D"), 5)
sample(names(vector))


# Q3
random_step <- sample(c(1, -1), 25, prob = c(0.5, 0.5), replace=TRUE)
X <- c(0,cumsum(random_step))
plot(X, type = "l")

more_step <- sample(c(1, -1), 1000, prob = c(0.5, 0.5), replace=TRUE)
X <- c(0,cumsum(more_step))
plot(X, type = "l")

X_25 <- 2*rbinom(1, 25, 0.5)-25
X_25_vector <- 2*rbinom(1e5, 25, 0.5)-25
sum(X_25_vector > 10)/1e5
1 - pbinom(17, 25, 0.5)


# Q4
D <- diag(c(1/(1:10)), nrow = 10)
U <- matrix(rep(-1, 100), nrow = 10)
diag(U) <- 4
U <- U %*% (diag(1/diag(sqrt((t(U) %*% U)))))
# for(i in 1:ncol(U)) {
# U[, i] <- U[, i] / sqrt(sum(U[, i]^2))
# }
stopifnot(identical(sqrt(colSums(U ** 2)), rep(1, ncol(U))))

X <- U %*% D %*% t(U)
eigen(X)
# the eigenvalues are the same as diagonal entries in D
# vector recycling: R likes to operate on vectors of the same length, so if it encounters two vectors of different lengths in a binary operation (*), it merely replicates (recycles) the smaller vector until it is the same length as the longest vector, then it does the operation.
(1/(1:10)) * t(U) 

# Q5
x <- 0.3
I <- 20
b <- vector(length=I)
i <- 1

while(x > 0){
  y <- 2 * x
  if (y >= 1){
    b[i] <- 1
  } else {
    b[i] <- 0
  }
  x <- y - b[i]
  if (x == 0 | i == I){
    stop
  } else {
    i <- i + 1
  }
}

bin_rep <- function(x, I){
  i <- 1
  b <- vector(length=I)
  while(x > 0){
    y <- 2 * x
    if (y >= 1){
      b[i] <- 1
    } else {
      b[i] <- 0
    }
    x <- y - b[i]
    if (x == 0 | i == I){
      return(b)
    } else {
      i <- i + 1
    }
  }
}

bin_0.3 <- bin_rep(0.3, 60)
bin_0.1_three_times <- bin_rep(0.1 + 0.1 + 0.1, 60)
which.min(bin_0.3 != bin_0.1_three_times)# which.min(x) and which.max(x) return the index of the first FALSE or TRUE, as FALSE < TRUE
# match(TRUE, bin_0.3 != bin_0.1_three_times)

