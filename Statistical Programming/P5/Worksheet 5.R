# Q1
# test_that("can calculate moving average for x=1:10, k=3", {
#   expect_equal(
#     calculate_moving_average_for_k(x = c(1:10), k = 3), 
#     c(1.5, 2:9, 9.5))
# })

calculate_moving_average_for_k <- function(x, k) {
  n <- length(x)
  sapply(1:n, function(j) {
    i1 <- max(1, j - (k - 1) / 2)
    i2 <- min(n, j + (k - 1) / 2)
    (1 / (i2 - i1 + 1)) * sum(x[(i1:i2)])
  })
}    

# test for expected behaviour (of the return value)
library("testthat")
test_that("can calculate moving average for x=1:10, k=3", {
  expect_equal(
    calculate_moving_average_for_k(x = 1:10, k = 3), 
    c(1.5, 2:9, 9.5))
})

calculate_moving_averages <- function(x, k_vec) {
  sapply(k_vec, function(j) {
    calculate_moving_average_for_k(x, j)
  })
}

# alternatively
calculate_moving_average_for_k <- function(x, k) {
  n <- length(x)
  sapply(k_vec, function(j) {
    sapply(1:n, function(J){
    i1 <- max(1, J - (j - 1) / 2)
    i2 <- min(n, J + (j - 1) / 2)
    (1 / (i2 - i1 + 1)) * sum(x[(i1:i2)])
  })
 })
}


test_that("can calculate moving average for k = 1, k = 3", {
  expected_result <- matrix(nrow = 10, ncol = 2)
  expected_result[, 1] <- 1:10
  expected_result[, 2] <- c(1.5, 2:9, 9.5)
  expect_equal(
    calculate_moving_averages(1:10, c(1, 3)),
    expected_result
  )
})


# base R
# check k is numeric
try(calculate_moving_average_for_k(x = c(1, 2, 3), k = "b"))
# check x is numeric
try(calculate_moving_average_for_k(x = "a", k = 1))
# check k is integer
try(calculate_moving_average_for_k(x = c(1:10), k = 1.5))
# it should return an error for non-integer k - so need to modify the function:
# if ((class(k) != integer) || (k < = 0))
 #  stop ("k is not a positive integer.")

# check if k_vec have integer entris
try(calculate_moving_averages(x = c(1:10), k = c(1.5, 2.5)))
# no error - need to modify the function

# testthat package
library(testthat)
expect_error(calculate_moving_average_for_k(x = c(1, 2, 3), k = 1.5))
# did not throw an error - need to modify the function


# Q2
n1 <- 200
n2 <- 100
nWeights <- 50
ref_allele_matrix <- matrix(sample(c(0, 1), n1 * n2, replace = TRUE), nrow = n1)
weight_matrix <- matrix(rnorm(n1 * n2, mean = 1, sd = 0.1), nrow = n1)
Gamma_Weights_States <- runif(nWeights)
row_update <- 1

initial_function <- function(
  ref_allele_matrix,
  fst,
  weight_matrix,
  Gamma_Weights_States,
  row_update
){
  x1_sums <- colSums(
    weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]
  )
  x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
  x1_matrix <- t(x1_matrix)
  x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
  B <- x1_matrix + x2
  S <- colSums(weight_matrix[-row_update,])
  A <- outer(S,Gamma_Weights_States, FUN='+')
  allele_frequencies <- t(B)/A
  allele_frequencies
}


new_function <- function(
  ref_allele_matrix,
  fst,
  weight_matrix,
  Gamma_Weights_States,
  row_update
){
  x1_sums <- colSums(
    weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]
  )
  x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
  x1_matrix <- t(x1_matrix)
  x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
  B <- x1_matrix + x2
  S <- colSums(weight_matrix[-row_update,])
  A <- outer(S,Gamma_Weights_States, FUN='+')
  allele_frequencies <- t(B)/A
  allele_frequencies
}



my_test <- function(){
test_that("compare the initial and new function results", {
  expect_equal(
      initial_function(
         ref_allele_matrix,
         fst,
         weight_matrix,
         Gamma_Weights_States,
         row_update
      ), 
      new_function(
         ref_allele_matrix,
         fst,
         weight_matrix,
         Gamma_Weights_States,
         row_update
      )
  )}
)}

my_test()



library(microbenchmark)
my_benchmark <- function(){
  microbenchmark(
    initial_function(
      ref_allele_matrix,
      fst,
      weight_matrix,
      Gamma_Weights_States,
      row_update
      ), 
    new_function(
      ref_allele_matrix,
      fst,
      weight_matrix,
      Gamma_Weights_States,
      row_update),
    times = 100
)}

my_benchmark()



library(profvis)
profvis({
  for (i in 1:1000) {
    x1_sums <- colSums(
      weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]
    )
    x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
    x1_matrix <- t(x1_matrix)
    x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
    B <- x1_matrix + x2
    S <- colSums(weight_matrix[-row_update,])
    A <- outer(S,Gamma_Weights_States, FUN='+')
    allele_frequencies <- t(B)/A
    allele_frequencies
  }
})



microbenchmark(
  colSums(weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]),
  colSums(
    weight_matrix*ref_allele_matrix
  ) - 
    weight_matrix[row_update,]*ref_allele_matrix[row_update,],
  times = 1000
)



new_function <- function(
  ref_allele_matrix,
  fst,
  weight_matrix,
  Gamma_Weights_States,
  row_update
){
  x1_sums <- colSums(
    weight_matrix*ref_allele_matrix
  ) - 
    weight_matrix[row_update,]*ref_allele_matrix[row_update,]
  x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
  x1_matrix <- t(x1_matrix)
  x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
  B <- x1_matrix + x2
  S <- colSums(weight_matrix[-row_update,])
  A <- outer(S,Gamma_Weights_States, FUN='+')
  allele_frequencies <- t(B)/A
  allele_frequencies
}

my_test()
my_benchmark()



f1 <- function(){
  x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
  x1_matrix <- t(x1_matrix)
  return(NULL)
}

f2 <- function(){
  n <- length(Gamma_Weights_States)
  x1_matrix <- matrix(rep(x1_sums, each = n, nrow = n))
  return(NULL)                      
}                         

f3 <- function(){
  n <- length(Gamma_Weights_States)
  m <- length(x1_sums)
  x1_matrix <- matrix(0, nrow = n, ncol = m)
  for (i in 1:n){
  # each column contains an entry of x1_sums in order
  x1_matrix [, i] <- x1_sums[i]
  }
  return(NULL)                      
}  

microbenchmark(f1, f2, f3, f4, times = 10000)



new_function <- function(
  ref_allele_matrix,
  fst,
  weight_matrix,
  Gamma_Weights_States,
  row_update
){
  x1_sums <- colSums(
    weight_matrix*ref_allele_matrix
  ) - 
    weight_matrix[row_update,]*ref_allele_matrix[row_update,]
  x1_matrix <- matrix(0, nrow = length(Gamma_Weights_States), 
                         ncol = length(x1_sums))
  for (i in 1:length(x1_sums)){
    x1_matrix [, i] <- x1_sums[i]
  }
  x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
  B <- x1_matrix + x2
  S <- colSums(weight_matrix[-row_update,])
  A <- outer(S,Gamma_Weights_States, FUN='+')
  allele_frequencies <- t(B)/A
  allele_frequencies
}

my_test()
my_benchmark()



microbenchmark(
  colSums(weight_matrix[-row_update,]),
  colSums(weight_matrix) - weight_matrix[row_update,],
  times = 1000
)




new_function <- function(
  ref_allele_matrix,
  fst,
  weight_matrix,
  Gamma_Weights_States,
  row_update
){
  x1_sums <- colSums(
    weight_matrix*ref_allele_matrix
  ) - 
    weight_matrix[row_update,]*ref_allele_matrix[row_update,]
  x1_matrix <- matrix(0, nrow = length(Gamma_Weights_States), 
                      ncol = length(x1_sums))
  for (i in 1:length(x1_sums)){
    x1_matrix [, i] <- x1_sums[i]
  }
  x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
  B <- x1_matrix + x2
  S <- colSums(weight_matrix) - weight_matrix[row_update,]
  A <- outer(S,Gamma_Weights_States, FUN='+')
  allele_frequencies <- t(B)/A
  allele_frequencies
}

my_test()
my_benchmark()