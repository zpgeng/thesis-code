setwd("Your working directory")
source("helper_2.R")

# Code for experiments of calculating precisions

generate_sample <- function(ind, vars=c(3, 10, 77), hidden=3, small=T){
  set.seed(ind)
  n <- ifelse(small, 1000, 10000)
  p <- 100
  # Assume that there are 3 hidden variables z1 z2 z3.
  z <- matrix(runif(n * hidden, 10, 20), nrow = n)
  b <- matrix(runif(p * hidden, 0, 1), nrow = hidden)
  eps_all <- matrix(rnorm(n * p), nrow = n)
  x <- z %*% b + eps_all
  colnames(x) = paste0(rep("x", p), seq(1, p))
  eps <- rnorm(n)
  y <- x[, vars[1]] + x[, vars[2]] + x[, vars[3]] + 
    z %*% runif(hidden) + eps
  return(list(y = y, x = x))
}

mixfun <- function(i, num_hidden=0){
  # Use formula and true hidden vars to generate data samples
  sample <- generate_sample(i+2020, small = FALSE)
  if (num_hidden == 0){
    result <- foci(sample$y, sample$x)
  } else {
    result <- foci_new(sample$y, sample$x, num_hidden = num_hidden,
                       real=FALSE)
  }
  temp <- result$selectedVar
  temp$names
}

experiment <- function(pc, loop){
  res_fin <- lapply(0:pc, function(j) lapply(1:loop, function(i) mixfun(i, num_hidden = j)))
  TP <- c()
  FP <- c()
  for (k in 1:(pc+1)){
    temp <- unlist(res_fin[[k]])
    flag1 <- 0
    flag2 <- 0
    for (i in 1:length(temp)){
      if (temp[i] == "x3" | temp[i] == "x10" | temp[i] =="x77"){
        flag1 <- flag1 + 1
      } else {
        flag2 <- flag2 + 1 }
    }
    TP <- c(TP, flag1)
    FP <- c(FP, flag2)
  }
  list(TP = TP, FP = FP , precision = TP/(TP + FP))
}

require(FOCI)
experiment(7, 100)


# Code for experiment including the true latent confounders

true_exp <- function(r, small=FALSE){
  sample <- generate_sample(r+2020, small = small)
  temp <- foci_new(sample$y, sample$x, real=sample$z)
  flag <- temp$selectedVar
  flag$names
}

compare <- function(loop){
  # True case
  res_true <- unlist(sapply(1:loop, function(r) true_exp(r)))
  true_TP <- sum(res_true == "x1" | res_true == "x10" | res_true == "x75")
  true_FP <- length(res_true) - true_TP
  list(TTP = true_TP, TFP = true_FP,
       Tprecision = true_TP/(true_TP + true_FP))
}

compare(100)


