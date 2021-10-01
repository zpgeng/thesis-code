setwd("Your working directory")
source("helper_2.R")

generate_sample <- function(ind, vars=c(3, 10, 77), hidden=3, small=T){
  set.seed(ind)
  n <- ifelse(small, 100000, 10000)
  p <- 100
  # Assume that there are 4 hidden variables z1 z2 z3.
  z <- matrix(runif(n * hidden, 10, 20), nrow = n)
  b <- matrix(runif(p * hidden, 0, 1), nrow = hidden)
  eps_all <- matrix(rnorm(n * p), nrow = n)
  x <- z %*% b + eps_all
  colnames(x) = paste0(rep("x", p), seq(1, p))
  eps <- rnorm(n)
  y <- x[, vars[1]] + x[, vars[2]] + x[, vars[3]] + 
    z %*% runif(hidden) + eps
  #y <- x[, 3] + x[, 10] + x[, 77] - z %*% runif(3) + eps
  X.PCA <- prcomp(x, center = TRUE, scale. = TRUE)
  return(list(y = y, x = x, z = z, XPCA = X.PCA))
}

mixfun <- function(i, sample, num_hidden=0){
  # Use formula and true hidden vars to generate data samples
  if (num_hidden == 0){
    result <- foci(sample$y, sample$x)
  } else {
    result <- foci_new(sample$y, sample$x, XPCA = sample$XPCA, 
                       num_hidden = num_hidden,
                       real=FALSE)
  }
  temp <- result$selectedVar
  temp$names
}

sample_all <- function(loop){
  lapply(1:loop, function(i) generate_sample(i+2020, small = TRUE))
}

experiment <- function(pc, loop){
  samples <- sample_all(loop)
  res_fin <- lapply(1:loop, function(i) mixfun(i, num_hidden = pc, sample = samples[[i]]))
  TP <- c()
  FP <- c()
  temp <- unlist(res_fin)
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
  list(TP = TP, FP = FP , precision = TP/(TP + FP))
}


true_exp <- function(r, small=TRUE){
  sample <- generate_sample(r+2020, small = small)
  temp <- foci_new(sample$y, sample$x, real=sample$z)
  flag <- temp$selectedVar
  flag$names
}

compare <- function(loop){
  # True case
  res_true <- unlist(sapply(1:loop, function(r) true_exp(r)))
  true_TP <- sum(res_true == "x3" | res_true == "x10" | res_true == "x77")
  true_FP <- length(res_true) - true_TP
  list(TTP = true_TP, TFP = true_FP,
       Tprecision = true_TP/(true_TP + true_FP))
}

## We can use the previous results to compare, no need to re-run.

## PC = 100, LOOP = 20

require(FOCI)
experiment(100, 20)
compare(20)