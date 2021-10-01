# Experiment 6

setwd("Your working directory")
source("helper_6.R")

# Main Function Begins

generate_sample <- function(ind, vars=c(3, 10, 17), hidden=3){
  set.seed(ind)
  #n <- ifelse(small, 5000, 10000)
  n <- 10000
  p <- 20
  # Assume that there are 3 hidden variables z1 z2 z3.
  z <- matrix(runif(n * hidden, 10, 20), nrow = n)
  b <- matrix(runif(p * hidden, 0, 1), nrow = hidden)
  eps_all <- matrix(rnorm(n * p), nrow = n)
  flag <- z %*% b
  flag1 <- flag[, 1:10]
  flag2 <- matrix(rnorm(n * (p - 10)), nrow = n)
  x <- cbind(flag1, flag2)
  x <- x + eps_all
  colnames(x) = paste0(rep("x", p), seq(1, p))
  eps <- rnorm(n)
  y <- x[, vars[1]]^2 + 3*x[, vars[2]] + sqrt(x[, vars[3]]) + 
    z %*% runif(hidden) + eps
  mysample <- function(ind, mydata){
    set.seed(ind)
    indices <- sample(1:nrow(mydata$x), nrow(mydata$x)/2,
                      replace=FALSE)
    tpca <- prcomp(mydata$x[indices, ], center = TRUE,
                   scale. = TRUE)
    list(y = mydata$y[indices, ], x = mydata$x[indices, ],
         z = mydata$z[indices, ], XPCA = tpca)
  }
  mydata <- list(y = y, x = x, z = z)
  temp1 <- mysample(ind, mydata)
  return(temp1)
}

mixfun <- function(sample, num_hidden=0){
  # Use formula and true hidden vars to generate data samples
  if (num_hidden == 0){
    result <- foci(sample$y, sample$x, numCores = 1)
  } else {
    result <- foci_new(sample$y, sample$x, XPCA = sample$XPCA, 
                       num_hidden = num_hidden,
                       real=FALSE, numCores = 1)
  }
  temp <- result$selectedVar
  temp <- temp$names
  # p = 20
  v <- paste0(rep("x", 20), seq(1, 20))
  flag <- sapply(1:20, function(i) is.element(v[i], temp))
  ifelse(flag == TRUE, 1, 0)
}

experiment <- function(pc, B=20){
  samples <- mclapply(1:B, function(i) generate_sample(2021+i), mc.cores = 8)
  res_fin <- mclapply(0:pc, function(j) lapply(1:B, function(i) mixfun(sample = samples[[i]], num_hidden = j)), mc.cores = 8)
  freq_vec <- list()
  for (k in 1:(pc+1)){
    temp <- numeric(20)
    for (l in 1:B){
      temp <- temp + res_fin[[k]][[l]] 
    }
    freq_vec[[k]] <- temp
  }
  freq_vec
}

require(FOCI)
require(parallel)
experiment(pc=20)
