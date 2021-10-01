setwd("Your working directory")
source("helper_VAE.R")


generate_sample <- function(ind, amount, magnitude, vars=c(3, 10, 17), hidden=3){
  # Signal are x3 x10 x17
  if (amount == "L"){
    n <- 10000
  }
  else if (amount == "S"){
    n <- 500
  }
  
  if (magnitude == "L"){
    lowerbound <- 10
    upperbound <- 20
  }
  else if (magnitude == "M"){
    lowerbound <- 0
    upperbound <- 1
  }
  else if (magnitude == "S"){
    lowerbound <- 0
    upperbound <- 0.01
  }
  
  p <- 20
  # Assume that there are 3 hidden variables z1 z2 z3.
  set.seed(2021)
  z <- matrix(runif(n * hidden, 0, 1), nrow = n)
  b <- matrix(runif(p * hidden, 0, 1), nrow = hidden)
  eps_all <- matrix(rnorm(n * p), nrow = n)
  flag <- z %*% b
  flag1 <- flag[, 1:10]
  flag2 <- matrix(rnorm(n * (p - 10)), nrow = n)
  x <- cbind(flag1, flag2)
  x <- x + eps_all
  colnames(x) = paste0(rep("x", p), seq(1, p))
  eps <- rnorm(n)
  # The formula you want to use, can be manually changed at your interest
  y <- x[, vars[1]]^2 + x[, vars[2]] + sqrt(abs(x[, vars[3]])) + 
    z %*% runif(hidden, lowerbound, upperbound) + eps
  mysample <- function(ind, mydata){
    set.seed(ind)
    indices <- sample(1:nrow(mydata$x), nrow(mydata$x)/2,
                      replace=FALSE)
    list(y = mydata$y[indices], x = mydata$x[indices, ],
         z = mydata$z[indices, ])
  }
  mydata <- list(y = y, x = x, z = z)
  temp1 <- mysample(ind, mydata)
  return(temp1)
}


is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

mixfun <- function(ind, sample, reg=T, num_hidden=0){
  # Use formula and true hidden vars to generate data samples
  if (nrow(sample$x) == 5000){
    amount = "L"
  }
  else if (nrow(sample$x) == 250){
    amount = "S"
  }
  
  location <- c("Your location for saving the python outputs")
  if (reg){
    if (num_hidden == 0){
      result <- foci_main(sample$y, sample$x, num_features=4)
    } else {
      # Note that this ZVAE file is generated using Python with the code
      # file of 'VAE_Latent_Confounder_Finder.ipynb'
      ZVAE <- read.csv(paste0(location, amount, num_hidden, "file.csv"))
      ZVAE <- data.matrix(ZVAE)
      set.seed(ind)
      indices <- sample(1:nrow(ZVAE), nrow(ZVAE)/2, replace=FALSE)
      z_est <- ZVAE[indices, ]
      result <- foci_new(sample$y, sample$x, ZVAE = z_est,
                         num_features=4, real=FALSE)
    }
  }
  else{
    if (num_hidden == 0){
      result <- foci_main(sample$y, sample$x)
    } else {
      ZVAE <- read.csv(paste0(location, amount, num_hidden, "file.csv"))
      ZVAE <- data.matrix(ZVAE)
      set.seed(ind)
      indices <- sample(1:nrow(ZVAE), nrow(ZVAE)/2, replace=FALSE)
      z_est <- ZVAE[indices, ]
      result <- foci_new(sample$y, sample$x, ZVAE = z_est, 
                         real=FALSE)
    }
  }
  
  temp <- result$selectedVar$names
  # p = 20
  v <- paste0(rep("x", 20), seq(1, 20))
  flag <- sapply(1:20, function(i) is.element(v[i], temp))
  freq <- ifelse(flag == TRUE, 1, 0)
  tvals <- result$stepCODEC
  indset <- lapply(1:length(v), function(i) which(v[i] == temp))
  cvalue <- numeric(20)
  for(i in 1:length(indset)){
    if (is.integer0(indset[[i]])){
      indset[[i]] <- 0
    }
    else {
      cvalue[i] <- tvals[indset[[i]]] # list of codec values
    }
  }
  return(list(freq = freq, cval = cvalue))
}

experiment <- function(pc, amount, magnitude, reg=T, B=20){
  samples <- mclapply(1:B, function(i) generate_sample(2021+i, amount=amount, magnitude=magnitude), mc.cores=8)
  res_fin <- mclapply(0:pc, function(j) mclapply(1:B, function(i) mixfun(2021 + i, sample = samples[[i]], reg=reg, num_hidden = j), mc.cores=8), mc.cores=8)
  res <- list()
  for (k in 1:(pc+1)){
    freqvec <- numeric(20)
    cvec <- numeric(20)
    for (l in 1:B){
      freqvec <- freqvec + res_fin[[k]][[l]]$freq
      cvec <- cvec + res_fin[[k]][[l]]$cval
    }
    cval <- cvec/freqvec
    cval[is.na(cval)] <- 0
    res[[k]] <- list(freq = freqvec, cval = cval)
  }
  res
}

require(FOCI)
require(parallel)


loc <- c("Your directory for saving the RData results")

# Choices of amount: "L": 10000, "S": 500 (samples)


cat("------------------N = 10000,  magnitude = medium, no reg begins-----------------------\n")

VAEL <- experiment(pc=20, amount="L", magnitude = "M",reg=F)
save(VAEL, file=paste0(loc, "VAEL.RData"))

cat("--------------------------Ends-------------------------------\n")


cat("------------------N = 500,  magnitude = medium, no reg begins-----------------------\n")

VAES <- experiment(pc=20, amount="S", magnitude = "M", reg=F)
save(VAES, file=paste0(loc, "VAES.RData"))

cat("--------------------------Ends-------------------------------\n")

cat("------------------N = 10000, magnitude = large, no reg begins-----------------------\n")

VAELmagL <- experiment(pc=20, amount="L", magnitude="L", reg=F)
save(VAELmagL, file=paste0(loc, "VAELmagL.RData"))

cat("--------------------------Ends------------------------------------------------------\n")

cat("------------------N = 10000, magnitude = small, no reg begins-----------------------\n")

VAELmagS <- experiment(pc=20, amount="L", magnitude="S", reg=F)
save(VAELmagS, file=paste0(loc, "VAELmagS.RData"))

cat("--------------------------Ends------------------------------------------------------\n")


cat("------------------N = 500, magnitude = large, no reg begins-----------------------\n")

VAESmagL <- experiment(pc=20, amount="S", magnitude="L", reg=F)
save(VAESmagL, file=paste0(loc, "VAESmagL.RData"))

cat("--------------------------Ends----------------------------------------------------\n")


cat("------------------N = 500, magnitude = large, no reg begins-----------------------\n")

VAESmagS <- experiment(pc=20, amount="S", magnitude="S", reg=F)
save(VAESmagS, file=paste0(loc, "VAESmagS.RData"))

cat("--------------------------Ends----------------------------------------------------\n")