setwd("Your working directory")
source("helper_PCA.R")


# Experiment 7 PCA

generate_sample <- function(ind, amount, magnitude, vars=c(3, 10, 17), hidden=3){
  
  if (amount == "B"){
    n <- 10000
  }
  else if (amount == "SS"){
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
  y <- x[, vars[1]]^2 + x[, vars[2]] + sqrt(abs(x[, vars[3]])) + 
    z %*% runif(hidden, lowerbound, upperbound) + eps
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

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

mixfun <- function(sample, reg=T, num_hidden=0){
  # Use formula and true hidden vars to generate data samples
  if (reg){
    if (num_hidden == 0){
      result <- foci_main(sample$y, sample$x, num_features=4)
    } else {
      result <- foci_new(sample$y, sample$x, XPCA = sample$XPCA, 
                         num_hidden = num_hidden, num_features=4,
                         real=FALSE)
    }
  }
  else{
    if (num_hidden == 0){
      result <- foci_main(sample$y, sample$x)
    } else {
      result <- foci_new(sample$y, sample$x, XPCA = sample$XPCA, 
                         num_hidden = num_hidden, real=FALSE)
    }
  }
  
  temp <- result$selectedVar
  temp <- temp$names
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
  list(freq = freq, cval = cvalue)
}

experiment <- function(pc, amount, magnitude, reg=T, B=20){
  samples <- mclapply(1:B, function(i) generate_sample(2021+i, amount=amount, magnitude=magnitude), mc.cores=8)
  res_fin <- mclapply(0:pc, function(j) mclapply(1:B, function(i) mixfun(sample = samples[[i]], reg=reg, num_hidden = j), mc.cores=8), mc.cores=8)
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

# Choices of amount: "B": 10000, "SS": 500 (samples)

location <- c("Your directory for saving the RData results")

cat("------------------N = 10000, magnitude = medium, no reg begins-----------------------\n")

PCAL <- experiment(pc=20, amount="B", magnitude = "M", reg=F)
save(PCAL, file=paste0(location, "PCAL.RData"))

cat("--------------------------Ends-------------------------------------------------------\n")
cat("------------------N = 500, magnitude = medium, no reg begins-----------------------\n")

PCAS <- experiment(pc=20, amount="SS", magnitude = "M", reg=F)
save(PCAS, file=paste0(location, "PCAS.RData"))

cat("--------------------------Ends------------------------------------------------------\n")
cat("------------------N = 10000, magnitude = small, no reg begins-----------------------\n")

PCALmagS <- experiment(pc=20, amount="B", magnitude = "S", reg=F)
save(PCALmagS, file=paste0(location, "PCALmagS.RData"))

cat("--------------------------Ends-----------------------------------------------------\n")

cat("------------------N = 10000, magnitude = large, no reg begins-----------------------\n")

PCALmagL <- experiment(pc=20, amount="B", magnitude = "L", reg=F)
save(PCALmagL, file=paste0(location, "PCALmagL.RData"))

cat("--------------------------Ends-----------------------------------------------------\n")

cat("------------------N = 500, magnitude = small, no reg begins-----------------------\n")

PCASmagS <- experiment(pc=20, amount="SS", magnitude = "S", reg=F)
save(PCASmagS, file=paste0(location, "PCASmagS.RData"))

cat("--------------------------Ends----------------------------------------------------\n")

cat("------------------N = 500, magnitude = large, no reg begins-----------------------\n")

PCASmagL <- experiment(pc=20, amount="SS", magnitude = "L", reg=F)
save(PCASmagL, file=paste0(location, "PCASmagL.RData"))

cat("--------------------------Ends----------------------------------------------------\n")
