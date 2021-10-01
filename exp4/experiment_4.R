# CODEC comparison Experiment

set.seed(2021)
n = 10000
p = 100
# Assume that there are 3 hidden variables z1 z2 z3.
z <- matrix(runif(n * 3, 10, 20), nrow = n)
b <- matrix(runif(p * 3, 0, 1), nrow = 3)
eps_all <- matrix(rnorm(n * p), nrow = n)
x <- z %*% b + eps_all
colnames(x) = paste0(rep("x", p), seq(1, p))
x.pca <- prcomp(x, center = T, scale. = T)
z_est <- x.pca$x[1:n, 1:3]
colnames(z_est) = paste0(rep("z_est", 3), seq(1, 3))
x_new <- cbind(x, z_est)
eps <- rnorm(n)
y <- x[, 3] + x[, 10] + x[, 77] + z %*% runif(3) + eps

require(pracma)
P <- randortho(3, type = "orthonormal")
P_new <- randortho(3, type = "orthonormal")
c0 <- codec(y, x[, 3:10], z_est)
# 3-D Rotation Matrix
theta <- pi/5
R <- matrix(c(cos(theta), -1*sin(theta), 0, 
              sin(theta), cos(theta), 0,
              0, 0, 1), nrow = 3, byrow = T)
(c1 <- codec(y, x[, 3:10], z_est %*% R))
# Orthogonal Matrix
(c2 <- codec(y, x[, 3:10], z_est %*% P))
(c3 <- codec(y, x[, 3:10], z_est %*% P_new))
c0 == c1; c1 == c2; c2 == c3; c3==c1
# Invertible Matrix
I <- matrix(c(1,4,7,4,2,5,1,2,3), nrow = 3, byrow = T)
codec(y, x[, 10], z_est %*% I)

codec(y, x[, 1], z_est)
codec(y, x[, 1], z_est %*% R)
codec(y, x[, 1], z_est %*% P)
codec(y, x[, 1], z_est %*% P_new)

# New experiment regarding invertible but not orthogonal matrix
require(pracma)
set.seed(2021)
U <- randortho(3, type = "orthonormal")
U_new <- randortho(3, type = "orthonormal")


d <- diag(c(1,3,5))
d_new <- diag(c(2,4,6))

P <- U %*% d %*% t(U)
P_new <- U_new %*% d_new %*% t(U_new)
invtrans <- function(n, P){
  set.seed(2021)
  p = 100
  # Assume that there are 3 hidden variables z1 z2 z3.
  z <- matrix(runif(n * 3, 10, 20), nrow = n)
  b <- matrix(runif(p * 3, 0, 1), nrow = 3)
  eps_all <- matrix(rnorm(n * p), nrow = n)
  x <- z %*% b + eps_all
  colnames(x) = paste0(rep("x", p), seq(1, p))
  x.pca <- prcomp(x, center = T, scale. = T)
  z_est <- x.pca$x[1:n, 1:3]
  colnames(z_est) = paste0(rep("z_est", 3), seq(1, 3))
  x_new <- cbind(x, z_est)
  eps <- rnorm(n)
  y <- x[, 3] + x[, 10] + x[, 77] + z %*% runif(3) + eps
  v1 <- codec(y, x[, 3:10], z_est)
  v1_P <- codec(y, x[, 3:10], z_est %*% P)
  diff <- v1_P - v1
  list(diff = diff, origin = v1)
}

reldiff <- c()
for (n in seq(500, 40000, by = 500)){
  temp <- invtrans(n, P)
  reldiff <- c(reldiff, temp$diff/temp$origin)
}

diffset <- data.frame(number = seq(500, 40000, by = 500),
                      difference = abs(reldiff))
require(ggplot2)
ggplot(diffset, aes(x = number, y = difference)) + geom_point()


# Robustness of variable selection

# Loading helper function
setwd("Your working directory")
source("proj_helper.R")


generate_sample <- function(ind, vars=c(3, 10, 77), hidden=3, small=T){
  set.seed(ind)
  n <- ifelse(small, 5000, 10000)
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
  #y <- x[, 3] + x[, 10] + x[, 77] - z %*% runif(3) + eps
  X.PCA <- prcomp(x, center = TRUE, scale. = TRUE)
  return(list(y = y, x = x, XPCA = X.PCA))
}

mixfun <- function(i, sample, proj=FALSE, num_hidden=0){
  # Use formula and true hidden vars to generate data samples
  if (num_hidden == 0){
    result <- foci(sample$y, sample$x)
  } else {
    result <- foci_proj(sample$y, sample$x, sample$XPCA,
                        real=FALSE, num_hidden = num_hidden, proj=proj)
  }
  temp <- result$selectedVar
  temp$names
}

sample_all <- function(loop){
  lapply(1:loop, function(i) generate_sample(i+2020, small = TRUE))
}

expProj <- function(pc, loop, P){
  samples <- sample_all(loop)
  # Without projection
  res_orig <- lapply(0:pc, function(j) lapply(1:loop, function(i) mixfun(i, sample=samples[[i]], num_hidden = j)))
  TP_orig <- c()
  FP_orig <- c()
  for (k in 1:(pc+1)){
    temp <- unlist(res_orig[[k]])
    flag1 <- 0
    flag2 <- 0
    for (i in 1:length(temp)){
      if (temp[i] == "x3" | temp[i] == "x10" | temp[i] =="x77"){
        flag1 <- flag1 + 1
      } else {
        flag2 <- flag2 + 1 }
    }
    TP_orig <- c(TP_orig, flag1)
    FP_orig <- c(FP_orig, flag2)
  }
  
  # With projection
  res_proj <- lapply(2:pc, function(j) lapply(1:loop, function(i) mixfun(i, sample = samples[[i]], proj = P, num_hidden = j)))
  TP_proj <- c()
  FP_proj <- c()
  for (k in 1:(pc-1)){
    temp <- unlist(res_proj[[k]])
    flag1 <- 0
    flag2 <- 0
    for (i in 1:length(temp)){
      if (temp[i] == "x3" | temp[i] == "x10" | temp[i] =="x77"){
        flag1 <- flag1 + 1
      } else {
        flag2 <- flag2 + 1 }
    }
    TP_proj <- c(TP_proj, flag1)
    FP_proj <- c(FP_proj, flag2)
  }
  
  list(TP0 = TP_orig, FP0 = FP_orig , precision0 = TP_orig/(TP_orig + FP_orig),
       TP1 = TP_proj, FP1 = FP_proj , precision1 = TP_proj/(TP_proj + FP_proj)
  )
}

#---------------------------------------------------------------
# TP1, FP1 results shows the projection starting from 2*2,
# since 1*1 projection is kind of meaningless to this experiment.
#---------------------------------------------------------------

require(pracma)
require(FOCI)
(try <- expProj(6, 20, P = TRUE))