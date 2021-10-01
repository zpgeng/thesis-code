require(FOCI)

# Experiment 1

# first setting
indeprun <- function(i){
  set.seed(i)
  n = 1000
  p = 100
  z <- runif(1000, 0, 1)
  x <- matrix(rnorm(n * p), nrow = n)
  x[, 77] <- 10 * sqrt(z)
  colnames(x) = paste0(rep("x", p), seq(1, p))
  x.pca <- prcomp(x)
  z_est <- x.pca$x[1:1000, 1]
  x <- cbind(x, z_est)
  y <- x[, 1] * x[, 10] + x[, 60] * z
  # with num_features equal to 3 and stop equal to FALSE, foci will give a list of
  # three selected features
  result = foci(y, x, num_features = 4, stop = FALSE, numCores = 1)
  result
}

res <- c(1, 2, 3, 4)
for (i in 1:50){
  temp <- indeprun(i)$selectedVar
  temp <- temp$names
  res <- cbind(res, temp)
}

(count_x1 <- sum(res == "x1"))
(count_x10 <- sum(res == "x10"))
(count_x60 <- sum(res == "x60"))
(count_z <- sum(res == "z_est"))
(count_x77 <- sum(res == "x77"))

# second setting

deprun <- function(i){
  set.seed(i)
  n = 1000
  p = 100
  z <- runif(1000, 0, 1)
  x <- matrix(rnorm(n * p), nrow = n)
  x[, 77] <- 10 * sqrt(z)
  colnames(x) = paste0(rep("x", p), seq(1, p))
  x.pca <- prcomp(x)
  z_est <- x.pca$x[1:1000, 1]
  x <- cbind(x, z_est)
  y <- x[, 1] * x[, 10] + x[, 77] * z
  # with num_features equal to 3 and stop equal to FALSE, foci will give a list of
  # three selected features
  result = foci(y, x, num_features = 4, stop = FALSE, numCores = 1)
  result
}

res <- c(1, 2, 3, 4)
for (i in 1:50){
  temp <- deprun(i)$selectedVar
  temp <- temp$names
  res <- cbind(res, temp)
}

(count_x1 <- sum(res == "x1"))
(count_x10 <- sum(res == "x10"))
(count_z <- sum(res == "z_est"))
(count_x77 <- sum(res == "x77"))