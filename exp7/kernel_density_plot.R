install.packages("hrbrthemes")
library(ggplot2)
library(dplyr)
library(hrbrthemes)

magnitude_sample <- function(amount, vars=c(3, 10, 17), hidden=3){
  # Signal are x3 x10 x17
  if (amount == "L"){
    n <- 10000
  }
  
  else if (amount == "S"){
    n <- 500
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
  x1 <- x[, vars[1]]^2
  x2 <- x[, vars[2]] 
  x3 <- sqrt(abs(x[, vars[3]]))
  hid <- z %*% runif(hidden, 0, 0.01) + eps
  list(x1 = x1, x2 = x2, x3 = x3, hid = hid)
}

df <- magnitude_sample("L")

data <- data.frame(
  type = c( rep("variable 1", 10000), rep("variable 2", 10000), 
            rep("variable 3", 10000), rep("hidden", 10000)),
  value = c( df$x1, df$x2, df$x3, df$hid )
)


ggplot(data, aes(x=value, fill=type)) + 
  geom_density(color="#e9ecef", alpha=0.4, position = "identity")




