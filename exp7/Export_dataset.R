# Generating small datasets with 500 entries.

small = TRUE
n <- ifelse(small, 500, 10000)
p <- 20
hidden <- 3
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

dfs <- data.frame(x)
write.csv(dfs,"smalldata.csv", row.names = F)



# Generating large datasets with 10000 entries.

small = FALSE
n <- ifelse(small, 500, 10000)
p <- 20
hidden <- 3
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

dfs <- data.frame(x)
write.csv(dfs,"largedata.csv", row.names = F)
