## Comparing Z and Est. Z
set.seed(2021)
n = 10000
p = 100
z <- rbeta(n, 5, 4)
b <- matrix(runif(p * 1, 0, 1), nrow = 1)
eps_all <- matrix(rnorm(n * p), nrow = n)
x <- z %*% b + eps_all
colnames(x) = paste0(rep("x", p), seq(1, p))
eps <- rnorm(n)
x.pca <- prcomp(x)
z_est <- x.pca$x[1:n, 1]

z_std <- scale(z)
z_eststd <- scale(z_est)

library(ggplot2)
library(overlapping)

df <- data.frame(x = c(z_std, z_eststd), gg = factor(rep(1:2, c(n, n))))
ggplot(df, aes(x, colour = gg)) + stat_ecdf() + 
  scale_colour_hue(name="Legend", labels=c('Standardized Z','Estimated Z')) + labs(x = "x", y = "Fn(x)")

overlap(list(x=z_std, y=z_eststd))
