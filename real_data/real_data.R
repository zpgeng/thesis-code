dfred <- read.csv("./real_data/winequality-red.csv", sep = ";")
dfwhite <- read.csv("./real_data/winequality-white.csv", sep = ";")

df <- rbind(dfwhite, dfred[1:102, ])
dfs <- data.frame(df)

# This step is to import the data to Python for reproducibility
write.csv(dfs,"wine.csv", row.names = F)

# Check if there are NA entries
library(tidyverse)
df %>% is.na() %>% colSums()

# Split the data into train and test set
set.seed(2021)
test_index <- sample(1:5000, 1000, replace = F)
Xtrain <- as.matrix(df[-test_index, 1:10])
ytrain <- df[-test_index, 12]
Xtest <- as.matrix(df[test_index, 1:10])
ytest <- df[test_index, 12]
N <- dim(Xtrain)[1]


# LASSO
library(glmnet)
LAMBDAs = exp(seq(log(0.05), log(5), length.out=20))
lasso = glmnet(as.matrix(Xtrain), as.matrix(ytrain),
               lambda=LAMBDAs, alpha=1, seed=2021)
Ss = colSums(lasso$beta!=0)
yfit = predict(lasso, Xtrain)
ypred = predict(lasso, Xtest)

EBICseq <- function(yfits, true, Ss, n, c=3) {
  EBICs = c()
  for (i in 1:ncol(yfits)) {
    ebic = n*log(mean((yfits[, i] - true)^2)) + c * Ss[i] * log(n)
    EBICs = c(EBICs, ebic)
  }
  return(EBICs)
}

EBICs = EBICseq(yfit, ytrain, Ss, N)
best_idx = which.min(EBICs)

(supp_lasso = c(1:10)[lasso$beta[, best_idx]!=0])
(train_mse_lasso = mean((yfit[, best_idx] - ytrain)^2))
(test_mse_lasso = mean((ypred[, best_idx] - ytest)^2))

# SCAD
library(ncvreg)
LAMBDAs = exp(seq(log(1), log(0.01), length.out=20))
scad = ncvreg(Xtrain, ytrain, penalty="SCAD",
              lambda = LAMBDAs, seed=2021)
Ss = predict(scad, Xtrain, type="nvars")
yfits = predict(scad, Xtrain)
ypred = predict(scad, Xtest)
EBICs = EBICseq(yfits, ytrain, Ss, N)
best_idx = which.min(EBICs)
(supp_scad = c(1:10)[scad$beta[, best_idx][-1]!=0])
(train_mse_scad = mean((yfits[, best_idx] - ytrain)^2))
(test_mse_scad = mean((ypred[, best_idx] - ytest)^2))

# FOCI
library(FOCI)
library(randomForest)
FOCI <- foci(df[, 12], df[, 1:10], numCores = 1)
foci_train <- data.frame(cbind(Xtrain[,FOCI$selectedVar$index], ytrain))
foci_test <- data.frame(cbind(Xtest[,FOCI$selectedVar$index], ytest))

model1 <- randomForest(ytrain ~ ., data=foci_train, importance=T)
yfit <- predict(model1, foci_train)
ypred <- predict(model1, foci_test)

(supp_foci = FOCI$selectedVar$index)
(train_mse_foci = mean((yfit - ytrain)^2))
(test_mse_foci = mean((ypred - ytest)^2))


# FOCI with oracle PCA
source("./helper.R")
XPCA <- prcomp(df[, 1:10], center = TRUE, scale. = TRUE)
FOCI_opca <- foci_new(df[, 12], df[, 1:10], XPCA=XPCA, 
                      num_hidden = 1,  numCores = 1)
pc1 <- prcomp(Xtrain, center = TRUE, scale. = TRUE)
pc2 <- prcomp(Xtest, center = TRUE, scale. = TRUE)
foci_train <- data.frame(cbind(Xtrain[,FOCI_opca$selectedVar$index],
                               pc1$x[, 1], ytrain))
foci_test <- data.frame(cbind(Xtest[,FOCI_opca$selectedVar$index],
                              pc2$x[, 1], ytest))

model2 <- randomForest(ytrain ~ ., data=foci_train, importance=T)
yfit <- predict(model2, foci_train)
ypred <- predict(model2, foci_test)

supp_foci_oracle = FOCI_opca$selectedVar$index
(train_mse_foci_oracle = mean((yfit - ytrain)^2))
(test_mse_foci_oracle = mean((ypred - ytest)^2))


# FOCI with full PCA
source("./helper.R")
XPCA <- prcomp(df[, 1:10], center = TRUE, scale. = TRUE)
FOCI_fpca <- foci_new(df[, 12], df[, 1:10], XPCA=XPCA, 
                      num_hidden = 10,  numCores = 1)
pc3 <- prcomp(Xtrain, center = TRUE, scale. = TRUE)
pc4 <- prcomp(Xtest, center = TRUE, scale. = TRUE)
foci_train <- data.frame(cbind(Xtrain[,FOCI_fpca$selectedVar$index],
                               pc3$x[, 1:10], ytrain))
foci_test <- data.frame(cbind(Xtest[,FOCI_fpca$selectedVar$index],
                              pc4$x[, 1:10], ytest))

model3 <- randomForest(ytrain ~ ., data=foci_train, importance=T)
yfit <- predict(model3, foci_train)
ypred <- predict(model3, foci_test)

(supp_foci_full = FOCI_fpca$selectedVar$index)
(train_mse_foci_full = mean((yfit - ytrain)^2))
(test_mse_foci_full = mean((ypred - ytest)^2))

# FOCI with oracle VAE
source("./helper_vae.R")
# This is to read the generated VAE latent estimte of Z from local
ZVAE <- read.csv("zvae.csv")
ZVAE <- data.matrix(ZVAE)

Ztrain <- ZVAE[-test_index, ]
Ztest <- ZVAE[test_index, ]
FOCI_vae <- foci_new(df[, 12], df[, 1:10], ZVAE = ZVAE,
                     real=FALSE, numCores = 1)

foci_train <- data.frame(Xtrain[,FOCI_vae$selectedVar$index],
                               Zest = Ztrain, ytrain)
foci_test <- data.frame(Xtest[,FOCI_vae$selectedVar$index],
                              Zest = Ztest, ytest)

model4 <- randomForest(ytrain ~ ., data=foci_train, importance=T)
yfit <- predict(model4, foci_train)
ypred <- predict(model4, foci_test)

(supp_foci_vae = FOCI_vae$selectedVar$index)
(train_mse_foci_vae = mean((yfit - ytrain)^2))
(test_mse_foci_vae = mean((ypred - ytest)^2))
