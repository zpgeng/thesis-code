# Experiment with not dense effect on X

.estimateConditionalQ <- function (Y, X, Z) {
  
  id <- group <- rnn <- NULL
  
  if(!is.matrix(X)) {
    X = as.matrix(X)
  }
  if(!is.matrix(Z)) {
    Z = as.matrix(Z)
  }
  
  n = length(Y)
  
  W = cbind(X, Z)
  
  # compute the nearest neighbor of X
  nn_X = RANN::nn2(X, query = X, k = 3)
  nn_index_X = nn_X$nn.idx[, 2]
  # handling repeated data
  repeat_data = which(nn_X$nn.dists[, 2] == 0)
  
  df_X = data.table::data.table(id = repeat_data, group = nn_X$nn.idx[repeat_data, 1])
  df_X[, rnn := .randomNN(id), by = "group"]
  
  nn_index_X[repeat_data] = df_X$rnn
  # nearest neighbors with ties
  ties = which(nn_X$nn.dists[, 2] == nn_X$nn.dists[, 3])
  ties = setdiff(ties, repeat_data)
  
  if(length(ties) > 0) {
    helper_ties <- function(a) {
      distances <- proxy::dist(matrix(X[a, ], ncol = ncol(X)), matrix(X[-a, ], ncol = ncol(X)))
      ids <- which(distances == min(distances))
      x <- sample(ids, 1)
      return(x + (x >= a))
    }
    
    nn_index_X[ties] = sapply(ties, helper_ties)
  }
  
  # compute the nearest neighbor of W
  nn_W = RANN::nn2(W, query = W, k = 3)
  nn_index_W = nn_W$nn.idx[, 2]
  repeat_data = which(nn_W$nn.dists[, 2] == 0)
  
  df_W = data.table::data.table(id = repeat_data, group = nn_W$nn.idx[repeat_data])
  df_W[, rnn := .randomNN(id), by = "group"]
  
  nn_index_W[repeat_data] = df_W$rnn
  # nearest neighbors with ties
  ties = which(nn_W$nn.dists[, 2] == nn_W$nn.dists[, 3])
  ties = setdiff(ties, repeat_data)
  
  if(length(ties) > 0) {
    helper_ties <- function(a) {
      distances <- proxy::dist(matrix(X[a, ], ncol = ncol(X)), matrix(X[-a, ], ncol = ncol(X)))
      ids <- which(distances == min(distances))
      x <- sample(ids, 1)
      return(x + (x >= a))
    }
    
    nn_index_W[ties] = sapply(ties, helper_ties)
  }
  
  # estimate Q
  R_Y = rank(Y, ties.method = "max")
  Q_n = sum(apply(rbind(R_Y, R_Y[nn_index_W]), 2, min),
            -apply(rbind(R_Y, R_Y[nn_index_X]), 2, min)) / (n^2)
  return(Q_n)
}




# .estimateConditionalS -------------------------------------------------------------------------
# Estimate S(Y, X)
#
# Estimate S(Y, X), the denuminator of the measure of dependence of Y on Z given X
#
# @param X: Matrix of predictors (n by p)
# @param Y: Vector (length n)
#
# @return estimation \eqn{S(Y, X)}
.estimateConditionalS <- function (Y, X){
  
  id <- group <- rnn <- NULL
  
  if(!is.matrix(X)) {
    X = as.matrix(X)
  }
  n = length(Y)
  
  # compute the nearest neighbor of X
  nn_X = RANN::nn2(X, query = X, k = 3)
  nn_index_X = nn_X$nn.idx[, 2]
  repeat_data = which(nn_X$nn.dists[, 2] == 0)
  
  df_X = data.table::data.table(id = repeat_data, group = nn_X$nn.idx[repeat_data, 1])
  df_X[, rnn := .randomNN(id), by = "group"]
  
  nn_index_X[repeat_data] = df_X$rnn
  # nearest neighbors with ties
  ties = which(nn_X$nn.dists[, 2] == nn_X$nn.dists[, 3])
  ties = setdiff(ties, repeat_data)
  
  if(length(ties) > 0) {
    helper_ties <- function(a) {
      distances <- proxy::dist(matrix(X[a, ], ncol = ncol(X)), matrix(X[-a, ], ncol = ncol(X)))
      ids <- which(distances == min(distances))
      x <- sample(ids, 1)
      return(x + (x >= a))
    }
    
    nn_index_X[ties] = sapply(ties, helper_ties)
  }
  
  # estimate S
  R_Y = rank(Y, ties.method = "max")
  S_n = sum(R_Y - apply(rbind(R_Y, R_Y[nn_index_X]), 2, min)) / (n^2)
  
  return(S_n)
}


# estimateConditionalT -------------------------------------------------------------------------
# Estimate T(Y, Z | X)
#
# Estimate T(Y, Z | X), the measure of dependence of Y on Z given X
#
# @param Y: Vector (length n)
# @param Z: Matrix of predictors (n by q)
# @param X: Matrix of predictors (n by p)
#
# @return estimation of \eqn{T(Y, Z|X)}.
.estimateConditionalT <- function(Y, Z, X){
  S = .estimateConditionalS(Y, X)
  
  # happens only if Y is constant
  if (S == 0) {
    return(1)
  } else {
    return(.estimateConditionalQ(Y, X, Z) / S)
  }
}




# .estimateQ -------------------------------------------------------------------------
# Estimate Q(Y, X)
#
# Estimate Q(Y, X), the numinator of the measure of dependence of Y on X
#
# @param X: Matrix of predictors (n by p).
# @param Y: Vector (length n).
#
# @return estimation of \eqn{Q(Y, X)}.
.estimateQ <- function(Y, X) {
  
  id <- group <- rnn <- NULL
  
  if(!is.matrix(X)) {
    X = as.matrix(X)
  }
  
  n = length(Y)
  nn_X = RANN::nn2(X, query = X, k = 3)
  # remove the first nearest neighbor for each x which is x itself in case of no repeat data
  # when there is repeated data this is wrong but for repeated data we find the nearest
  # neighbors separately.
  nn_index_X = nn_X$nn.idx[, 2]
  
  # find all data points that are not unique
  repeat_data = which(nn_X$nn.dists[, 2] == 0)
  
  # for the repeated data points, choose one of their identicals at random and set its index
  # as the index of the nearest neighbor
  df_X = data.table::data.table(id = repeat_data, group = nn_X$nn.idx[repeat_data, 1])
  df_X[, rnn := .randomNN(id), by = "group"]
  nn_index_X[repeat_data] = df_X$rnn
  
  # nearest neighbors with ties
  ties = which(nn_X$nn.dists[, 2] == nn_X$nn.dists[, 3])
  ties = setdiff(ties, repeat_data)
  if(length(ties) > 0) {
    helper_ties <- function(a) {
      distances <- proxy::dist(matrix(X[a, ], ncol = ncol(X)), matrix(X[-a, ], ncol = ncol(X)))
      ids <- which(distances == min(distances))
      x <- sample(ids, 1)
      return(x + (x >= a))
    }
    
    nn_index_X[ties] = sapply(ties, helper_ties)
  }
  
  
  # estimate Q
  R_Y = rank(Y, ties.method = "max")
  L_Y = rank(-Y, ties.method = "max")
  Q_n = sum(apply(rbind(R_Y, R_Y[nn_index_X]), 2, min) - (L_Y^2)/n) / (n^2)
  
  return(Q_n)
}



# .estimateS -------------------------------------------------------------------------
# Estimate S(Y)
#
# Estimate S(Y) , the denuminator of the measure of dependence of Y on X
#
# @param Y: Vector (length n).
# @return estimation of \eqn{S(Y)}.
.estimateS <- function (Y) {
  n = length(Y)
  L_Y = rank(-Y, ties.method = "max")
  S_n = gmp::asNumeric(sum(gmp::as.bigz(L_Y) * gmp::as.bigz(n - L_Y))) / (n^3)
  return(S_n)
}



# .estimateT -------------------------------------------------------------------------
# Estimate T(Y, X)
#
# Estimate T(Y, X), the measure of dependence of Y on X
#
# @param X: Matrix of predictors (n by p)
# @param Y: Vector (length n)
# @return estimation of \eqn{T(Y, X) = Q(Y, X) / S(Y)}.
.estimateT <- function(Y, X){
  # May 15, Mona removed FOCI:::
  S = .estimateS(Y)
  # happens only if Y is a constant vector.
  if (S == 0) {
    return(1)
  } else {
    return(.estimateQ(Y, X) / S)
  }
}

.randomNN <- function(ids) {
  m <- length(ids)
  
  x <- sample(x = (m - 1), m, replace = TRUE)
  x <- x + (x >= (1:m))
  
  return(ids[x])
}


foci_new <- function(Y, X, XPCA, real=FALSE, num_hidden=NULL, num_features = NULL, stop = TRUE, numCores = parallel::detectCores()){
  
  namesX <- colnames(X)
  if (is.null(num_features)) num_features = dim(X)[2]
  n = length(Y)
  p = ncol(X)
  Q = rep(0, num_features)
  index_select = rep(0, num_features)
  if (typeof(real) == "logical"){
    Z_est <- XPCA$x[1:n, 1:num_hidden]
  } else if (typeof(real) == "double"){
    Z_est <- real
  } else {
    print("Invalid type!")
  }
  # select the first variable
  if (is.null(dim(X))) {
    seq_Q = .estimateConditionalQ(Y, Z_est, X)
  } else {
    estimateQFixedY <- function(id){
      return(.estimateConditionalQ(Y, Z_est, X[, id]))
    } # Use cbind(Z_est, X[, id]) instead of ConditionalQ (I cannot)
    seq_Q = parallel::mclapply(seq(1, p), estimateQFixedY, mc.cores = numCores)
    seq_Q = unlist(seq_Q)
  }
  
  Q[1] = max(seq_Q)
  if (Q[1] <= 0 & stop == TRUE) return(NULL)
  index_max = min(which(seq_Q == Q[1]))
  index_select[1] = index_max
  count = 1
  
  # select rest of the variables
  while (count < num_features) {
    seq_Q = rep(0, p - count)
    # indices that have not been selected yet
    index_left = setdiff(seq(1, p), index_select[1:count])
    
    # find the next best feature
    estimateQFixedYandSubX <- function(id){
      return(.estimateConditionalQ(Y, Z_est,
                                   cbind(X[, c(index_select[1:count], id)])))
    }
    
    if (length(index_left) == 1) {
      seq_Q = estimateQFixedYandSubX(index_left[1])
    } else {
      seq_Q = parallel::mclapply(index_left, estimateQFixedYandSubX, mc.cores = numCores)
      seq_Q = unlist(seq_Q)
    }
    Q[count + 1] = max(seq_Q)
    index_max = min(which(seq_Q == Q[count + 1]))
    if (Q[count + 1] <= Q[count] & stop == TRUE) break
    index_select[count + 1] = index_left[index_max]
    count = count + 1
  }
  
  selectedVar = data.table::data.table(index = index_select[1:count], names = namesX[index_select[1:count]])
  stepT = Q / .estimateConditionalS(Y, Z_est)
  result = list(selectedVar = selectedVar, stepT = stepT[1:count])
  result
}
