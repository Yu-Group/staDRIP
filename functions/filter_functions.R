library(glmnet)

filterByVar <- function(dat, min.var, max.p) {
  ## filterByVar: function to filter data by largest variance
  ##
  ## Inputs: (must specify either min.var or max.p)
  ##  -dat = n x p data matrix
  ##  -min.var = variance cutoff
  ##  -max.p = maximum number of features to keep
  ##
  ## Output:
  ##  -fdat = filtered n x new_p data matrix
  
  vars <- apply(dat, 2, var, na.rm = T)
  
  if (!missing(min.var)) {
    fdat <- dat[,vars >= min.var]
  }else if (!missing(max.p)) {
    var.cutoff <- sort(vars, decreasing = T)[max.p]
    fdat <- dat[,vars >= var.cutoff]
  }else {
    stop("Must input either min.var or max.p.")
  }
  
  return(fdat)
  
}


filterByCor <- function(dat, y, min.cor, max.p) {
  ## filterByCor: function to filter data by highest correlation associated w/ y
  ##
  ## Inputs: (must specify either min.cor or max.p)
  ##  -dat = n x p data matrix
  ##  -y = response vector
  ##  -min.cor = correlation cutoff
  ##  -max.p = maximum number of features to keep
  ##
  ## Output:
  ##  -fdat = filtered n x new_p data matrix

  cors <- abs(apply(dat, 2, cor, y = y, use = "pairwise.complete.obs"))

  if (!missing(min.cor)) {
    fdat <- dat[, cors >= min.cor]
  } else if (!missing(max.p)) {
    cor.cutoff <- sort(cors, decreasing = T)[max.p]
    fdat <- dat[, cors >= cor.cutoff]
  } else {
    stop("Must input either min.cor or max.p.")
  }

  return(fdat)
}


filterByLasso <- function(dat, y, family = "gaussian", max.p) {
  ## filterByLasso: function to filter data by highest feature importance using Lasso
  ## 
  ## Inputs: 
  ##  -dat = n x p data matrix
  ##  -y = response vector
  ##  -max.p = maximum number of features to keep
  ##
  ## Output:
  ##  - fdat = filtered n x max.p data matrix
  
  lasso.fit <- glmnet(x = dat, y = y, family = family, alpha = 1)
  lambdas <- lasso.fit$lambda
  
  ptr <- T
  while (ptr) {
    lasso.fit <- glmnet(x = dat, y = y, family = family, alpha = 1, lambda = lambdas)
    if (length(which(lasso.fit$df > max.p)) >= 1) {
      ptr <- F
      idx <- which(lasso.fit$df > max.p)[1]
      keep.feat <- names(sort(abs(lasso.fit$beta[, idx]), decreasing = T))[1:max.p]
    } else {
      lambdas <- lasso.fit$lambda * 1e-1
    }
  }
  
  return(dat[, keep.feat])
}


removeZeroVarCols <- function(data) {
  ## removeZeroVarCols: function to clean data by removing columns with 
  ##    zero variance
  ##
  ## Input: data = data matrix or data frame
  ##
  ## Output: data_cleaned = data matrix without the constant columns
  ##
  ## Usage: removeZeroVarCols(data = cbind(1:5, rep(1, 5)))
  
  zero_vars <- which(apply(X = data, MARGIN = 2, FUN = var, na.rm = T) == 0)
  
  if (length(zero_vars) >= 1) {
    data_cleaned <- data[, -zero_vars]
  } else {
    data_cleaned <- data
  }
  
  return(data_cleaned)
}


