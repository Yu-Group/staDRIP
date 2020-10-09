
h <- function(g1, g2, r1, r2, s){
  if(r1 < r2){
    hp = 0.5*(1 + 2*sqrt(2)*(pnorm((g2 - g1)/(2*s)) - 0.5))
    #hp = pnorm((g1 - g2)/(sqrt(2)*s))
    #hp = (g1 < g2)
  }
  if(r1 == r2){
    hp = 0.5
  }
  if(r1 > r2){
    hp = 0.5*(1 + 2*sqrt(2)*(pnorm((g1 - g2)/(2*s)) - 0.5))
    #hp = pnorm((g2 - g1)/(sqrt(2)*s))
    #hp = (g1 > g2)
  }
  return(hp)
}

pc <- function(g, r, s){
  n = length(g)
  res = sapply(1:(n-1), 
               function(i){
                 sum(sapply((i+1):n, function(j) h(g[i], g[j], r[i], r[j], s)))
               } )
  return(2*sum(res)/(n*(n-1)))
}

pc.nulldist <- function(y){
  p = length(y)
  pc.null <- matrix(0, p, 2)
  for(i in 1:p) {
    yi = y[, i]
    yi = yi[!is.na(yi)]
    s = sd(yi)
    res = sapply(1:1000, function(i) pc(yi, sample(yi), s))
    pc.null[i, ] <- c(mean(res), sd(res))
  }
  return(pc.null)
}


wpc <- function(ypred, yvalid, pc.null){
  
  #ypred: predicted response
  #yvalid: true response
  #pc.null: mean and sd of null dist.
  
  p = ncol(ypred)
  pc.pred = rep(0, p)
  for(i in 1:p){
    if (all(is.na(ypred[, i]))) {
      pc.pred[i] <- NA
    }else {
      y = yvalid[, i]
      pred = ypred[, i]
      y.missing <- is.na(y)
      pred.missing <- is.na(pred)
      pred = pred[(!pred.missing) & (!y.missing)]
      y = y[(!y.missing) & (!pred.missing)]
      pc.pred[i] = pc(y, pred, sd(y))
    }
  }
  
  if (sum(is.na(pc.pred)) > 0) {
    return(NA)
  }else {
    wd = (pc.pred - pc.null[, 1])/pc.null[, 2]
    wpc.pred = sum(wd * pc.pred)/sum(wd)
    return(wpc.pred)
  }
}



pcs <- function(ypred, yvalid, pc.null){
  
  #ypred: predicted response
  #yvalid: true response
  #pc.null: mean and sd of null dist.
  
  p = ncol(ypred)
  pc.pred = rep(0, p)
  for(i in 1:p){
    if (all(is.na(ypred[, i]))) {
      pc.pred[i] <- NA
    }else {
      y = yvalid[, i]
      pred = ypred[, i]
      y.missing <- is.na(y)
      pred.missing <- is.na(pred)
      pred = pred[(!y.missing) & (!pred.missing)]
      y = y[(!y.missing) & (!pred.missing)]
      pc.pred[i] = pc(y, pred, sd(y))
    }
  }
  return(pc.pred)
}

