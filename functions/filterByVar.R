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
