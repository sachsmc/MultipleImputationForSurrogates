
get.R.hat <- function(miceout){
  
  n <- dim(miceout$chainMean)[2]
  m <- dim(miceout$chainMean)[3]
  
  W <- colMeans(apply(miceout$chainMean, MARGIN = 1, FUN = function(mat){
    apply(mat, MARGIN = 2, FUN = var)
    }))
  
  B <- apply(apply(miceout$chainMean, MARGIN = 1, FUN = function(mat){
    apply(mat, MARGIN = 2, FUN = mean)
  }), MARGIN = 2, FUN = var)

  R.hat <- sqrt(1 + B/W)
  
  return(R.hat)
  
}


impute.data <- function(trial.data, iterations = 20){
  require(mice)

  ## now impute the outcome
  
  if("D" %in% colnames(trial.data)){  # time to event
  
  trial.temp <- within(trial.data[,c("Z", "S.0", "S.1", "W", "D.1", "D.0","Y.1", "Y.0", "Y", "D")], {
                         Y.1 <- log(Y.1)
                         Y.0 <- log(Y.0)
                         D.0 <- as.factor(D.0)
                         D.1 <- as.factor(D.1)
  })
    
    predMat <- 1 - diag(10)
    predMat[,c(1,9,10)] <- 0
    
    iter1.mice <- mice(trial.temp, 
                       m = 5, print = F, maxit = iterations,
                       method = c("", "norm", "norm", "", "logreg", "logreg", "norm", "norm", "", ""), 
                       predictorMatrix = predMat)
    
    
  }  else{
    
    trial.data$Y.1 <- as.factor(trial.data$Y.1)
    trial.data$Y.0 <- as.factor(trial.data$Y.0)
    iter1.mice <- mice(trial.data[,c("Z", "S.0", "S.1", "W", "Y.1", "Y.0", "Y")], 
                       m = 5, print = FALSE, maxit = iterations,
                       method = c("", "norm", "norm", "", "logreg", "logreg", ""), 
                       predictorMatrix = 1 - diag(7))
    
    
  }
  return(iter1.mice)
}

pool.sandwich <- function(mids){
  require(sandwich)
  m <- length(mids$analyses)
  k <- length(mids$analyses[[1]]$coeff)
  names <- names(mids$analyses[[1]]$coeff)
  qhat <- matrix(NA, nrow = m, ncol = k, dimnames = list(1:m, 
        names))
  u <- array(NA, dim = c(m, k, k), dimnames = list(1:m, names, 
        names))
  
  for(i in 1:m){
    fit <- mids$analyses[[i]]
    
  qhat[i, ] <- coef(fit)
  ui <- sandwich(fit)
  u[i, , ] <- ui
  
  }
  
  qbar <- apply(qhat, 2, mean)
    ubar <- apply(u, c(2, 3), mean)
    e <- qhat - matrix(qbar, nrow = m, ncol = k, byrow = TRUE)
    b <- (t(e) %*% e)/(m - 1)
    t <- ubar + (1 + 1/m) * b
    r <- (1 + 1/m) * diag(b/ubar)
    lambda <- (1 + 1/m) * diag(b/t)
    dfcom <- df.residual(mids$analyses[[1]])
    df <- mice:::mice.df(m, lambda, dfcom, "smallsample")
    fmi <- (r + 2/(df + 3))/(r + 1)
    names(r) <- names(df) <- names(fmi) <- names(lambda) <- names
    fit <- list(call = mids$call, call1 = mids$analyses[[1]]$call, call2 = mids$call1, 
        nmis = mids$nmis, m = m, qhat = qhat, u = u, qbar = qbar, 
        ubar = ubar, b = b, t = t, r = r, dfcom = dfcom, df = df, 
        fmi = fmi, lambda = lambda)
    oldClass(fit) <- c("mipo", oldClass(mids))
    return(fit)
  
}

analyze.imputed.data <- function(miceout){
  
  if("D" %in% names(miceout$nmis)){
      require(survival)
      
  fit.curve <- with(miceout, survreg(Surv(Y, D) ~ Z*S.1, dist = "exponential"))
  fit.surface <- with(miceout, survreg(Surv(Y, D) ~ Z*(S.0 + S.1), dist = "exponential"))
 
      } else{
        
  fit.curve <- with(miceout, glm(Y ~ Z*S.1, family = binomial(link = "probit")))
  fit.surface <- with(miceout, glm(Y ~ Z*(S.0 + S.1), family = binomial(link = "probit")))
 
      }
  
  pool.curve <- pool.sandwich(fit.curve)
  pool.surface <- pool.sandwich(fit.surface)
  
  return(list(pool.curve = pool.curve, pool.surface = pool.surface))
  
}






