
samp.data.binary <- function(TE, sigma = c(1, 1, 1), mu = c(0,2,2), 
                             inc.placebo, nnn, alpha.0, alpha.1, beta.S.0.0=0, beta.S.0.1=0, 
                             beta.S.1.0, beta.S.1.1, beta.W.0=0, beta.W.1=0, beta.X = .1,
                             rhos1W, rhos1s0, rhos0W){
  require(MASS)
  Sigma <- matrix(
                  c(sigma[1]^2, rhos1s0*sigma[1]*sigma[2], rhos0W*sigma[1]*sigma[3], 
                    rhos1s0*sigma[1]*sigma[2],sigma[2]^2, rhos1W*sigma[3]*sigma[2], 
                    rhos0W*sigma[1]*sigma[3], rhos1W*sigma[3]*sigma[2], sigma[3]^2), 
                  nrow = 3)  ## [S(0), S(1), W]
  XXX <- mvrnorm(nnn, mu = mu, Sigma)  
  
  ## determining alpha0 and alpha1
  
#  inc.vaccine <- (1 - TE)*inc.placebo
  
#  find.XXX <- mvrnorm(4000, mu = mu, Sigma)
#  find.Y.0 <- function(alpha.0) mean(pnorm(alpha.0 + beta.S.0.0*find.XXX[,1] + beta.S.1.0*find.XXX[,2] + beta.W.0*find.XXX[,3])) - inc.placebo
#  find.Y.1 <- function(alpha.1) mean(pnorm(alpha.1 + beta.S.0.1*find.XXX[,1] + beta.S.1.1*find.XXX[,2] + beta.W.1*find.XXX[,3])) - inc.vaccine
  
#  alpha.0 <- uniroot(find.Y.0, interval = c(-100, 100))$root
#  alpha.1 <- uniroot(find.Y.1, interval = c(-100, 100))$root
  
  p.Y.0 <- pnorm(alpha.0 + beta.S.0.0*XXX[,1] + beta.S.1.0*XXX[,2] + beta.W.0*XXX[,3] + beta.X*X)
  p.Y.1 <- pnorm(alpha.1 + beta.S.0.1*XXX[,1] + beta.S.1.1*XXX[,2] + beta.W.1*XXX[,3] + beta.X*X)

## generate Ys

  Y.0 <- unlist(lapply(p.Y.0, function(p) rbinom(1,1,p)))
  Y.1 <- unlist(lapply(p.Y.1, function(p) rbinom(1,1,p)))
  
  output.betas <- c(alpha.0, alpha.1 - alpha.0, beta.S.0.0, beta.S.1.0, beta.W.0, beta.S.0.1 - beta.S.0.0, 
                    beta.S.1.1 - beta.S.1.0, beta.W.1 - beta.W.0)
  names(output.betas) <- c("alpha", "gamma.Z", "gamma.0.0", "gamma.0.1", "gamma.0.2", "gamma.1.0", "gamma.1.1", "gamma.1.2")
  
  
  dat.out <- (list(sample = data.frame(Y.0 = Y.0, Y.1 = Y.1, S.0 = XXX[,1], S.1 = XXX[,2], W = XXX[,3], X = X), output.betas = output.betas))
  class(dat.out) <- c("binary", "raw.data")
  return(dat.out)

}


samp.data.tte <- function(TE, sigma = c(1, 1, 1), mu = c(0,2,2), 
                             inc.placebo, nnn, alpha.0, alpha.1, beta.S.0.0=0, beta.S.0.1=0, 
                             beta.S.1.0, beta.S.1.1, beta.W.0=0, beta.W.1=0, beta.X = .1, 
                             rhos1W, rhos1s0, rhos0W){
  require(MASS)
  require(survival)
  Sigma <- matrix(
    c(sigma[1]^2, rhos1s0*sigma[1]*sigma[2], rhos0W*sigma[1]*sigma[3], 
      rhos1s0*sigma[1]*sigma[2],sigma[2]^2, rhos1W*sigma[3]*sigma[2], 
      rhos0W*sigma[1]*sigma[3], rhos1W*sigma[3]*sigma[2], sigma[3]^2), 
    nrow = 3)  ## [S(0), S(1), W]
  XXX <- mvrnorm(nnn, mu = mu, Sigma)  
  
  ## determining alpha0 and alpha1
  
  inc.vaccine <- (1 - TE)*inc.placebo  #incidence is incidence at 9 (time is in days) years (end of DCCT Trial)
  
#  find.XXX <- mvrnorm(4000, mu = mu, Sigma)
  
#  find.Y.0 <- function(alpha.0) mean((1 - exp(-9*exp(alpha.0 + beta.S.0.0*find.XXX[,1] + beta.S.1.0*find.XXX[,2] + beta.W.0*find.XXX[,3]))) - inc.placebo)
  
#  find.Y.1 <- function(alpha.1) mean((1 - exp(-9*exp(alpha.1 + beta.S.0.1*find.XXX[,1] + beta.S.1.1*find.XXX[,2] + beta.W.1*find.XXX[,3]))) - inc.vaccine)
  
#  alpha.0 <- uniroot(find.Y.0, interval = c(-1e12, 10000000))$root
#  alpha.1 <- uniroot(find.Y.1, interval = c(-1e12, 10000000))$root
  
  X <- rnorm(nnn)
  
  ## generate Ys
  
  Y.0 <- log(1 - runif(nnn))/(-exp(alpha.0 + beta.S.0.0*XXX[,1] + beta.S.1.0*XXX[,2] + beta.W.0*XXX[,3] + beta.X*X))
  Y.1 <- log(1 - runif(nnn))/(-exp(alpha.1 + beta.S.0.1*XXX[,1] + beta.S.1.1*XXX[,2] + beta.W.1*XXX[,3] + beta.X*X))
  
  output.betas <- -1*c(alpha.0, alpha.1 - alpha.0, beta.S.0.0, 
                       beta.S.1.0, beta.W.0, beta.S.0.1 - beta.S.0.0, 
                       beta.S.1.1 - beta.S.1.0, beta.W.1 - beta.W.0, beta.X)
  names(output.betas) <- c("alpha", "gamma.Z", "gamma.0.0", "gamma.0.1", "gamma.0.2", "gamma.1.0", "gamma.1.1", "gamma.1.2", "gamma.X")
  
  dat.out <- (list(sample = data.frame(Y.0 = Y.0, Y.1 = Y.1, S.0 = XXX[,1], S.1 = XXX[,2], W = XXX[,3], X = X), 
                   output.betas = output.betas))
  class(dat.out) <- c("time", "raw.data")
  return(dat.out)
  
}



get.trial.data <- function(raw.data.class, prob.trt = .5, BIP = FALSE, BSM = FALSE, CPV = FALSE){
  raw.data <- raw.data.class$sample
  
  ZZZ <- rbinom( dim(raw.data)[1], 1, prob.trt)
  YYY <- ifelse(ZZZ==1, raw.data$Y.1, raw.data$Y.0)
  SSS <- ifelse(ZZZ==1, raw.data$S.1, raw.data$S.0)
  
  ## switch for augmented trial design: none, cpv, bsm, bip or any combination thereof
    
  dat.out <- data.frame(S = SSS, Z = ZZZ, W = raw.data$W, X = raw.data$X, Y = YYY)
  if("time" %in% class(raw.data.class)){  ## add censoring time
    D <- as.numeric(YYY < 7.5)
    dat.out <- cbind(dat.out, D)  
    dat.out$D.0 <- dat.out$D.1 <- dat.out$D
    dat.out$D.0[dat.out$Z == 1] <- NA
    dat.out$D.1[dat.out$Z == 0] <- NA
    dat.out$Y[D==0] <- 7.5
  }
  
  
  dat.out <- within(dat.out, {
                    Y.0 <- Y
                    Y.0[Z == 1] <- NA
                    Y.1 <- Y
                    Y.1[Z == 0] <- NA
                    S.0 <- S
                    S.0[Z == 1] <- NA
                    S.1 <- S
                    S.1[Z == 0] <- NA
                    })
  
  
  if(!BIP){
    dat.out$W <-NA
  }
  if(BSM){
    dat.out$S.0[dat.out$Z == 1] <- raw.data$S.0[ZZZ == 1]
  }
  if(CPV){
    if("D" %in% colnames(dat.out)){
    dat.out$S.1[with(dat.out, Z == 0 & D == 0)] <- raw.data$S.1[with(dat.out, Z == 0 & D == 0)]
  } else{
    dat.out$S.1[with(dat.out, Z == 0 & Y == 0)] <- raw.data$S.1[with(dat.out, Z == 0 & Y == 0)]
  }}
    
  return(dat.out)
  
}



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

mice.impute.survreg <- function(y, ry, x, ...){
  require(MASS)
  require(survival)
  y.name <- paste("Y", strsplit(grep("D", colnames(x), value = TRUE), ".", fixed = T)[[1]][2], sep= ".")
  dat.temp <- data.frame(y, x)
  colnames(dat.temp)[1] <- y.name
  form <- paste("Surv(", y.name, ",", gsub("Y", "D", y.name), ") ~ S.0 + S.1")
  if("W" %in% colnames(x)){ form <- paste(form, " + W") }
  fit.y <- survreg(as.formula(form), data = dat.temp[ry,], dist ="exponential")
  
  beta.star <- mvrnorm(1, mu = fit.y$coeff, Sigma = vcov(fit.y))
  fit.y$coefficients <- beta.star
  lambda <- predict(fit.y, newdata = dat.temp[!ry,], type = "lp")
  return( log(1 - runif(length(lambda)))/(-exp(lambda)) )
  
}

mice.impute.probit <- function (y, ry, x, ...) 
{
  aug <- mice:::augment(y, ry, x, ...)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  w <- aug$w
  x <- cbind(1, as.matrix(x))
  expr <- expression(glm.fit(x[ry, ], y[ry], family = binomial(link = probit), 
                             weights = w[ry]))
  fit <- suppressWarnings(eval(expr))
  fit.sum <- summary.glm(fit)
  beta <- coef(fit)
  rv <- t(chol(fit.sum$cov.unscaled))
  beta.star <- beta + rv %*% rnorm(ncol(rv))
  p <- pnorm(x[!ry, ] %*% beta.star)
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  if (is.factor(y)) {
    vec <- factor(vec, c(0, 1), levels(y))
  }
  return(vec)
}

mice.impute.survweibull <- function(y, ry, x, ...){
  require(MASS)
  require(survival)
  y.name <- paste("Y", strsplit(grep("D", colnames(x), value = TRUE), ".", fixed = T)[[1]][2], sep= ".")
  dat.temp <- data.frame(y, x)
  colnames(dat.temp)[1] <- y.name
  form <- paste("Surv(", y.name, ",", gsub("Y", "D", y.name), ") ~ S.0 + S.1")
  if("W" %in% colnames(x)){ form <- paste(form, " + W") }
  fit.y <- survreg(as.formula(form), data = dat.temp[ry,], dist ="weibull")
  num<-length(fit.y$coeff)+1
  stars.out <- mvrnorm(1, mu = c(fit.y$coeff,log(fit.y$scale)), Sigma = vcov(fit.y))
  scale.star <- stars.out[num]
  beta.star<-stars.out[-num]
  fit.y$coefficients <- beta.star
  fit.y$scale <-exp(scale.star)
  lambda <- predict(fit.y, newdata = dat.temp[!ry,], type = "lp")
  
  return(rweibull(length(lambda), shape=(1/fit.y$scale), scale=exp(pmin(3, lambda))))
  
}
