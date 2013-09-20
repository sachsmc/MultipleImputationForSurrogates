
samp.data.binary <- function(TE, sigma = c(1, 1, 1), mu = c(0,2,2), 
                             inc.placebo, nnn, beta.S.0.0=0, beta.S.0.1=0, 
                             beta.S.1.0, beta.S.1.1, beta.W.0=0, beta.W.1=0, 
                             rhos1W, rhos1s0, rhos0W){
  require(MASS)
  Sigma <- matrix(
                  c(sigma[1]^2, rhos1s0*sigma[1]*sigma[2], rhos0W*sigma[1]*sigma[3], 
                    rhos1s0*sigma[1]*sigma[2],sigma[2]^2, rhos1W*sigma[3]*sigma[2], 
                    rhos0W*sigma[1]*sigma[3], rhos1W*sigma[3]*sigma[2], sigma[3]^2), 
                  nrow = 3)  ## [S(0), S(1), W]
  XXX <- mvrnorm(nnn, mu = mu, Sigma)  
  
  ## determining alpha0 and alpha1
  
  inc.vaccine <- (1 - TE)*inc.placebo
  
  find.XXX <- mvrnorm(4000, mu = mu, Sigma)
  find.Y.0 <- function(alpha.0) mean(pnorm(alpha.0 + beta.S.0.0*find.XXX[,1] + beta.S.1.0*find.XXX[,2] + beta.W.0*find.XXX[,3])) - inc.placebo
  find.Y.1 <- function(alpha.1) mean(pnorm(alpha.1 + beta.S.0.1*find.XXX[,1] + beta.S.1.1*find.XXX[,2] + beta.W.1*find.XXX[,3])) - inc.vaccine
  
  alpha.0 <- uniroot(find.Y.0, interval = c(-100, 100))$root
  alpha.1 <- uniroot(find.Y.1, interval = c(-100, 100))$root
  
  p.Y.0 <- pnorm(alpha.0 + beta.S.0.0*XXX[,1] + beta.S.1.0*XXX[,2] + beta.W.0*XXX[,3])
  p.Y.1 <- pnorm(alpha.1 + beta.S.0.1*XXX[,1] + beta.S.1.1*XXX[,2] + beta.W.1*XXX[,3])

## generate Ys

  Y.0 <- unlist(lapply(p.Y.0, function(p) rbinom(1,1,p)))
  Y.1 <- unlist(lapply(p.Y.1, function(p) rbinom(1,1,p)))
  
  output.betas <- c(alpha.0, alpha.1 - alpha.0, beta.S.0.0, beta.S.1.0, beta.W.0, beta.S.0.1 - beta.S.0.0, beta.S.1.1 - beta.S.1.0, beta.W.1 - beta.W.0)
  names(output.betas) <- c("alpha", "gamma.Z", "gamma.0.0", "gamma.0.1", "gamma.0.2", "gamma.1.0", "gamma.1.1", "gamma.1.2")
  
  
  dat.out <- (list(sample = data.frame(Y.0 = Y.0, Y.1 = Y.1, S.0 = XXX[,1], S.1 = XXX[,2], W = XXX[,3]), output.betas = output.betas))
  class(dat.out) <- c("binary", "raw.data")
  return(dat.out)

}


samp.data.tte <- function(TE, sigma = c(1, 1, 1), mu = c(0,2,2), 
                             inc.placebo, nnn, beta.S.0.0=0, beta.S.0.1=0, 
                             beta.S.1.0, beta.S.1.1, beta.W.0=0, beta.W.1=0, 
                             rhos1W, rhos1s0, rhos0W){
  require(MASS)
  Sigma <- matrix(
    c(sigma[1]^2, rhos1s0*sigma[1]*sigma[2], rhos0W*sigma[1]*sigma[3], 
      rhos1s0*sigma[1]*sigma[2],sigma[2]^2, rhos1W*sigma[3]*sigma[2], 
      rhos0W*sigma[1]*sigma[3], rhos1W*sigma[3]*sigma[2], sigma[3]^2), 
    nrow = 3)  ## [S(0), S(1), W]
  XXX <- mvrnorm(nnn, mu = mu, Sigma)  
  
  ## determining alpha0 and alpha1
  
  inc.vaccine <- (1 - TE)*inc.placebo  #incidence is incidence at 9 (time is in days) years (end of DCCT Trial)
  
  find.XXX <- mvrnorm(4000, mu = mu, Sigma)
  
  find.Y.0 <- function(alpha.0) mean((1 - exp(-9*exp(alpha.0 + beta.S.0.0*find.XXX[,1] + beta.S.1.0*find.XXX[,2] + beta.W.0*find.XXX[,3]))) - inc.placebo)
  
  find.Y.1 <- function(alpha.1) mean((1 - exp(-9*exp(alpha.1 + beta.S.0.1*find.XXX[,1] + beta.S.1.1*find.XXX[,2] + beta.W.1*find.XXX[,3]))) - inc.vaccine)
  
  alpha.0 <- uniroot(find.Y.0, interval = c(-1e12, 10000000))$root
  alpha.1 <- uniroot(find.Y.1, interval = c(-1e12, 10000000))$root
  
  ## generate Ys
  
  Y.0 <- log(1 - runif(nnn))/(-exp(alpha.0 + beta.S.0.0*XXX[,1] + beta.S.1.0*XXX[,2] + beta.W.0*XXX[,3]))
  Y.1 <- log(1 - runif(nnn))/(-exp(alpha.1 + beta.S.0.1*XXX[,1] + beta.S.1.1*XXX[,2] + beta.W.1*XXX[,3]))
  
  output.betas <- c(alpha.0, alpha.1 - alpha.0, beta.S.0.0, beta.S.1.0, beta.W.0, beta.S.0.1 - beta.S.0.0, beta.S.1.1 - beta.S.1.0, beta.W.1 - beta.W.0)
  names(output.betas) <- c("alpha", "gamma.Z", "gamma.0.0", "gamma.0.1", "gamma.0.2", "gamma.1.0", "gamma.1.1", "gamma.1.2")
  
  dat.out <- (list(sample = data.frame(Y.0 = Y.0, Y.1 = Y.1, S.0 = XXX[,1], S.1 = XXX[,2], W = XXX[,3]), output.betas = output.betas))
  class(dat.out) <- c("time", "raw.data")
  return(dat.out)
  
}



get.trial.data <- function(raw.data.class, prob.trt = .5, BIP = FALSE, BSM = FALSE, CPV = FALSE){
  raw.data <- raw.data.class$sample
  
  ZZZ <- rbinom( dim(raw.data)[1], 1, prob.trt)
  YYY <- ifelse(ZZZ==1, raw.data$Y.1, raw.data$Y.0)
  SSS <- ifelse(ZZZ==1, raw.data$S.1, raw.data$S.0)
  
  ## switch for augmented trial design: none, cpv, bsm, bip or any combination thereof
  SSS[ZZZ==0] <- NA
  R<-ifelse(is.na(SSS), 0,1)
  
  dat.out <- data.frame(S = SSS, Z = ZZZ, W = raw.data$W, Y = YYY)
  if("time" %in% class(raw.data.class)){  ## add censoring time
    D <- as.numeric(YYY < 7.5)
    dat.out <- cbind(dat.out, D)  
    dat.out$D.0 <- dat.out$D.1 <- dat.out$D
    dat.out$D.0[dat.out$Z == 1] <- NA
    dat.out$D.1[dat.out$Z == 0] <- NA
  }
  
  if(!BIP){
    dat.out$W <-NULL
  }
  if(BSM){
    dat.out$BSM <- raw.data$S.0
  }
  if(CPV){
    dat.out$CPV <- raw.data$S.1
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
  
  return(dat.out)
  
}




