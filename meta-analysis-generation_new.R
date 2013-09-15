
samp.data.binary <- function(TE, sigma = c(1, 1, 1), mu = c(0,2,2), inc.placebo, nnn, beta.S.0.0=0, beta.S.0.1=0, beta.S.1.0, beta.S.1.1, beta.W.0=0, beta.W.1=0, rhos1W, rhos1s0, rhosoW){
  require(MASS)
  Sigma <- matrix(c(sigma[1]^2, rhos1s0*sigma[1]*sigma[2], rhosoW*sigma[1]*sigma[3], rhos1s0*sigma[1]*sigma[2],sigma[2]^2, 
                    rhos1W*sigma[3]*sigma[2], rhosoW*sigma[1]*sigma[3], rhos1W*sigma[3]*sigma[2], 
                    sigma[3]^2), nrow = 3)  ## [S(0), S(1), W]
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
  
  output.betas <- c(alpha.0, alpha.1 - alpha.0, beta.S.1.0, beta.W.0, beta.S.1.1 - beta.S.1.0, beta.W.1 - beta.W.0)
  names(output.betas) <- c("gamma.0.0", "gamma.1.0", "gamma.0.1", "gamma.0.2", "gamma.1.1", "gamma.1.2")
  
  return(list(sample = data.frame(Y.0 = Y.0, Y.1 = Y.1, S.0 = XXX[,1], S.1 = XXX[,2], W = XXX[,3]), output.betas = output.betas))

}

get.trial.data <- function(sample, prob.trt = .5, S.sampling = "simple 1"){
  ZZZ <- rbinom( dim(sample)[1], 1, prob.trt)
  YYY <- ifelse(ZZZ==1, sample$Y.1, sample$Y.0)
  Y.0<- ifelse(ZZZ==1, NA, sample$Y.0)
  Y.1<- ifelse(ZZZ==0, NA, sample$Y.1)
  SSS <- ifelse(ZZZ==1, sample$S.1, sample$S.0)
  
  ## switch for sampling of S simple random is only one currently implemented
  
  if(length(grep("simple", S.sampling)) > 0){
    
    prop.S <- as.numeric(gsub("simple ", "", S.sampling))
    missing.s <- sample(1:length(SSS), length(SSS)*(1-prop.S))  
    SSS[missing.s] <- NA
    
  }
  SSS[ZZZ==0] <- NA
  R<-ifelse(is.na(SSS), 0,1)
  
  return(data.frame(S = SSS, W = sample$W, Y = YYY, Z = ZZZ, S.0 = sample$S.0, S.1 = sample$S.1, Y.0=Y.0, Y.1=Y.1, R=R))
  
}

run.probit.model_data <- function(dat){

all.fits <- matrix(NA, nrow = length(imputed.list), ncol = 8)
  for(i in 1:length(imputed.list)){
   fit <- glm(Y ~ Z*S.1, data = dat, family = binomial(link = "probit")) 
    all.fits[i,] <- c(fit$coeff, (diag(vcov(fit))))
  }
 
 Qbar <- colMeans(all.fits[,1:4])
 Ubar <- colMeans(all.fits[,5:8])
  B <- diag(cov(all.fits[,1:4]))
  
  tot.se <- sqrt(Ubar + (1 + 1/length(imputed.list))*B)
  
    return(list(beta = Qbar, se.beta = tot.se))
}




