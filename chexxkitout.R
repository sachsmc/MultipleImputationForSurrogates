scenVect <- data.frame(
  "tte" ,0.6,1,1,1,0,2,2,0.2,4000,0,0,0.5,2.75,0,0,0,0.3,0,0.5,FALSE,FALSE,FALSE,10,"S0NO_S1high_tte_nothing_right" ,
  stringsAsFactors = FALSE)

scenVect <- data.frame(
   "binary",0.6,1,1,1,0,2,2,0.2,4000,.06,1.26,0,0,-0.25,-1,0,0,0.8,0.3,0.8,0.5,TRUE,FALSE,FALSE,10,TRUE,"logreg", "S0NO_S1high_binary_BIP0.80.2_right" ,
   stringsAsFactors = FALSE)

scenVect <- data.frame(
  "tte" ,0.6,1,1,1,0,2,2,0.2,4000,0.5,2.75,0.5,2.75,0,0,0.2,0.3,0.5,0.5,TRUE,FALSE,FALSE,10,"S0high_S1high_tte_BIP0.20.8_right",  stringsAsFactors = FALSE)

outcomeType <- scenVect[1,1]
samp.args <- list(TE = scenVect[1,2], sigma = unlist(scenVect[1,3:5]), mu = unlist(scenVect[1,6:8]), 
                  inc.placebo = scenVect[1,9], nnn = scenVect[1,10], alpha.0 = scenVect[1,11], alpha.1 = scenVect[1,12], 
                  beta.S.0.0 = scenVect[1,13], 
                  beta.S.0.1 = scenVect[1,14], beta.S.1.0 = scenVect[1,15], beta.S.1.1 = scenVect[1,16],
                  beta.W.0 = scenVect[1,17], beta.W.1 = scenVect[1,18], rhos1W = scenVect[1,19], 
                  rhos1s0 = scenVect[1,20], rhos0W = scenVect[1,21])
options <- as.list(scenVect[1,20:24])
name<-scenVect[1,25]
names(options) = c("prob.trt", "BIP", "BSM", "CPV", "iterations")

mice.impute.norm2 <- 
function (y, ry, x, ...) 
{
  x <- cbind(1, as.matrix(x))
  parm <- my.norm.draw(y, ry, x, ...)
  return(x[!ry, ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
}

my.norm.draw <- 
function (y, ry, x, ridge = 0, ...) 
{
  xobs <- x[ry, ]
  yobs <- y[ry]
  xtx <- t(xobs) %*% xobs
  pen <- ridge * diag(xtx)
  if (length(pen) == 1) 
    pen <- matrix(pen)
  v <- solve(xtx + diag(pen))
  coef <- t(yobs %*% xobs %*% v)
  residuals <- yobs - xobs %*% coef
  df <- max(sum(ry) - ncol(x), 1)
  sigma.star <- sqrt(sum((residuals)^2)/df/rchisq(1, df))
  beta.star <- coef + (t(chol((v + t(v))/2)) %*% rnorm(ncol(x))) * 
    sigma.star
  parm <- list(coef, beta.star, sigma.star)
  names(parm) <- c("coef", "beta", "sigma")
  return(parm)
}

source("meta-analysis-generation_new.R")
source("mice_test.R")

run.one <- function(a){
  require(mice)
  sample0 <- samp.data.tte(.6, sigma = c(1, 1, 1), mu = c(0,2,2), 
                               .2, 4000, .06, 1.26, beta.S.0.0=0, beta.S.0.1=0, 
                               beta.S.1.0 = -.25, beta.S.1.1=-1, beta.W.0=0, beta.W.1=0, beta.X = .1,
                               rhos1W=.5, rhos1s0=.3, rhos0W=.5)
    
  trial.data <- get.trial.data(sample0, .5, BIP = TRUE, BSM = TRUE, CPV = F)

  #trial.data$Y.0 <- as.factor(trial.data$Y.0)
  #trial.data$Y.1 <- as.factor(trial.data$Y.1)
  
  predmat <- 1 - diag(dim(trial.data)[2])
  #predmat[,c(1, 2, 4)] <- 0
  
  #micetest <- mice(trial.data, predictorMatrix = predmat, print = F, maxit = 30, m = 5,
  #                 method = c("", "", "", "", "norm", "norm", "probit", "probit"))
  predmat[,c(1, 2, 5, 6)] <- 0
  predmat[11, c(8, 12)] <- 0
  predmat[12, c(7, 11)] <- 0
  micetest <- mice(trial.data, predictorMatrix= predmat, print = F, maxit = 15, ridge = 0,
                   method = c(rep("", 6), "pmm", "pmm", "norm", "norm", "survweibull", "survweibull"))
  pool(with(micetest, survreg(Surv(Y, D)~ Z*S.1 + X, dist = "exponential")))$qbar
  #pool(with(micetest, glm(Y ~ Z*S.1 , family = binomial(link = "probit"))))$qbar
}

sample0 <- samp.data.tte(.6, sigma = c(1, 1, 1), mu = c(0,2,2), 
                            .2, 4000, .06, 1.26, beta.S.0.0=0, beta.S.0.1=0, 
                            beta.S.1.0 = -.25, beta.S.1.1=-1, beta.W.0=0, beta.W.1=0, beta.X = .1,
                            rhos1W=.5, rhos1s0=.3, rhos0W=.5)


troof <- sample0$output.betas[c(1, 2, 4, 9, 7)]
checkme <- as.data.frame(t(sapply(1:50, run.one)))

#troof

100*(colMeans(checkme) - troof)/sqrt(diag(cov(checkme)))

scen8 <- read.csv("~/Downloads/S0NO_S1high_binary_BIP0.80.2_rightWed Oct  9 14:11:10 2013_coeffcients_curve.csv")
scen8 <- read.csv("~/Downloads/S0NO_S1high_tte_nothing_rightThu Oct 10 01:25:30 2013_coeffcients_curve.csv")
colMeans(scen8)
sample0$output.betas

with(samp.args, -1*c(beta.S.0.1 ,
                 beta.S.1.1 - beta.S.0.1,
                 beta.S.0.0,
                 beta.S.1.0 - beta.S.0.0))

