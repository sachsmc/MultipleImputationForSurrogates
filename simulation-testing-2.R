
#defaults
#binary or tte  TE = .6; sigma = c(1, 1, 1); mu = c(0,2,2); inc.placebo = .2; 
#nnn=4000; beta.S.0.0=0; beta.S.0.1=0; beta.S.1.0 = 0; 
#beta.S.1.1 = 0; beta.W.0=0; beta.W.1=0; rhos1W=0.8; rhos1s0=0.3; rhosoW=0.8

#samp.args <- list(TE = .6, sigma = c(1,1,1), mu = c(0,2,2), inc.placebo = .2,
 #                 nnn = 4000, beta.S.0.0 = 0, beta.S.0.1 = 0, beta.S.1.0 = 0, 
  #                beta.S.1.1 = 0, beta.W.0 = 0, beta.W.1 = 0, rhos1W = .8, rhos1s0 =.3, rhos0W = .8)

#scenVect <- data.frame( "tte",0.6,1,1,1,0,2,2,0.2,4000,0.06, 1, -0.5,-2.5,0,0,0,0,0.2,0.5,0.2,0.5,TRUE,FALSE,FALSE,10,TRUE,"survreg", "S0high_S1NO_binary_BIP0.2BSM0.5CPV_right" , stringsAsFactors = FALSE)

run.many <- function(scenVect, reps){  ## scenVect needs to be mixed vector, ie, a data frame with 1 row
source("meta-analysis-generation_new.R")
source("mice_test.R")

  outcomeType <- scenVect[1,1]
  samp.args <- list(TE = scenVect[1,2], sigma = unlist(scenVect[1,3:5]), mu = unlist(scenVect[1,6:8]), 
                    inc.placebo = scenVect[1,9], nnn = scenVect[1,10], alpha.0 = scenVect[1,11], alpha.1 = scenVect[1,12], beta.S.0.0 = scenVect[1,13], 
                    beta.S.0.1 = scenVect[1,14], beta.S.1.0 = scenVect[1,15], beta.S.1.1 = scenVect[1,16],
                    beta.W.0 = scenVect[1,17], beta.W.1 = scenVect[1,18], rhos1W = scenVect[1,19], 
                    rhos1s0 = scenVect[1,20], rhos0W = scenVect[1,21])
  options <- as.list(scenVect[1,22:28])
  name<-paste(scenVect[1,29], options[6], options[7], sep = "-")
  names(options) = c("prob.trt", "BIP", "BSM", "CPV", "iterations", "stdMice", "logregpmm")



## outputs
runonwrapper<-function(number){
source("meta-analysis-generation_new.R")
source("mice_test.R")
set.seed(number)

runone <- function(outcomeType, samp.args, options){  
  require(mice)
  
  samp.func <- paste0("samp.data.", outcomeType)
  sample0 <- do.call(samp.func, samp.args)
  
  trial.data <- do.call("get.trial.data", 
                        list(raw.data.class = sample0, prob.trt = options$prob.trt, 
                             BIP = options$BIP, BSM = options$BSM, CPV = options$CPV))
  
  if(options$stdMice & !("D" %in% colnames(trial.data))){
    
    predmat <- 1 - diag(7)
    predmat[,c(1,3)] <- 0
    if(options$logregpmm %in% c("probit", "logreg")){
      trial.data$Y.0 <- as.factor(trial.data$Y.0)
      trial.data$Y.1 <- as.factor(trial.data$Y.1)
    }
    micedo <- mice(trial.data[,-1], print = F, predictorMatrix = predmat, maxit = 30, m = 15,
                    method = c("", "", "", "norm", "norm", options$logregpmm, options$logregpmm))
    curve.mids <- with(micedo, glm(Y ~ Z*S.1, family = binomial(link = "probit")))$analyses
    surface.mids <- with(micedo, glm(Y ~ Z*(S.0 + S.1), family = binomial(link = "probit")))$analyses
    miceout <- list(pool.curve = pool.sandwich(curve.mids), pool.surface = pool.sandwich(surface.mids), R.hat = get.R.hat(micedo))

    } else if(options$stdMice & ("D" %in% colnames(trial.data))){
      require(survival)
      predmat <- 1 - diag(dim(trial.data)[2])
      predmat[,c(1, 2, 4, 5)] <- 0
      predmat[10, c(7, 11)] <- 0
      predmat[11, c(6, 10)] <- 0
      micedo <- mice(trial.data, predictorMatrix= predmat, print = F, maxit = 30, m = 15,
                       method = c(rep("", 5), "pmm", "pmm", "norm", "norm", "survreg", "survreg"))
      curve.mids <- with(micedo, survreg(Surv(Y, D) ~ Z*S.1, dist = "exponential"))$analyses
      surface.mids <- with(micedo, survreg(Surv(Y, D) ~ Z*(S.0 + S.1), dist = "exponential"))$analyses
      miceout <- list(pool.curve = pool.sandwich(curve.mids), pool.surface = pool.sandwich(surface.mids), R.hat = get.R.hat(micedo))
    
  } else if(!options$stdMice) { 
  
  miceout <- tryCatch(analyze.hot.deck(trial.data, m = 5), error = function(e){ print(e); NA}, finally = print("lookout"))
  if(length(miceout) == 1 & is.na(miceout[1])){
    return(list(curve.coeff = rep(NA, 4), curve.cov = rep(NA, 4*4), surface.coeff = rep(NA, 6), surface.cov = rep(NA, 6*6)))
  }
  
  }
  
  curvecoeff <- c(miceout$pool.curve$qbar, sqrt(diag(miceout$pool.curve$t)))
  names(curvecoeff) <- rep(names(miceout$pool.curve$qbar), 2)
  
  surfacecoeff <- c(miceout$pool.surface$qbar, sqrt(diag(miceout$pool.surface$t)))
  names(surfacecoeff) <- rep(names(miceout$pool.surface$qbar), 2)
  
  curvecovariance <- as.vector(miceout$pool.curve$t)
  names(curvecovariance) <- as.vector(outer(colnames(miceout$pool.curve$t), colnames(miceout$pool.curve$t), FUN = function(x, y) paste(x, y, sep = ",")))
  surfacecovariance <- as.vector(miceout$pool.surface$t)
  names(surfacecovariance) <- as.vector(outer(colnames(miceout$pool.surface$t), colnames(miceout$pool.surface$t), FUN = function(x, y) paste(x, y, sep = ","))) 
  
  if(!is.null(miceout$R.hat)){ 
    return(list(curve.coeff = curvecoeff, curve.cov = curvecovariance, 
                surface.coeff = surfacecoeff, surface.cov = surfacecovariance, R.hat = miceout$R.hat))
  } else{
  return(list(curve.coeff = curvecoeff, curve.cov = curvecovariance, surface.coeff = surfacecoeff, surface.cov = surfacecovariance))
  }
  
  }

out<-runone(outcomeType, samp.args, options)



return(out)
}
  
  output.apply<-mpi.applyLB(1:reps, runonwrapper)


      surface = as.data.frame(t(sapply(output.apply,function(d) d$surface.coeff)))
      curve = as.data.frame(t(sapply(output.apply,function(d) d$curve.coeff)))

surface.cov = as.data.frame(t(sapply(output.apply,function(d) d$surface.cov)))
curve.cov = as.data.frame(t(sapply(output.apply,function(d) d$curve.cov)))


  write.csv(curve, paste(name, Sys.Date(), "_coeffcients_curve.csv", sep=''), row.names=FALSE)
  write.csv(surface, paste(name, Sys.Date(), "_coeffcients_surface.csv", sep=''), row.names=FALSE)
write.csv(curve.cov, paste(name, Sys.Date(), "_covariances_curve.csv", sep=''), row.names=FALSE)
write.csv(surface.cov, paste(name, Sys.Date(), "_covariances_surface.csv", sep=''), row.names=FALSE)

if(!is.null(output.apply[[1]]$R.hat)){
  
  write.csv(as.data.frame(t(sapply(output.apply, function(d) d$R.hat))), paste(name, Sys.Date(), "_Rhat_both.csv", sep=''), row.names=FALSE)
  
}



  
}                                        
                     

