source("meta-analysis-generation_new.R")
source("multi-impute-analysis.R")

#defaults
#binary or tte  TE = .6; sigma = c(1, 1, 1); mu = c(0,2,2); inc.placebo = .2; 
#nnn=4000; beta.S.0.0=0; beta.S.0.1=0; beta.S.1.0 = 0; 
#beta.S.1.1 = 0; beta.W.0=0; beta.W.1=0; rhos1W=0.8; rhos1s0=0.3; rhosoW=0.8

#samp.args <- list(TE = .6, sigma = c(1,1,1), mu = c(0,2,2), inc.placebo = .2,
 #                 nnn = 4000, beta.S.0.0 = 0, beta.S.0.1 = 0, beta.S.1.0 = 0, 
  #                beta.S.1.1 = 0, beta.W.0 = 0, beta.W.1 = 0, rhos1W = .8, rhos1s0 =.3, rhos0W = .8)

run.one <- function(outcomeType, samp.args, options){  
  
  samp.func <- paste0("samp.data.", outcomeType)
  sample0 <- do.call(samp.func, samp.args)
  
trial.data <- do.call("get.trial.data", 
                      list(raw.data.class = sample0, prob.trt = options$prob.trt, 
                           BIP = options$BIP, BSM = options$BSM, CPV = options$CPV))
  
miceout <- impute.data(trial.data, iterations = options$iterations)
analysis <- analyze.imputed.data(miceout)
  
lapply(analysis, function(lll) list(est = lll$qbar, se = sqrt(diag( lll$t ))))
  
}

run.many <- function(scenVect, reps = 500){  ## scenVect needs to be mixed vector, ie, a data frame with 1 row
  outcomeType <- scenVect[1,1]
  samp.args <- list(TE = scenVect[1,2], sigma = unlist(scenVect[1,3:5]), mu = unlist(scenVect[1,6:8]), 
                    inc.placebo = scenVect[1,9], nnn = scenVect[1,10], beta.S.0.0 = scenVect[1,11], 
                    beta.S.0.1 = scenVect[1,12], beta.S.1.0 = scenVect[1,13], beta.S.1.1 = scenVect[1,14],
                    beta.W.0 = scenVect[1,15], beta.W.1 = scenVect[1,16], rhos1W = scenVect[1,17], 
                    rhos1s0 = scenVect[1,18], rhos0W = scenVect[1,19])
  options <- as.list(scenVect[1,20:24])
  names(options) = c("prob.trt", "BIP", "BSM", "CPV", "iterations")
  
  ## outputs
  
  output.apply <- lapply(1:reps, function(fake){
      run.one(outcomeType, samp.args, options)
    })
    
  return(
  
  list(curve = list(betas = t(sapply(output.apply, function(d) d$pool.curve$est)), 
                    ses = t(sapply(output.apply, function(d) d$pool.curve$se))), 
       surface = list(betas = t(sapply(output.apply, function(d) d$pool.surface$est)), 
                      ses = t(sapply(output.apply, function(d) d$pool.surface$se))))
  
  )
  
}                                        
                                           
                


