
micerunone.tte<-function(trial.data){
trial.data$Y.0.E<-ifelse(trial.data$D.0==1, trial.data$Y.0, NA)
trial.data$Y.1.E<-ifelse(trial.data$D.1==1, trial.data$Y.1, NA)
mice1 <- mice(cbind(trial.data$W, trial.data$Z, trial.data$S.0, trial.data$S.1), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0), nrow = 4, ncol = 4, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

mice2 <- mice(cbind(trial.data$W, complete(mice1)[,3:4],trial.data$D.1, trial.data$D.0),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

mice3 <- mice(cbind(trial.data$W, complete(mice1)[,3:4],complete(mice2)[,4:5], trial.data$Y.0.E),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0), nrow = 6, ncol = 6, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

mice4 <- mice(cbind(trial.data$W, complete(mice1)[,3:4],complete(mice2)[,4:5], trial.data$Y.1.E),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0), nrow = 6, ncol = 6, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)


trial.data$log.X.1<-ifelse(complete(mice4)[,4]==1, log(complete(mice4)[,6]), log(7.5))
trial.data$log.X.0<-ifelse(complete(mice4)[,5]==1, log(complete(mice3)[,6]), log(7.5))

miceS1<-mice(cbind(trial.data$W, trial.data$S.1, complete(mice1)[,3], complete(mice2)[,4:5], trial.data$log.X.1, trial.data$log.X.0), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceS0<-mice(cbind(trial.data$W, trial.data$S.0, complete(mice1)[,4], complete(mice2)[,4:5], trial.data$log.X.1, trial.data$log.X.0), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceD0<-mice(cbind(trial.data$W, trial.data$D.0, complete(miceS1)[,2],complete(miceS0)[,2], complete(mice2)[,4]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceD1<-mice(cbind(trial.data$W, trial.data$D.1, complete(miceS1)[,2],complete(miceS0)[,2], complete(mice2)[,5]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceY0<-mice(cbind(trial.data$W, trial.data$Y.0.E, complete(miceS1)[,2],complete(miceS0)[,2], complete(miceD1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceY1<-mice(cbind(trial.data$W, trial.data$Y.1.E, complete(miceS1)[,2],complete(miceS0)[,2], complete(miceD0)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

trial.data$log.X.1<-ifelse(complete(miceD1)[,2]==1, log(complete(miceY1)[,2]), log(7.5))
trial.data$log.X.0<-ifelse(complete(miceD0)[,2]==1, log(complete(miceY0)[,2]), log(7.5))

miceS1_rep<-mice(cbind(trial.data$W, trial.data$S.1, complete(miceS0)[,2], trial.data$log.X.1, trial.data$log.X.0, complete(miceD0)[,2], complete(miceD1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceS0_rep<-mice(cbind(trial.data$W, trial.data$S.0, complete(miceS1)[,2], trial.data$log.X.1, trial.data$log.X.0, complete(miceD0)[,2], complete(miceD1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceD0_rep<-mice(cbind(trial.data$W, trial.data$D.0, complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], trial.data$log.X.1, complete(miceD1)[,2], trial.data$log.X.0), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceD1_rep<-mice(cbind(trial.data$W, trial.data$D.1, complete(miceS0_rep)[,2], trial.data$log.X.0, complete(miceS1_rep)[,2], complete(miceD0)[,2], trial.data$log.X.1), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

miceY0_rep<-mice(cbind(trial.data$W, trial.data$Y.0.E, complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], complete(miceD1_rep)[,2], trial.data$log.X.1, complete(miceD0_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceY1_rep<-mice(cbind(trial.data$W, trial.data$Y.1.E, complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], complete(miceD0_rep)[,2], trial.data$log.X.0, complete(miceD1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7,  byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

trial.data$log.X.1<-ifelse(complete(miceD1_rep)[,2]==1, log(complete(miceY1_rep)[,2]), log(7.5))
trial.data$log.X.0<-ifelse(complete(miceD0_rep)[,2]==1, log(complete(miceY0_rep)[,2]), log(7.5))

for(i in 1:50){
miceS1_rep<-mice(cbind(trial.data$W, trial.data$S.1, complete(miceS0_rep)[,2], trial.data$log.X.1, trial.data$log.X.0, complete(miceD0_rep)[,2], complete(miceD1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceS0_rep<-mice(cbind(trial.data$W, trial.data$S.0, complete(miceS1_rep)[,2], trial.data$log.X.1, trial.data$log.X.0, complete(miceD0_rep)[,2], complete(miceD1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceD0_rep<-mice(cbind(trial.data$W, trial.data$D.0, complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], trial.data$log.X.1, complete(miceD1_rep)[,2], trial.data$log.X.0), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceD1_rep<-mice(cbind(trial.data$W, trial.data$D.1, complete(miceS0_rep)[,2], trial.data$log.X.0, complete(miceS1_rep)[,2], complete(miceD0_rep)[,2], trial.data$log.X.1), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

miceY0_rep<-mice(cbind(trial.data$W, trial.data$Y.0.E, complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], complete(miceD1_rep)[,2], trial.data$log.X.1, complete(miceD0_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceY1_rep<-mice(cbind(trial.data$W, trial.data$Y.1.E, complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], complete(miceD0_rep)[,2], trial.data$log.X.0, complete(miceD1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7,  byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

trial.data$log.X.1<-ifelse(complete(miceD1_rep)[,2]==1, log(complete(miceY1_rep)[,2]), log(7.5))
trial.data$log.X.0<-ifelse(complete(miceD0_rep)[,2]==1, log(complete(miceY0_rep)[,2]), log(7.5))
}

outputdata<-cbind(trial.data$S,trial.data$Z, trial.data$W, trial.data$Y, trial.data$D, complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], complete(miceY1_rep)[,2], complete(miceY0_rep)[,2], complete(miceD1_rep)[,2], complete(miceD0_rep)[,2])
colnames(outputdata) <- c("S", "Z", "W", "Y", "D",  "S.1", "S.0", "Y.1", "Y.0", "D.1", "D.0")
return(outputdata)
}

analyze.hot.deck <- function(trial.data, m = 5){
  
  imputed.sets <- lapply(1:m, function(i) micerunone(trial.data))
  
  multi.analyses.curve <- t(sapply(imputed.sets, function(trial.data){
    fit <- glm(Y ~ Z*S.1, data = trial.data, family = binomial(link = "probit"))
    return(fit$coeff) 
    }))
  
  return(colMeans(multi.analyses.curve))
}


run.one <- function(outcomeType, samp.args, options){  
  
  samp.func <- paste0("samp.data.", outcomeType)
  sample0 <- do.call(samp.func, samp.args)
  
  trial.data <- do.call("get.trial.data", list(raw.data.class = sample0, prob.trt = options$prob.trt, BIP = options$BIP, BSM = options$BSM, CPV = options$CPV))
  
  miceout <- analyze.hot.deck(trial.data, m = 5)
  
  
}



run.many <- function(scenVect, reps = 50){  ## scenVect needs to be mixed vector, ie, a data frame with 1 row
  outcomeType <- scenVect[1,1]
  samp.args <- list(TE = scenVect[1,2], sigma = unlist(scenVect[1,3:5]), mu = unlist(scenVect[1,6:8]), 
                    inc.placebo = scenVect[1,9], nnn = scenVect[1,10], beta.S.0.0 = scenVect[1,11], 
                    beta.S.0.1 = scenVect[1,12], beta.S.1.0 = scenVect[1,13], beta.S.1.1 = scenVect[1,14],
                    beta.W.0 = scenVect[1,15], beta.W.1 = scenVect[1,16], rhos1W = scenVect[1,17], 
                    rhos1s0 = scenVect[1,18], rhos0W = scenVect[1,19])
  options <- as.list(scenVect[1,20:24])
  names(options) = c("prob.trt", "BIP", "BSM", "CPV", "iterations")
  
  ## outputs
  
  output.apply <- matrix(NA, nrow = reps, ncol = 4)
  for(i in 1:reps){
    output.apply[i,] <- run.one(outcomeType, samp.args, options)
    cat(i)  
  }
  
  return(output.apply)
  
}                                        
