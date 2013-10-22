
micerunone.binary<-function(trial.data){
require(mice)
if(sum(is.na(trial.data$S.0))>1){
mice1 <- mice(trial.data[,3:6], m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0), nrow = 4, ncol = 4, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
mice2 <- mice(cbind(trial.data[,3], complete(mice1)[,3:4],trial.data[,7:8]),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

miceS1<-mice(cbind(trial.data[,3], trial.data[,5], complete(mice1)[,4], complete(mice2)[,4:5]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
miceS0<-mice(cbind(trial.data[,3], trial.data[,6], complete(mice1)[,3], complete(mice2)[,4:5]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
miceY0<-mice(cbind(trial.data[,3], trial.data[,8], complete(mice1)[,3:4], complete(mice2)[,4]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceY1<-mice(cbind(trial.data[,3], trial.data[,7], complete(mice1)[,3:4], complete(mice2)[,5]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)


miceS1_rep<-mice(cbind(trial.data[,3], trial.data[,5], complete(miceS0)[,2], complete(miceY0)[,2], complete(miceY1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
miceS0_rep<-mice(cbind(trial.data[,3], trial.data[,6], complete(miceS1_rep)[,2], complete(miceY0)[,2], complete(miceY1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
miceY0_rep<-mice(cbind(trial.data[,3], trial.data[,8], complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], complete(miceY1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceY1_rep<-mice(cbind(trial.data[,3], trial.data[,7], complete(miceS0_rep)[,2], complete(miceY0_rep)[,2], complete(miceS1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)


for(i in 1:50){
miceS1_rep<-mice(cbind(trial.data[,3], trial.data[,5], complete(miceS0_rep)[,2], complete(miceY0_rep)[,2], complete(miceY1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "norm", print = FALSE, maxit=1)

miceS0_rep<-mice(cbind(trial.data[,3], trial.data[,6], complete(miceS1_rep)[,2], complete(miceY0_rep)[,2], complete(miceY1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "norm", print = FALSE, maxit=1)

miceY0_rep<-mice(cbind(trial.data[,3], trial.data[,8], complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], complete(miceY1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

miceY1_rep<-mice(cbind(trial.data[,3], trial.data[,7], complete(miceS0_rep)[,2], complete(miceY0_rep)[,2], complete(miceS1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
}
outputdata<-cbind(trial.data[,1:4], complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], complete(miceY1_rep)[,2], complete(miceY0_rep)[,2])

}else{
mice1 <- mice(trial.data[,3:6], m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0), nrow = 4, ncol = 4, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
mice2 <- mice(cbind(trial.data[,3], complete(mice1)[,3:4],trial.data[,7:8]),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)

miceS1<-mice(cbind(trial.data[,3], trial.data[,5], trial.data$S.0, complete(mice2)[,4:5]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
miceY0<-mice(cbind(trial.data[,3], trial.data[,8], complete(mice1)[,3], trial.data$S.0, complete(mice2)[,4]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceY1<-mice(cbind(trial.data[,3], trial.data[,7], complete(mice1)[,3], trial.data$S.0, complete(mice2)[,5]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)


miceS1_rep<-mice(cbind(trial.data[,3], trial.data[,5], trial.data$S.0, complete(miceY0)[,2], complete(miceY1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
miceY0_rep<-mice(cbind(trial.data[,3], trial.data[,8], complete(miceS1_rep)[,2], trial.data$S.0, complete(miceY1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceY1_rep<-mice(cbind(trial.data[,3], trial.data[,7], trial.data$S.0, complete(miceY0_rep)[,2], complete(miceS1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)


for(i in 1:50){
miceS1_rep<-mice(cbind(trial.data[,3], trial.data[,5], trial.data$S.0, complete(miceY0_rep)[,2], complete(miceY1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
miceY0_rep<-mice(cbind(trial.data[,3], trial.data[,8], complete(miceS1_rep)[,2], trial.data$S.0, complete(miceY1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
miceY1_rep<-mice(cbind(trial.data[,3], trial.data[,7], trial.data$S.0, complete(miceY0_rep)[,2], complete(miceS1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
}

outputdata<-cbind(trial.data[,1:4], complete(miceS1_rep)[,2], trial.data$S.0, complete(miceY1_rep)[,2], complete(miceY0_rep)[,2])
}
colnames(outputdata) <- c("S", "Z", "W", "Y", "S.1", "S.0", "Y.1", "Y.0")
return(outputdata)
}


micerunone.tte<-function(trial.data){
  require(mice)
if(sum(is.na(trial.data$S.0))>1){
  mice1 <- mice(cbind(trial.data$W, trial.data$Z, trial.data$S.0, trial.data$S.1), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0), nrow = 4, ncol = 4, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
  mice2 <- mice(cbind(trial.data$W, complete(mice1)[,3:4],trial.data$D.1, trial.data$D.0),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  mice3 <- mice(cbind(trial.data$W, complete(mice1)[,3:4],complete(mice2)[,4:5], trial.data$Y.0),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0), nrow = 6, ncol = 6, byrow = TRUE), method = "survreg", print = FALSE, maxit=1)
  mice4 <- mice(cbind(trial.data$W, complete(mice1)[,3:4],complete(mice2)[,4:5], trial.data$Y.1),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0), nrow = 6, ncol = 6, byrow = TRUE), method = "survreg", print = FALSE, maxit=1)

  miceS1<-mice(cbind(trial.data$W, trial.data$S.1, complete(mice1)[,3], complete(mice2)[,4:5], trial.data$log.X.1, trial.data$log.X.0), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
  miceS0<-mice(cbind(trial.data$W, trial.data$S.0, complete(mice1)[,4], complete(mice2)[,4:5], trial.data$log.X.1, trial.data$log.X.0), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "norm", print = FALSE, maxit=1)
  miceD0<-mice(cbind(trial.data$W, trial.data$D.0, complete(miceS1)[,2],complete(miceS0)[,2], complete(mice2)[,4]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  miceD1<-mice(cbind(trial.data$W, trial.data$D.1, complete(miceS1)[,2],complete(miceS0)[,2], complete(mice2)[,5]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  miceY0<-mice(cbind(trial.data$W, cbind(trial.data$Y.0,complete(miceD0)[,2]), complete(miceS1)[,2],complete(miceS0)[,2], complete(miceD1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "survreg", print = FALSE, maxit=1)
  miceY1<-mice(cbind(trial.data$W, cbind(trial.data$Y.1,complete(miceD1)[,2]), complete(miceS1)[,2],complete(miceS0)[,2], complete(miceD0)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "survreg", print = FALSE, maxit=1)
  
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
  
  outputdata<-as.data.frame(cbind(trial.data$S,trial.data$Z, trial.data$W, trial.data$Y, trial.data$D, complete(miceS1_rep)[,2], complete(miceS0_rep)[,2], complete(miceY1_rep)[,2], complete(miceY0_rep)[,2], complete(miceD1_rep)[,2], complete(miceD0_rep)[,2]))
  
 }else{
  trial.data$Y.0.E<-ifelse(trial.data$D.0==1, trial.data$Y.0, NA)
  trial.data$Y.1.E<-ifelse(trial.data$D.1==1, trial.data$Y.1, NA)
  mice1 <- mice(cbind(trial.data$W, trial.data$Z, trial.data$S.0, trial.data$S.1), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0), nrow = 4, ncol = 4, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  
  mice2 <- mice(cbind(trial.data$W, complete(mice1)[,4],trial.data$S.0, trial.data$D.1, trial.data$D.0),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  
  mice3 <- mice(cbind(trial.data$W, complete(mice1)[,4],trial.data$S.0, complete(mice2)[,4:5], trial.data$Y.0.E),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0), nrow = 6, ncol = 6, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  
  mice4 <- mice(cbind(trial.data$W, complete(mice1)[,4],trial.data$S.0, complete(mice2)[,4:5], trial.data$Y.1.E),  m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0), nrow = 6, ncol = 6, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  
  
  trial.data$log.X.1<-ifelse(complete(mice4)[,4]==1, log(complete(mice4)[,6]), log(7.5))
  trial.data$log.X.0<-ifelse(complete(mice4)[,5]==1, log(complete(mice3)[,6]), log(7.5))
  
  miceS1<-mice(cbind(trial.data$W, trial.data$S.1, trial.data$S.0, complete(mice2)[,4:5], trial.data$log.X.1, trial.data$log.X.0), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  miceD0<-mice(cbind(trial.data$W, trial.data$D.0, complete(miceS1)[,2],trial.data$S.0, complete(mice2)[,4]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  miceD1<-mice(cbind(trial.data$W, trial.data$D.1, complete(miceS1)[,2],trial.data$S.0, complete(mice2)[,5]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  miceY0<-mice(cbind(trial.data$W, trial.data$Y.0.E, complete(miceS1)[,2],trial.data$S.0, complete(miceD1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  miceY1<-mice(cbind(trial.data$W, trial.data$Y.1.E, complete(miceS1)[,2],trial.data$S.0, complete(miceD0)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  
  trial.data$log.X.1<-ifelse(complete(miceD1)[,2]==1, log(complete(miceY1)[,2]), log(7.5))
  trial.data$log.X.0<-ifelse(complete(miceD0)[,2]==1, log(complete(miceY0)[,2]), log(7.5))
  
  miceS1_rep<-mice(cbind(trial.data$W, trial.data$S.1, trial.data$S.0, trial.data$log.X.1, trial.data$log.X.0, complete(miceD0)[,2], complete(miceD1)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  miceD0_rep<-mice(cbind(trial.data$W, trial.data$D.0, complete(miceS1_rep)[,2], trial.data$S.0, trial.data$log.X.1, complete(miceD1)[,2], trial.data$log.X.0), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  miceD1_rep<-mice(cbind(trial.data$W, trial.data$D.1, trial.data$S.0, trial.data$log.X.0, complete(miceS1_rep)[,2], complete(miceD0)[,2], trial.data$log.X.1), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  miceY0_rep<-mice(cbind(trial.data$W, trial.data$Y.0.E, complete(miceS1_rep)[,2], trial.data$S.0, complete(miceD1_rep)[,2], trial.data$log.X.1, complete(miceD0_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  miceY1_rep<-mice(cbind(trial.data$W, trial.data$Y.1.E, complete(miceS1_rep)[,2], trial.data$S.0, complete(miceD0_rep)[,2], trial.data$log.X.0, complete(miceD1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7,  byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
  
  trial.data$log.X.1<-ifelse(complete(miceD1_rep)[,2]==1, log(complete(miceY1_rep)[,2]), log(7.5))
  trial.data$log.X.0<-ifelse(complete(miceD0_rep)[,2]==1, log(complete(miceY0_rep)[,2]), log(7.5))
  
  for(i in 1:50){
    miceS1_rep<-mice(cbind(trial.data$W, trial.data$S.1, trial.data$S.0, trial.data$log.X.1, trial.data$log.X.0, complete(miceD0_rep)[,2], complete(miceD1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
    miceD0_rep<-mice(cbind(trial.data$W, trial.data$D.0, complete(miceS1_rep)[,2], trial.data$S.0, trial.data$log.X.1, complete(miceD1_rep)[,2], trial.data$log.X.0), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
    miceD1_rep<-mice(cbind(trial.data$W, trial.data$D.1, trial.data$S.0, trial.data$log.X.0, complete(miceS1_rep)[,2], complete(miceD0_rep)[,2], trial.data$log.X.1), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
    
    miceY0_rep<-mice(cbind(trial.data$W, trial.data$Y.0.E, complete(miceS1_rep)[,2], trial.data$S.0, complete(miceD1_rep)[,2], trial.data$log.X.1, complete(miceD0_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
    miceY1_rep<-mice(cbind(trial.data$W, trial.data$Y.1.E, complete(miceS1_rep)[,2], trial.data$S.0, complete(miceD0_rep)[,2], trial.data$log.X.0, complete(miceD1_rep)[,2]), m = 1, predictorMatrix = matrix(c(0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 7, ncol = 7,  byrow = TRUE), method = "pmm", print = FALSE, maxit=1)
    
    trial.data$log.X.1<-ifelse(complete(miceD1_rep)[,2]==1, log(complete(miceY1_rep)[,2]), log(7.5))
    trial.data$log.X.0<-ifelse(complete(miceD0_rep)[,2]==1, log(complete(miceY0_rep)[,2]), log(7.5))
  }
  
  outputdata<-as.data.frame(cbind(trial.data$S,trial.data$Z, trial.data$W, trial.data$Y, trial.data$D, complete(miceS1_rep)[,2], trial.data$S.0, complete(miceY1_rep)[,2], complete(miceY0_rep)[,2], complete(miceD1_rep)[,2], complete(miceD0_rep)[,2]))
 }
  colnames(outputdata) <- c("S", "Z", "W", "Y", "D",  "S.1", "S.0", "Y.1", "Y.0", "D.1", "D.0")
  return(outputdata)
}


pool.sandwich <- function(analyses){
  require(sandwich)
  m <- length(analyses)
  k <- length(analyses[[1]]$coeff)
  names <- names(analyses[[1]]$coeff)
  qhat <- matrix(NA, nrow = m, ncol = k, dimnames = list(1:m, 
                                                         names))
  u <- array(NA, dim = c(m, k, k), dimnames = list(1:m, names, 
                                                   names))
  
  for(i in 1:m){
    fit <- analyses[[i]]
    
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
  dfcom <- df.residual(analyses[[1]])
  df <- mice:::mice.df(m, lambda, dfcom, "smallsample")
  fmi <- (r + 2/(df + 3))/(r + 1)
  names(r) <- names(df) <- names(fmi) <- names(lambda) <- names
  fit <- list(call = analyses[[1]]$call, m = m, qhat = qhat, u = u, qbar = qbar, 
              ubar = ubar, b = b, t = t, r = r, dfcom = dfcom, df = df, 
              fmi = fmi, lambda = lambda)
  oldClass(fit) <- c("mipo", oldClass(analyses))
  return(fit)
  
}


analyze.hot.deck <- function(trial.data, m = 5){
  
  if("D" %in% names(trial.data)){
    imputed.sets <- lapply(1:m, function(i) micerunone.tte(trial.data))
    require(survival)
    
    multi.analyses.curve <- lapply(imputed.sets, function(trial.data){
      fit <- survreg(Surv(Y, D) ~ Z*S.1, dist = "exponential", data = trial.data)
      })
    
    multi.analyses.surface <- lapply(imputed.sets, function(trial.data){
      fit <- survreg(Surv(Y, D) ~ Z*(S.0 + S.1), dist = "exponential", data = trial.data)
    })
    
    
  } else{
  imputed.sets <- lapply(1:m, function(i) micerunone.binary(trial.data))
  
  multi.analyses.curve <- lapply(imputed.sets, function(trial.data){
    fit <- glm(Y ~ Z*S.1, data = trial.data, family = binomial(link = "probit"))
    return(fit)
  })
  
  multi.analyses.surface <- lapply(imputed.sets, function(trial.data){
    fit <- glm(Y ~ Z*(S.0 + S.1), data = trial.data, family = binomial(link = "probit"))
    return(fit)
  })
  }
  curve.pool <- pool.sandwich(multi.analyses.curve)
  surface.pool <- pool.sandwich(multi.analyses.surface)
  return(list(pool.curve = curve.pool, pool.surface = surface.pool))
}

