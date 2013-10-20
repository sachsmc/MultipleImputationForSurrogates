require(mice)


###Step 2 impute Y(0) using imputed S(1) as well as W and potentially S(0):

imputeY0<-function(trial.data.steps,k){
zedo0<-trial.data.steps[trial.data.steps$Z==0,]

sampSrows<-sample(1:dim(zedo0)[1], dim(zedo0)[1], replace = TRUE)
zedo0resamp<-zedo0[sampSrows, ]
meanmodY0<-glm(Y~S+W+S.0, family=binomial(link="probit"), data=zedo0resamp)


predcitforz0<-predict(meanmodY0, trial.data.steps[trial.data.steps$Z==0,])
newdata1<-trial.data.steps[trial.data.steps$Z==1,]
predcitforz1<-predict(meanmodY0, newdata1)
for(i in 1:length(predcitforz1)){
distanceoi<-(abs(predcitforz0-predcitforz1[i])+min(abs(predcitforz0-predcitforz1[i])))^k
ly0oi<-(1/distanceoi)/sum(1/distanceoi)
length(trial.data.steps$Y[trial.data.steps$Z==0])
newdata1$Y.0[i]<-sample(trial.data.steps$Y.0[trial.data.steps$Z==0], 1, replace=FALSE, prob=ly0oi)
}
trial.data.steps2<-rbind(trial.data.steps[trial.data.steps$Z==0,], newdata1)
return(trial.data.steps2)
}


###Step 3 impute Y(1) using imputed S(1) as well as W and potentially S(0):
imputeY1<-function(trial.data.steps2,k){
zedo1<-trial.data.steps2[trial.data.steps2$Z==1,]
sampSrows<-sample(1:dim(zedo1)[1], dim(zedo1)[1], replace = TRUE)
zedo1resamp<-zedo1[sampSrows, ]
meanmodY1<-glm(Y~S+W+S.0,  family=binomial(link="probit"), data=zedo1resamp)


predcitforz1<-predict(meanmodY1, trial.data.steps2[trial.data.steps2$Z==1,])
newdata1<-trial.data.steps2[trial.data.steps2$Z==0,]
predcitforz0<-predict(meanmodY1, newdata1)
for(i in 1:length(predcitforz0)){
distanceoi<-(abs(predcitforz1-predcitforz0[i])+min(abs(predcitforz1-predcitforz0[i])))^k
ly1oi<-(1/distanceoi)/sum(1/distanceoi)
length(trial.data.steps2$Y[trial.data.steps2$Z==1])
newdata1$Y.1[i]<-sample(trial.data.steps2$Y.1[trial.data.steps2$Z==1], 1, replace=FALSE, prob=ly1oi)
}
trial.data.steps3<-rbind(trial.data.steps2[trial.data.steps2$Z==1,], newdata1)
return(trial.data.steps3)
}


###Step 4 re-impute S(1) given W, Y(0) and Y(1) and potentially S(0)
reimputeS<-function(trial.data.steps3,k){
zedo1<-trial.data.steps3[trial.data.steps3$R==1,]
sampSrows<-sample(1:dim(zedo1)[1], dim(zedo1)[1], replace = TRUE)
zedo1resamp<-zedo1[sampSrows, ]
meanmodS<-lm(S~Y.0+W+S.0+Y.1, data=zedo1resamp)

predcitforR1<-predict(meanmodS, trial.data.steps3[trial.data.steps3$R==1,])
newdata1<-trial.data.steps3[trial.data.steps3$R==0,]
predcitforR0<-predict(meanmodS, newdata1)
for(i in 1:length(predcitforR0)){
distanceoi<-(abs(predcitforR1-predcitforR0[i])+min(abs(predcitforR1-predcitforR0[i])))^k
lsoi<-(1/distanceoi)/sum(1/distanceoi)
length(trial.data.steps3$Y[trial.data.steps3$Z==0])
newdata1$S[i]<-sample(trial.data.steps3$S[trial.data.steps3$R==1], 1, replace=FALSE, prob=lsoi)
}
trial.data.steps4<-rbind(trial.data.steps3[trial.data.steps3$R==1,], newdata1)
return(trial.data.steps4)
}

###Step 5 re-do steps 2-4 until converagence.


rondomselectorderreimpute<-function(trial.data.steps4,k){
set.seed<-sample(.Random.seed[2:3], 1)
selectpull<-sample(1:4, 1)
if(selectpull==1){
trial.data.steps<-reimputeS(imputeY1(imputeY0(trial.data.steps4,k),k),k)
meanmodS<-sum(lm(S~Y.0+W+S.0+Y.1, data=trial.data.steps)$coeff)
}
if(selectpull==2){
trial.data.steps<-reimputeS(imputeY0(imputeY1(trial.data.steps4,k),k),k)
meanmodS<-sum(lm(S~Y.0+W+S.0+Y.1, data=trial.data.steps)$coeff)
}
if(selectpull==3){
trial.data.steps<-imputeY1(reimputeS(imputeY0(trial.data.steps4,k),k),k)
meanmodS<-sum(lm(S~Y.0+W+S.0+Y.1, data=trial.data.steps)$coeff)
}
if(selectpull==4){
trial.data.steps<-imputeY0(reimputeS(imputeY1(trial.data.steps4,k),k),k)
meanmodS<-sum(lm(S~Y.0+W+S.0+Y.1, data=trial.data.steps)$coeff)
}
return(list(trial.data.steps, meanmodS))
}

Zconverfun<-function(betaout){
num13<-floor(dim(betaout)[1]/3)
B1i<-apply(betaout[1:num13,], 2, sum)*(3/dim(betaout)[1])
B1.<-sum(betaout[1:num13,])*(3/(dim(betaout)[2]*dim(betaout)[1]))

B3i<-apply(betaout[(dim(betaout)[1]-num13):dim(betaout)[1],], 2, sum)*(3/dim(betaout)[1])
B3.<-sum(betaout[(dim(betaout)[1]-num13):dim(betaout)[1],])*(3/(dim(betaout)[2]*dim(betaout)[1]))


S1<-(1/(dim(betaout)[2]-1))*sum((B1i-B1.)^2)
S3<-(1/(dim(betaout)[2]-1))*sum((B3i-B3.)^2)
Z<-S1/S3
return(Z)
}

Runmodfunc<-function(runout1, runout2, runout3, runout4, runout5, runout6){
mod1<-glm(Y ~ Z*S, data = runout1[[1]], family = binomial(link="probit"))
mod2<-glm(Y ~ Z*S, data = runout2[[1]], family = binomial(link="probit"))
mod3<-glm(Y ~ Z*S, data = runout3[[1]], family = binomial(link="probit"))
mod4<-glm(Y ~ Z*S, data = runout4[[1]], family = binomial(link="probit"))
mod5<-glm(Y ~ Z*S, data = runout5[[1]], family = binomial(link="probit"))
mod6<-glm(Y ~ Z*S, data = runout6[[1]], family = binomial(link="probit"))
coeffcients<-apply(rbind(mod1$coeff,mod2$coeff,mod3$coeff,mod4$coeff,mod5$coeff,mod6$coeff),2,mean)
allSE<-rbind(diag(vcov(mod1)), diag(vcov(mod2)), diag(vcov(mod3)), diag(vcov(mod4)), diag(vcov(mod5)), diag(vcov(mod6)))
rubins<-sqrt(apply(allSE, 2, mean)+(1+1/6)*diag(cov(allSE)))
return(cbind(coeffcients,rubins))
}

startsetandrun_func<-function(TE, sigma, mu, inc.placebo, nnn, beta.S.0.0, beta.S.0.1, beta.S.1.0, beta.S.1.1, beta.W.0, beta.W.1, rhos1W, rhos1s0, rhos0W, k, M){

sample0 <- samp.data.binary(TE, sigma, mu, inc.placebo, nnn, beta.S.0.0, beta.S.0.1, beta.S.1.0, beta.S.1.1, beta.W.0, beta.W.1, rhos1W, rhos1s0, rhos0W)
trial.data <- get.trial.data(sample0, prob.trt = .5, BIP = TRUE, BSM = FALSE, CPV = FALSE)

###start of chain 1#####
mice1 <- mice(trial.data[,1:6], m = 1, predictorMatrix = matrix(c(0,1, rep(0, 34)), nrow = 6, ncol = 6, byrow = TRUE), 
             method = "pmm", print = FALSE)

trial.data.steps <- cbind(complete(mice1), trial.data[,7:9])
trial.data.steps2<-imputeY0(trial.data.steps,k)
trial.data.steps3<-imputeY1(trial.data.steps2,k)
trial.data.steps4<-reimputeS(trial.data.steps3,k)
trial.data.stepsrun<-rondomselectorderreimpute(trial.data.steps4,k)
beta1i<-trial.data.stepsrun[[2]]
runout1<-rondomselectorderreimpute(trial.data.stepsrun[[1]],k)
beta1i<-c(beta1i, runout1[[2]]) 

###start of chain 2#####
mice1 <- mice(trial.data[,1:6], m = 1, predictorMatrix = matrix(c(0,1, rep(0, 34)), nrow = 6, ncol = 6, byrow = TRUE), 
             method = "pmm", print = FALSE)

trial.data.steps <- cbind(complete(mice1), trial.data[,7:9])
trial.data.steps2<-imputeY0(trial.data.steps, k)
trial.data.steps3<-imputeY1(trial.data.steps2, k)
trial.data.steps4<-reimputeS(trial.data.steps3, k)
trial.data.stepsrun<-rondomselectorderreimpute(trial.data.steps4,k)
beta2i<-trial.data.stepsrun[[2]]
runout2<-rondomselectorderreimpute(trial.data.stepsrun[[1]],k)
beta2i<-c(beta2i, runout2[[2]]) 

###start of chain 3#####
mice1 <- mice(trial.data[,1:6], m = 1, predictorMatrix = matrix(c(0,1, rep(0, 34)), nrow = 6, ncol = 6, byrow = TRUE), 
             method = "pmm", print = FALSE)

trial.data.steps <- cbind(complete(mice1), trial.data[,7:9])
trial.data.steps2<-imputeY0(trial.data.steps, k)
trial.data.steps3<-imputeY1(trial.data.steps2, k)
trial.data.steps4<-reimputeS(trial.data.steps3, k)
trial.data.stepsrun<-rondomselectorderreimpute(trial.data.steps4,k)
beta3i<-trial.data.stepsrun[[2]]
runout3<-rondomselectorderreimpute(trial.data.stepsrun[[1]],k)
beta3i<-c(beta3i, runout3[[2]]) 

###start of chain 4#####
mice1 <- mice(trial.data[,1:6], m = 1, predictorMatrix = matrix(c(0,1, rep(0, 34)), nrow = 6, ncol = 6, byrow = TRUE), 
             method = "pmm", print = FALSE)

trial.data.steps <- cbind(complete(mice1), trial.data[,7:9])
trial.data.steps2<-imputeY0(trial.data.steps, k)
trial.data.steps3<-imputeY1(trial.data.steps2, k)
trial.data.steps4<-reimputeS(trial.data.steps3, k)
trial.data.stepsrun<-rondomselectorderreimpute(trial.data.steps4,k)
beta4i<-trial.data.stepsrun[[2]]
runout4<-rondomselectorderreimpute(trial.data.stepsrun[[1]],k)
beta4i<-c(beta4i, runout4[[2]]) 

###start of chain 5#####
mice1 <- mice(trial.data[,1:6], m = 1, predictorMatrix = matrix(c(0,1, rep(0, 34)), nrow = 6, ncol = 6, byrow = TRUE), 
             method = "pmm", print = FALSE)

trial.data.steps <- cbind(complete(mice1), trial.data[,7:9])
trial.data.steps2<-imputeY0(trial.data.steps, k)
trial.data.steps3<-imputeY1(trial.data.steps2, k)
trial.data.steps4<-reimputeS(trial.data.steps3, k)
trial.data.stepsrun<-rondomselectorderreimpute(trial.data.steps4,k)
beta5i<-trial.data.stepsrun[[2]]
runout5<-rondomselectorderreimpute(trial.data.stepsrun[[1]],k)
beta5i<-c(beta5i, runout5[[2]]) 


###start of chain 6#####
mice1 <- mice(trial.data[,1:6], m = 1, predictorMatrix = matrix(c(0,1, rep(0, 34)), nrow = 6, ncol = 6, byrow = TRUE), 
             method = "pmm", print = FALSE)

trial.data.steps <- cbind(complete(mice1), trial.data[,7:9])
trial.data.steps2<-imputeY0(trial.data.steps, k)
trial.data.steps3<-imputeY1(trial.data.steps2, k)
trial.data.steps4<-reimputeS(trial.data.steps3, k)
trial.data.stepsrun<-rondomselectorderreimpute(trial.data.steps4,k)
beta6i<-trial.data.stepsrun[[2]]
runout6<-rondomselectorderreimpute(trial.data.stepsrun[[1]],k)
beta6i<-c(beta6i, runout6[[2]]) 

for(i in 1:M){
runout1<-rondomselectorderreimpute(runout1[[1]],k)
beta1i<-c(beta1i, runout1[[2]]) 
runout2<-rondomselectorderreimpute(runout2[[1]],k)
beta2i<-c(beta2i, runout2[[2]]) 
runout3<-rondomselectorderreimpute(runout3[[1]],k)
beta3i<-c(beta3i, runout3[[2]]) 
runout4<-rondomselectorderreimpute(runout4[[1]],k)
beta4i<-c(beta4i, runout4[[2]]) 
runout5<-rondomselectorderreimpute(runout5[[1]],k)
beta5i<-c(beta5i, runout5[[2]]) 
runout6<-rondomselectorderreimpute(runout6[[1]],k)
beta6i<-c(beta6i, runout6[[2]]) 
print(i)
}
betaout<-cbind(beta1i, beta2i, beta3i, beta4i, beta5i, beta6i)
converagence<-Zconverfun(betaout)
resultsout<-Runmodfunc(runout1, runout2, runout3, runout4, runout5, runout6)
#return(cbind(resultsout, converagence, M, k))
return(cbind(t(resultsout[,1]),t(resultsout[,2]), converagence, M, k))
}


#outtest<-startsetandrun_func(.6,  sigma=c(1, 1, 1), mu=c(0,2,2), .2, 4000, 0, 0, 0,  0, 0, 0, 0.8, 0.3, 0.8, 3, 5)
