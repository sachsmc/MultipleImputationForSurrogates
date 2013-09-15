setwd('~/Dropbox/Betz_postdoc/meta_paper_IGPS/CODE') 
source("meta-analysis-generation.R")
library(mice)

hit.one <- function(){
sample0 <- samp.data.binary (TE = .6, sigma = c(1, 1, 1), mu = c(0,2,2), inc.placebo = .2, 
                             nnn=4000, beta.S.0.0=0, beta.S.0.1=0, beta.S.1.0 = 0, 
                             beta.S.1.1 = 0, beta.W.0=0, beta.W.1=0, rhos1W=0.8, rhos1s0=0.3, rhosoW=0.8)

trial.data <- get.trial.data(sample0$sample, prob.trt = .5, S.sampling = "simple 1")

mice1 <- mice(trial.data, m = 1, predictorMatrix = matrix(c(0,1, rep(0, 34)), nrow = 6, ncol = 6, byrow = TRUE), 
             method = "pmm", print = FALSE)

#fit.full <- glm(Y~ Z*S.1, data = trial.data, family = binomial(link = "probit"))

#imputed.list <- impute.model(trial.data, M = 5)
#imp.fit <- run.probit.model(imputed.list)

#pee.eff <- 2*pnorm(-abs(imp.fit$beta[4])/imp.fit$se.beta[4])
imp.beta <- pool(glm.mids(Y ~ Z*S, data = mice1, family = binomial(link="probit")))

imp.beta.suface <- pool(glm.mids(Y ~ Z*S.1 +Z*S.0, data = mice1, family = binomial(link="probit")))
#pee.eff <- 2*pnorm(-abs(imp.beta$qbar[4])/sqrt(diag(imp.beta$t))[4])
pee.eff <- 1-pchisq(t(imp.beta$qbar[3:4])%*%solve(imp.beta$t[3:4,3:4])%*%imp.beta$qbar[3:4], df = 2)
return(c(imp.beta$qbar, pee.eff))
}

true.betas <- samp.data.binary (TE = .6, sigma = c(1, 1, 1), mu = c(0,2,2), 
                                           inc.placebo = .2, nnn=4000, beta.S.0.0=0, beta.S.0.1=0, 
                                           beta.S.1.0 = 0, beta.S.1.1 = 0, beta.W.0=0, 
                                           beta.W.1=0, rhos1W=0.8, rhos1s0=0.5, rhosoW=0.8)$output.betas[c(1, 2, 3, 5)]

                                           
                                           
                                           

est.betas <- NULL
for(i in 1:100){
  est.betas <- rbind(est.betas, hit.one())
  if(i %in% seq(5, 100, by = 10)){ cat(i); cat(" ") }
}

hist(est.betas[,5])
mean(est.betas[,5] < .05)
## model including only W 
colMeans(est.betas)[1:4] - true.betas
sqrt(diag(cov(est.betas)))

source("meta-analysis-generation-S0.R")

est.betasS0 <- NULL
for(i in 1:100){
  est.betasS0 <- rbind(est.betasS0, hit.one())
  cat(i)
}

## model including W, Y.1 and S0
colMeans(est.betasS0[,5:8]) - true.betas


source("meta-analysis-generation-S0andY0.R")

est.betasS0Y0 <- NULL
for(i in 1:100){
  est.betasS0Y0 <- rbind(est.betasS0Y0, hit.one())
  cat(i)
}

## model including W, Y.1 Y.0 and S0
colMeans(est.betasS0Y0[,5:8]) - true.betas

##residuals

resid <- subset(imputed.list[[1]], Z ==0)$S.1 - subset(trial.data, Z == 0)$S.1
plot(subset(imputed.list[[1]], Z ==0)$S.1 ~ subset(sample0$sample, Z == 0)$S.1)
plot(resid ~ subset(trial.data, Z == 0)$Y)



