library(gdata)

##work
setwd('/Users/gabrielee/Dropbox/Betz_postdoc/imputation SOP/CODE')
##home
setwd('~/Dropbox/Betz_postdoc/Imputation SOP/CODE')



source("meta-analysis-generation_new2.R")
source("multi-impute-analysis.R")

scenVect<-secnarios[i,]

gettruebetas <- function(scenVect){ 
outcomeType <- scenVect$outcomeType2
samp.args <- list(TE = scenVect[1,2], sigma = unlist(scenVect[1,3:5]), mu = unlist(scenVect[1,6:8]), 
inc.placebo = scenVect[1,9], nnn = scenVect[1,10], beta.S.0.0 = scenVect[1,11], 
beta.S.0.1 = scenVect[1,12], beta.S.1.0 = scenVect[1,13], beta.S.1.1 = scenVect[1,14],
beta.W.0 = scenVect[1,15], beta.W.1 = scenVect[1,16], rhos1W = scenVect[1,17], 
rhos1s0 = scenVect[1,18], rhos0W = scenVect[1,19])
options <- as.list(scenVect[1,20:24])
names(options) = c("prob.trt", "BIP", "BSM", "CPV", "iterations")

samp.func <- paste0("samp.data.", outcomeType)
sample0 <- do.call(samp.func, samp.args)
surface_outbetas<-cbind(sample0$output.betas[c(-5, -8)])
curve_outbetas<-cbind(sample0$output.betas[c(-3, -5,-6, -8)])
return(list=c(surface_outbetas=surface_outbetas, curve_outbetas=curve_outbetas))
}

secnarios<-read.csv('scenarioFile.csv', stringsAsFactors = FALSE)
setwd('/Users/gabrielee/Dropbox/Betz_postdoc/imputation SOP/results')

setwd('~/Dropbox/Betz_postdoc/Imputation SOP/results')

#read in 1st set of simulations becuase you are stupid and didn't format date...####
S0NO_S1high_binary_BIP0.20.2_right_curve<-read.csv("S0NO_S1high_binary_BIP0.20.2_rightThu Sep 26 11:39:40 2013_coeffcients_curve.csv")
S0NO_S1high_binary_BIP0.20.2_right_surface<-read.csv("S0NO_S1high_binary_BIP0.20.2_rightThu Sep 26 11:39:40 2013_coeffcients_surface.csv")
S0NO_S1high_binary_BIP0.20.8_right_curve<-read.csv("S0NO_S1high_binary_BIP0.20.8_rightThu Sep 26 11:42:31 2013_coeffcients_curve.csv")
S0NO_S1high_binary_BIP0.20.8_right_surface<-read.csv("S0NO_S1high_binary_BIP0.20.8_rightThu Sep 26 11:42:31 2013_coeffcients_surface.csv")
S0NO_S1high_binary_BIP0.2BSM0.5CPV_right_curve<-read.csv("S0NO_S1high_binary_BIP0.2BSM0.5CPV_rightThu Sep 26 11:42:10 2013_coeffcients_curve.csv")
S0NO_S1high_binary_BIP0.2BSM0.5CPV_right_surface<-read.csv("S0NO_S1high_binary_BIP0.2BSM0.5CPV_rightThu Sep 26 11:42:10 2013_coeffcients_surface.csv")
S0NO_S1high_binary_BIP0.2BSM0.5_right_curve<-read.csv("S0NO_S1high_binary_BIP0.2BSM0.5_rightThu Sep 26 11:40:26 2013_coeffcients_curve.csv")
S0NO_S1high_binary_BIP0.2BSM0.5_right_surface<-read.csv("S0NO_S1high_binary_BIP0.2BSM0.5_rightThu Sep 26 11:40:26 2013_coeffcients_surface.csv")
S0NO_S1high_binary_BIP0.80.2_right_curve<-read.csv("S0NO_S1high_binary_BIP0.80.2_rightThu Sep 26 11:42:31 2013_coeffcients_curve.csv")
S0NO_S1high_binary_BIP0.80.2_right_surface<-read.csv("S0NO_S1high_binary_BIP0.80.2_rightThu Sep 26 11:42:31 2013_coeffcients_surface.csv")
S0NO_S1high_binary_BIP0.80.8_right_curve<-read.csv("S0NO_S1high_binary_BIP0.80.8_rightThu Sep 26 11:39:13 2013_coeffcients_curve.csv")
S0NO_S1high_binary_BIP0.80.8_right_surface<-read.csv("S0NO_S1high_binary_BIP0.80.8_rightThu Sep 26 11:39:13 2013_coeffcients_surface.csv")
S0NO_S1high_binary_nothing_right_curve<-read.csv("S0NO_S1high_binary_nothing_rightThu Sep 26 11:38:35 2013_coeffcients_curve.csv")
S0NO_S1high_binary_nothing_right_surface<-read.csv("S0NO_S1high_binary_nothing_rightThu Sep 26 11:38:35 2013_coeffcients_surface.csv")
S0NO_S1high_tte_BIP0.20.2_right_curve<-read.csv("S0NO_S1high_tte_BIP0.20.2_rightThu Sep 26 11:35:50 2013_coeffcients_curve.csv")
S0NO_S1high_tte_BIP0.20.2_right_surface<-read.csv("S0NO_S1high_tte_BIP0.20.2_rightThu Sep 26 11:35:50 2013_coeffcients_surface.csv")
S0NO_S1high_tte_BIP0.20.8_right_curve<-read.csv("S0NO_S1high_tte_BIP0.20.8_rightThu Sep 26 11:35:40 2013_coeffcients_curve.csv")
S0NO_S1high_tte_BIP0.20.8_right_surface<-read.csv("S0NO_S1high_tte_BIP0.20.8_rightThu Sep 26 11:35:40 2013_coeffcients_surface.csv")
S0NO_S1high_tte_BIP0.2BSM0.5CPV_right_curve<-read.csv("S0NO_S1high_tte_BIP0.2BSM0.5CPV_rightThu Sep 26 11:40:39 2013_coeffcients_curve.csv")
S0NO_S1high_tte_BIP0.2BSM0.5CPV_right_surface<-read.csv("S0NO_S1high_tte_BIP0.2BSM0.5CPV_rightThu Sep 26 11:40:39 2013_coeffcients_surface.csv")
S0NO_S1high_tte_BIP0.2BSM0.5_right_curve<-read.csv("S0NO_S1high_tte_BIP0.2BSM0.5_rightThu Sep 26 11:33:16 2013_coeffcients_curve.csv")
S0NO_S1high_tte_BIP0.2BSM0.5_right_surface<-read.csv("S0NO_S1high_tte_BIP0.2BSM0.5_rightThu Sep 26 11:33:16 2013_coeffcients_surface.csv")
S0NO_S1high_tte_BIP0.80.2_right_curve<-read.csv("S0NO_S1high_tte_BIP0.80.2_rightThu Sep 26 11:36:00 2013_coeffcients_curve.csv")
S0NO_S1high_tte_BIP0.80.2_right_surface<-read.csv("S0NO_S1high_tte_BIP0.80.2_rightThu Sep 26 11:36:00 2013_coeffcients_surface.csv")
S0NO_S1high_tte_BIP0.80.8_right_curve<-read.csv("S0NO_S1high_tte_BIP0.80.8_rightThu Sep 26 11:33:37 2013_coeffcients_curve.csv")
S0NO_S1high_tte_BIP0.80.8_right_surface<-read.csv("S0NO_S1high_tte_BIP0.80.8_rightThu Sep 26 11:33:37 2013_coeffcients_surface.csv")
S0NO_S1high_tte_nothing_right_curve<-read.csv("S0NO_S1high_tte_nothing_rightThu Sep 26 11:29:14 2013_coeffcients_curve.csv")
S0NO_S1high_tte_nothing_right_surface<-read.csv("S0NO_S1high_tte_nothing_rightThu Sep 26 11:29:14 2013_coeffcients_surface.csv")
S0high_S1NO_binary_BIP0.20.2_right_curve<-read.csv("S0high_S1NO_binary_BIP0.20.2_rightThu Sep 26 11:47:51 2013_coeffcients_curve.csv")
S0high_S1NO_binary_BIP0.20.2_right_surface<-read.csv("S0high_S1NO_binary_BIP0.20.2_rightThu Sep 26 11:47:51 2013_coeffcients_surface.csv")
S0high_S1NO_binary_BIP0.20.8_right_curve<-read.csv("S0high_S1NO_binary_BIP0.20.8_rightThu Sep 26 11:47:33 2013_coeffcients_curve.csv")
S0high_S1NO_binary_BIP0.20.8_right_surface<-read.csv("S0high_S1NO_binary_BIP0.20.8_rightThu Sep 26 11:47:33 2013_coeffcients_surface.csv")
S0high_S1NO_binary_BIP0.2BSM0.5CPV_right_curve<-read.csv("S0high_S1NO_binary_BIP0.2BSM0.5CPV_rightThu Sep 26 11:48:29 2013_coeffcients_curve.csv")
S0high_S1NO_binary_BIP0.2BSM0.5CPV_right_surface<-read.csv("S0high_S1NO_binary_BIP0.2BSM0.5CPV_rightThu Sep 26 11:48:29 2013_coeffcients_surface.csv")
S0high_S1NO_binary_BIP0.2BSM0.5_right_curve<-read.csv("S0high_S1NO_binary_BIP0.2BSM0.5_rightThu Sep 26 11:47:17 2013_coeffcients_curve.csv")
S0high_S1NO_binary_BIP0.2BSM0.5_right_surface<-read.csv("S0high_S1NO_binary_BIP0.2BSM0.5_rightThu Sep 26 11:47:17 2013_coeffcients_surface.csv")
S0high_S1NO_binary_BIP0.80.2_right_curve<-read.csv("S0high_S1NO_binary_BIP0.80.2_rightThu Sep 26 11:48:07 2013_coeffcients_curve.csv")
S0high_S1NO_binary_BIP0.80.2_right_surface<-read.csv("S0high_S1NO_binary_BIP0.80.2_rightThu Sep 26 11:48:07 2013_coeffcients_surface.csv")
S0high_S1NO_binary_BIP0.80.8_right_curve<-read.csv("S0high_S1NO_binary_BIP0.80.8_rightThu Sep 26 11:45:44 2013_coeffcients_curve.csv")
S0high_S1NO_binary_BIP0.80.8_right_surface<-read.csv("S0high_S1NO_binary_BIP0.80.8_rightThu Sep 26 11:45:44 2013_coeffcients_surface.csv")
S0high_S1NO_binary_nothing_right_curve<-read.csv("S0high_S1NO_binary_nothing_rightThu Sep 26 11:45:35 2013_coeffcients_curve.csv")
S0high_S1NO_binary_nothing_right_surface<-read.csv("S0high_S1NO_binary_nothing_rightThu Sep 26 11:45:35 2013_coeffcients_surface.csv")
S0high_S1NO_tte_BIP0.20.2_right_curve<-read.csv("S0high_S1NO_tte_BIP0.20.2_rightThu Sep 26 11:40:27 2013_coeffcients_curve.csv")
S0high_S1NO_tte_BIP0.20.2_right_surface<-read.csv("S0high_S1NO_tte_BIP0.20.2_rightThu Sep 26 11:40:27 2013_coeffcients_surface.csv")
S0high_S1NO_tte_BIP0.20.8_right_curve<-read.csv("S0high_S1NO_tte_BIP0.20.8_rightThu Sep 26 11:32:12 2013_coeffcients_curve.csv")
S0high_S1NO_tte_BIP0.20.8_right_surface<-read.csv("S0high_S1NO_tte_BIP0.20.8_rightThu Sep 26 11:32:12 2013_coeffcients_surface.csv")
S0high_S1NO_tte_BIP0.2BSM0.5CPV_right_curve<-read.csv("S0high_S1NO_tte_BIP0.2BSM0.5CPV_rightThu Sep 26 11:30:20 2013_coeffcients_curve.csv")
S0high_S1NO_tte_BIP0.2BSM0.5CPV_right_surface<-read.csv("S0high_S1NO_tte_BIP0.2BSM0.5CPV_rightThu Sep 26 11:30:20 2013_coeffcients_surface.csv")
S0high_S1NO_tte_BIP0.2BSM0.5_right_curve<-read.csv("S0high_S1NO_tte_BIP0.2BSM0.5_rightThu Sep 26 11:30:53 2013_coeffcients_curve.csv")
S0high_S1NO_tte_BIP0.2BSM0.5_right_surface<-read.csv("S0high_S1NO_tte_BIP0.2BSM0.5_rightThu Sep 26 11:30:53 2013_coeffcients_surface.csv")
S0high_S1NO_tte_BIP0.80.2_right_curve<-read.csv("S0high_S1NO_tte_BIP0.80.2_rightThu Sep 26 11:32:37 2013_coeffcients_curve.csv")
S0high_S1NO_tte_BIP0.80.2_right_surface<-read.csv("S0high_S1NO_tte_BIP0.80.2_rightThu Sep 26 11:32:37 2013_coeffcients_surface.csv")
S0high_S1NO_tte_BIP0.80.8_right_curve<-read.csv("S0high_S1NO_tte_BIP0.80.8_rightThu Sep 26 11:40:30 2013_coeffcients_curve.csv")
S0high_S1NO_tte_BIP0.80.8_right_surface<-read.csv("S0high_S1NO_tte_BIP0.80.8_rightThu Sep 26 11:40:30 2013_coeffcients_surface.csv")
S0high_S1NO_tte_nothing_right_curve<-read.csv("S0high_S1NO_tte_nothing_rightThu Sep 26 11:42:58 2013_coeffcients_curve.csv")
S0high_S1NO_tte_nothing_right_surface<-read.csv("S0high_S1NO_tte_nothing_rightThu Sep 26 11:42:58 2013_coeffcients_surface.csv")
S0high_S1high_binary_BIP0.20.2_right_curve<-read.csv("S0high_S1high_binary_BIP0.20.2_rightThu Sep 26 11:43:33 2013_coeffcients_curve.csv")
S0high_S1high_binary_BIP0.20.2_right_surface<-read.csv("S0high_S1high_binary_BIP0.20.2_rightThu Sep 26 11:43:33 2013_coeffcients_surface.csv")
S0high_S1high_binary_BIP0.20.8_right_curve<-read.csv("S0high_S1high_binary_BIP0.20.8_rightThu Sep 26 11:44:17 2013_coeffcients_curve.csv")
S0high_S1high_binary_BIP0.20.8_right_surface<-read.csv("S0high_S1high_binary_BIP0.20.8_rightThu Sep 26 11:44:17 2013_coeffcients_surface.csv")
S0high_S1high_binary_BIP0.2BSM0.5CPV_right_curve<-read.csv("S0high_S1high_binary_BIP0.2BSM0.5CPV_rightThu Sep 26 11:44:11 2013_coeffcients_curve.csv")
S0high_S1high_binary_BIP0.2BSM0.5CPV_right_surface<-read.csv("S0high_S1high_binary_BIP0.2BSM0.5CPV_rightThu Sep 26 11:44:11 2013_coeffcients_surface.csv")
S0high_S1high_binary_BIP0.2BSM0.5_right_curve<-read.csv("S0high_S1high_binary_BIP0.2BSM0.5_rightThu Sep 26 11:43:56 2013_coeffcients_curve.csv")
S0high_S1high_binary_BIP0.2BSM0.5_right_surface<-read.csv("S0high_S1high_binary_BIP0.2BSM0.5_rightThu Sep 26 11:43:56 2013_coeffcients_surface.csv")
S0high_S1high_binary_BIP0.80.2_right_curve<-read.csv("S0high_S1high_binary_BIP0.80.2_rightThu Sep 26 11:44:01 2013_coeffcients_curve.csv")
S0high_S1high_binary_BIP0.80.2_right_surface<-read.csv("S0high_S1high_binary_BIP0.80.2_rightThu Sep 26 11:44:01 2013_coeffcients_surface.csv")
S0high_S1high_binary_BIP0.80.8_right_curve<-read.csv("S0high_S1high_binary_BIP0.80.8_rightThu Sep 26 11:43:40 2013_coeffcients_curve.csv")
S0high_S1high_binary_BIP0.80.8_right_surface<-read.csv("S0high_S1high_binary_BIP0.80.8_rightThu Sep 26 11:43:40 2013_coeffcients_surface.csv")
S0high_S1high_binary_nothing_right_curve<-read.csv("S0high_S1high_binary_nothing_rightThu Sep 26 11:42:03 2013_coeffcients_curve.csv")
S0high_S1high_binary_nothing_right_surface<-read.csv("S0high_S1high_binary_nothing_rightThu Sep 26 11:42:03 2013_coeffcients_surface.csv")
S0high_S1high_tte_BIP0.20.2_right_curve<-read.csv("S0high_S1high_tte_BIP0.20.2_rightThu Sep 26 11:36:15 2013_coeffcients_curve.csv")
S0high_S1high_tte_BIP0.20.2_right_surface<-read.csv("S0high_S1high_tte_BIP0.20.2_rightThu Sep 26 11:36:15 2013_coeffcients_surface.csv")
S0high_S1high_tte_BIP0.20.8_right_curve<-read.csv("S0high_S1high_tte_BIP0.20.8_rightThu Sep 26 11:37:05 2013_coeffcients_curve.csv")
S0high_S1high_tte_BIP0.20.8_right_surface<-read.csv("S0high_S1high_tte_BIP0.20.8_rightThu Sep 26 11:37:05 2013_coeffcients_surface.csv")
S0high_S1high_tte_BIP0.2BSM0.5CPV_right_curve<-read.csv("S0high_S1high_tte_BIP0.2BSM0.5CPV_rightThu Sep 26 11:35:04 2013_coeffcients_curve.csv")
S0high_S1high_tte_BIP0.2BSM0.5CPV_right_surface<-read.csv("S0high_S1high_tte_BIP0.2BSM0.5CPV_rightThu Sep 26 11:35:04 2013_coeffcients_surface.csv")
S0high_S1high_tte_BIP0.2BSM0.5_right_curve<-read.csv("S0high_S1high_tte_BIP0.2BSM0.5_rightThu Sep 26 11:35:09 2013_coeffcients_curve.csv")
S0high_S1high_tte_BIP0.2BSM0.5_right_surface<-read.csv("S0high_S1high_tte_BIP0.2BSM0.5_rightThu Sep 26 11:35:09 2013_coeffcients_surface.csv")
S0high_S1high_tte_BIP0.80.2_right_curve<-read.csv("S0high_S1high_tte_BIP0.80.2_rightThu Sep 26 11:36:35 2013_coeffcients_curve.csv")
S0high_S1high_tte_BIP0.80.2_right_surface<-read.csv("S0high_S1high_tte_BIP0.80.2_rightThu Sep 26 11:36:35 2013_coeffcients_surface.csv")
S0high_S1high_tte_BIP0.80.8_right_curve<-read.csv("S0high_S1high_tte_BIP0.80.8_rightThu Sep 26 11:36:43 2013_coeffcients_curve.csv")
S0high_S1high_tte_BIP0.80.8_right_surface<-read.csv("S0high_S1high_tte_BIP0.80.8_rightThu Sep 26 11:36:43 2013_coeffcients_surface.csv")
S0high_S1high_tte_nothing_right_curve<-read.csv("S0high_S1high_tte_nothing_rightThu Sep 26 11:35:45 2013_coeffcients_curve.csv")
S0high_S1high_tte_nothing_right_surface<-read.csv("S0high_S1high_tte_nothing_rightThu Sep 26 11:35:46 2013_coeffcients_surface.csv")
type1_binary_BIP0.20.2_right_curve<-read.csv("type1_binary_BIP0.20.2_rightThu Sep 26 11:24:03 2013_coeffcients_curve.csv")
type1_binary_BIP0.20.2_right_surface<-read.csv("type1_binary_BIP0.20.2_rightThu Sep 26 11:24:03 2013_coeffcients_surface.csv")
type1_binary_BIP0.20.8_right_curve<-read.csv("type1_binary_BIP0.20.8_rightThu Sep 26 11:34:22 2013_coeffcients_curve.csv")
type1_binary_BIP0.20.8_right_surface<-read.csv("type1_binary_BIP0.20.8_rightThu Sep 26 11:34:22 2013_coeffcients_surface.csv")
type1_binary_BIP0.2BSM0.5CPV_right_curve<-read.csv("type1_binary_BIP0.2BSM0.5CPV_rightThu Sep 26 11:35:42 2013_coeffcients_curve.csv")
type1_binary_BIP0.2BSM0.5CPV_right_surface<-read.csv("type1_binary_BIP0.2BSM0.5CPV_rightThu Sep 26 11:35:42 2013_coeffcients_surface.csv")
type1_binary_BIP0.2BSM0.5_right_curve<-read.csv("type1_binary_BIP0.2BSM0.5_rightThu Sep 26 11:35:04 2013_coeffcients_curve.csv")
type1_binary_BIP0.2BSM0.5_right_surface<-read.csv("type1_binary_BIP0.2BSM0.5_rightThu Sep 26 11:35:04 2013_coeffcients_surface.csv")
type1_binary_BIP0.80.2_right_curve<-read.csv("type1_binary_BIP0.80.2_rightThu Sep 26 11:29:31 2013_coeffcients_curve.csv")
type1_binary_BIP0.80.2_right_surface<-read.csv("type1_binary_BIP0.80.2_rightThu Sep 26 11:29:31 2013_coeffcients_surface.csv")
type1_binary_BIP0.80.8_right_curve<-read.csv("type1_binary_BIP0.80.8_rightThu Sep 26 11:18:17 2013_coeffcients_curve.csv")
type1_binary_BIP0.80.8_right_surface<-read.csv("type1_binary_BIP0.80.8_rightThu Sep 26 11:18:17 2013_coeffcients_surface.csv")
type1_binary_nothing_right_curve<-read.csv("type1_binary_nothing_rightThu Sep 26 11:12:22 2013_coeffcients_curve.csv")
type1_binary_nothing_right_surface<-read.csv("type1_binary_nothing_rightThu Sep 26 11:12:22 2013_coeffcients_surface.csv")
type1_tte_BIP0.20.2_right_curve<-read.csv("type1_tte_BIP0.20.2_rightWed Sep 25 18:28:37 2013_coeffcients_curve.csv")
type1_tte_BIP0.20.2_right_surface<-read.csv("type1_tte_BIP0.20.2_rightWed Sep 25 18:28:37 2013_coeffcients_surface.csv")
type1_tte_BIP0.20.8_right_curve<-read.csv("type1_tte_BIP0.20.8_rightWed Sep 25 18:28:55 2013_coeffcients_curve.csv")
type1_tte_BIP0.20.8_right_surface<-read.csv("type1_tte_BIP0.20.8_rightWed Sep 25 18:28:55 2013_coeffcients_surface.csv")
type1_tte_BIP0.2BSM0.5CPV_right_curve<-read.csv("type1_tte_BIP0.2BSM0.5CPV_rightWed Sep 25 18:31:29 2013_coeffcients_curve.csv")
type1_tte_BIP0.2BSM0.5CPV_right_surface<-read.csv("type1_tte_BIP0.2BSM0.5CPV_rightWed Sep 25 18:31:29 2013_coeffcients_surface.csv")
type1_tte_BIP0.2BSM0.5_right_curve<-read.csv("type1_tte_BIP0.2BSM0.5_rightWed Sep 25 18:32:26 2013_coeffcients_curve.csv")
type1_tte_BIP0.2BSM0.5_right_surface<-read.csv("type1_tte_BIP0.2BSM0.5_rightWed Sep 25 18:32:26 2013_coeffcients_surface.csv")
type1_tte_BIP0.80.2_right_curve<-read.csv("type1_tte_BIP0.80.2_rightWed Sep 25 18:28:06 2013_coeffcients_curve.csv")
type1_tte_BIP0.80.2_right_surface<-read.csv("type1_tte_BIP0.80.2_rightWed Sep 25 18:28:06 2013_coeffcients_surface.csv")
type1_tte_BIP0.80.8_right_curve<-read.csv("type1_tte_BIP0.80.8_rightWed Sep 25 18:27:11 2013_coeffcients_curve.csv")
type1_tte_BIP0.80.8_right_surface<-read.csv("type1_tte_BIP0.80.8_rightWed Sep 25 18:27:11 2013_coeffcients_surface.csv")
type1_tte_nothing_right_curve<-read.csv("type1_tte_nothing_rightWed Sep 25 18:21:58 2013_coeffcients_curve.csv")
type1_tte_nothing_right_surface<-read.csv("type1_tte_nothing_rightWed Sep 25 18:21:58 2013_coeffcients_surface.csv")


  output_curve<-matrix(rep(0, 56*17), nrow=56)
    output_surface<-matrix(rep(0, 56*25), nrow=56)

i<-41
apply(surface_est, 2, mean)

dim(surfaceout)

for(i in 1:dim(secnarios)[1]){
	namecall<-as.name(secnarios$noncharname[i])
	gettruebetas_out<-gettruebetas(secnarios[i,])
	truevalues_curve<-gettruebetas_out[7:10]
        truevalues_surface<-gettruebetas_out[1:6]

        
        curveoutput<-eval(as.name(paste(namecall,"_curve", sep="")))
        surfaceout<-eval(as.name(paste(namecall,"_surface", sep="")))
        
        curve_est<-curveoutput[,1:(dim(curveoutput)/2)[2]]
        curve_Se<-curveoutput[,((dim(curveoutput)/2)[2]+1):dim(curveoutput)[2]]
        
        surface_est<-surfaceout[,1:(dim(surfaceout)/2)[2]]
        surface_Se<-surfaceout[,((dim(surfaceout)/2)[2]+1):dim(surfaceout)[2]]
       
       meanbias_est_curve<-apply(curve_est, 2, mean)-t(truevalues_curve)
       meanbias_est_surface<-apply(surface_est, 2, mean)-t(truevalues_surface)
       
       
       meandiff_Se_curve<-apply(curve_Se, 2, mean)-sqrt(apply(curve_est, 2, var))
       meandiff_Se_surface<-apply(surface_Se, 2, mean)-sqrt(apply(surface_est, 2, var))
       
       coverage_curve_se<-matrix(rep(0, 1000*4), ncol=4)
       coverage_surface_se<-matrix(rep(0, 1000*6), ncol=6)
for(j in 1:4){
  upper<-curve_est+curve_Se*1.97
  lower<-curve_est-curve_Se*1.97
  coverage_curve_se[,j]<-ifelse(upper[,j]>truevalues_curve[j]&lower[,j]<truevalues_curve[j], 1, 0)
  }
  coverage_curve_se_out<-apply(coverage_curve_se, 2, mean)
for(j in 1:6){
  upper<-surface_est+surface_Se*1.97
  lower<-surface_est-surface_Se*1.97
  coverage_surface_se[,j]<-ifelse(upper[,j]>truevalues_surface[j]&lower[,j]<truevalues_surface[j], 1, 0)
  }
  coverage_surface_se_out<-apply(coverage_surface_se, 2, mean)
  
  output_curve[i,]<-cbind(secnarios$noncharname[i], t(sqrt(apply(curve_est, 2, var))), (meanbias_est_curve),  t(meandiff_Se_curve), t(coverage_curve_se_out))
  output_surface[i,]<-cbind(secnarios$noncharname[i], t(sqrt(apply(surface_est, 2, var))), (meanbias_est_surface),t(meandiff_Se_surface),t(coverage_surface_se_out))
}

write.csv( output_surface, ' output_surface.csv', row.names=FALSE)















