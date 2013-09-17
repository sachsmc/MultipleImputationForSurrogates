
impute.data <- function(trial.data){
  require(mice)
  
  ## impute S0 and S1  ~  W
  
  iter0.mice <- mice(trial.data[,c("S", "W")], m = 1, print = FALSE,
                     method = "norm", predictorMatrix = matrix(c(0, 0,  1, 0), nrow =2))
  
  trial.data$S.0[trial.data$Z == 0] <- complete(iter0.mice)$S[trial.data$Z == 0]

  iter0b.mice <- mice(trial.data[,c("S.0", "S.1", "W")], m = 1, print = FALSE,
                      method = "norm", 
                      predictorMatrix = matrix(c(rep(0, 6),1,1,0), nrow = 3, ncol =3))
  
  trial.data[,c("S.0", "S.1")] <- complete(iter0b.mice)[,c("S.0", "S.1")]
  
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
