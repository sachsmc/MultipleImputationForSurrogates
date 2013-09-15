
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
