



mice.impute.survweibull <- function(y, ry, x, ...){
  require(MASS)
  require(survival)
  y.name <- paste("Y", strsplit(grep("D", colnames(x), value = TRUE), ".", fixed = T)[[1]][2], sep= ".")
  dat.temp <- data.frame(y, x)
  colnames(dat.temp)[1] <- y.name
  form <- paste("Surv(", y.name, ",", gsub("Y", "D", y.name), ") ~ S.0 + S.1")
  if("W" %in% colnames(x)){ form <- paste(form, " + W") }
  fit.y <- survreg(as.formula(form), data = dat.temp[ry,], dist ="weibull")
  num<-length(fit.y$coeff)+1
  stars.out <- mvrnorm(1, mu = c(fit.y$coeff,log(fit.y$scale)) Sigma = vcov(fit.y))
  scale.star <- stars.out[num]
  beta.star<-stars.out[-num]
  fit.y$coeff <- beta.star
  fit.y$scale <-exp(scale.star)
  lambda <- predict(fit.y, newdata = dat.temp[!ry,], type = "lp")
  return(rweibull(length(lambda), shape=(1/fit.y$scale), scale=exp(lambda)))
  
}

