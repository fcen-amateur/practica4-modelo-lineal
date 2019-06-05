intervalo_beta_asympt<-function(lm_object,data,j,alpha){
  n=length(lm_object$residuals)
  X=model.matrix(lm_object$terms,data)
  hmatrix=solve((t(X)%*%X)/n)
  r=qr(X)$rank
  s_cuadrado=sum(t(lm_object$residuals)%*%lm_object$residuals)/(n-r)
  dif=qnorm(alpha/2)*sqrt(s_cuadrado*hmatrix[j,j]/n)
  c1=lm_object$coefficients[j]-dif
  c2=lm_object$coefficients[j]+dif
  return(sort(c(c1,c2),decreasing = FALSE))
}
