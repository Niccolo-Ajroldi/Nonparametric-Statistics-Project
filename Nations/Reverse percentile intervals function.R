
reverse_percentile_interval <- function(T.obs, T.boot, alpha){
  
  
  right.quantile <- quantile(T.boot, 1 - alpha/2)
  left.quantile  <- quantile(T.boot, alpha/2)
  
  
  
  CI.RP <- c(T.obs - (right.quantile - T.obs), 
             mean (c(T.obs - (right.quantile - T.obs),T.obs - (left.quantile - T.obs))), 
             T.obs - (left.quantile - T.obs))
  
  
  plot(ecdf(T.boot))
  abline(v=T.obs, lty=2)
  abline(v = CI.RP, col='red')
  
  return(CI.RP)
  
}