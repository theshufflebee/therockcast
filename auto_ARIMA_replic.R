# Auto ARIMA Replication
# Replicates core functionality of forecast::auto.arima()

library(stats)

my.auto.arima <- function(y, max.p=5, max.q=5, max.d=5)
  {
  T <- length(y)
  d <- 0
  max.p <- max.p
  max.d <- max.d
  max.q <- max.q
  matrix.AIC <- matrix(NaN,max.p+1,max.q+1)
  matrix.BIC <- matrix(NaN,max.p+1,max.q+1)
  matrix.AICc <- matrix(NaN,max.p+1,max.q+1)
  for(p in 0:max.p){
    for(q in 0:max.q){
      fit.arima <- arima(y,order=c(p+1,d,q+1),method="ML")
      matrix.AIC[p+1,q+1] <- -2*fit.arima$loglik + (p+q+d+2)*2
      matrix.BIC[p+1,q+1] <- -2*fit.arima$loglik + (p+q+d+2)*log(T)
      matrix.AICc[p+1,q+1] <- -2*fit.arima$loglik + (p+q+d+2)*2 + (2 * (p+q+d+2)^2 + 2 * (p+q+d+2)) / (T - (p+q+d+2) - 1)
    }
    }
  min_AIC <- min(matrix.AIC)
  min_BIC <- min(matrix.BIC)
  min_AICc <- min(matrix.AICc)
  
  position_AIC <- which(matrix.AIC == min_AIC, arr.ind = TRUE)
  position_BIC <- which(matrix.BIC == min_BIC, arr.ind = TRUE)
  position_AICc <- which(matrix.AICc == min_AICc, arr.ind = TRUE)
  
  opt.p <- min(position_AIC[1],position_BIC[1],position_AICc[1])
  opt.q <- min(position_AIC[2],position_BIC[2],position_AICc[2])
  
  
  vec.AIC <- NULL
  vec.BIC <- NULL
  vec.AICc  <- NULL
  for(d in 0:max.d){
    fit.arima <- arima(y,order=c(opt.p,d,opt.q),method="ML")
    vec.AIC <- c(vec.AIC,
                 -2*fit.arima$loglik + (p+q+d)*2)
    vec.BIC <- c(vec.BIC,
                 -2*fit.arima$loglik + (p+q+d)*log(T))
    vec.AICc <- c(vec.AICc,
                -2*fit.arima$loglik + (p+q+d)*2 + (2 * (p+q+d)^2 + 2 * (p+q+d)) / (T - (p+q+d) - 1))
  }
  min_v_AIC <- min(vec.AIC)
  min_v_BIC <- min(vec.BIC)
  min_v_AICc <- min(vec.AICc)
  
  position_v_AIC <- which(vec.AIC == min_v_AIC, arr.ind = TRUE)
  position_v_BIC <- which(vec.BIC == min_v_BIC, arr.ind = TRUE)
  position_v_AICc <- which(vec.AICc == min_v_AICc, arr.ind = TRUE)

  opt.d <- min(position_v_AIC,position_v_BIC,position_v_AICc) - 1
  
  parameters <- c(opt.p, opt.d, opt.q)
  
  model <- arima(y, order = parameters)
  cat(sprintf("ARIMA(%d,%d,%d)", parameters[1], parameters[2], parameters[3]))
  print(model)
}


## To replicate functionality of forecast function 
my.forecast = function(model,h=1){
  prediction_output = predict(object=model, n.ahead=h)
  values <- as.vector(prediction_output$pred)
  return(values)
}





