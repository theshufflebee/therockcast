#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This first small function just enables easy automatic interpretations of
#   the final selected models.
explain_arima <- function(model_object, var_name) {
  
  var_name = var_name
  
  # Extract the resulting model
  vec <- model_object$arma
  
  # Get the specification for p,d,q and put in string
  p <- vec[1]
  q <- vec[2]
  d <- vec[6]
  spec_str <- paste0("ARIMA(", p, ", ", d, ", ", q, ")")
  
  # Make it legible 
  result <- paste0(
    "This model is fitted to the variable '", var_name, 
    "' specified as ", spec_str, ".")
  return(result) }



#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This function searches for the best p and q for a *fixed* d (unnecessary 
#   to loop over d in our project since we either set d=0 for output gap 
#    or d=1 for interest and inflation)

my.auto.arima <- function(x, max.p, max.q, d, var_name="x") {
  
  # Get the length of the time series
  # We use n = length(x) - d as the effective sample size for BIC/AICc
  # Note: arima()$nobs will give the exact number used.
  T_orig <- length(x) 
  
  # Initialize matrices to store the criteria values
  matrix.AIC <- matrix(NA, max.p + 1, max.q + 1)
  matrix.BIC <- matrix(NA, max.p + 1, max.q + 1)
  matrix.AICc <- matrix(NA, max.p + 1, max.q + 1)
  
  # Add row and column names for clarity
  rownames(matrix.AICc) <- 0:max.p
  colnames(matrix.AICc) <- 0:max.q
  rownames(matrix.AIC) <- 0:max.p
  colnames(matrix.AIC) <- 0:max.q
  rownames(matrix.BIC) <- 0:max.p
  colnames(matrix.BIC) <- 0:max.q
  

  # Loop over all p from 0 to max.p
  for(p in 0:max.p) {
    # Loop over all q from 0 to max.q
    for(q in 0:max.q) {
      
      # tryCatch to handle errors 
      fit.arima <- try(
        arima(x, order = c(p, d, q), method = "ML"),
        silent = TRUE)
      
      # Check if the fit was successful
      if(!inherits(fit.arima, "try-error")) {
        
        # Get log-likelihood
        loglik <- fit.arima$loglik
        
        # Get number of parameters.
        # This includes p, q, and the mean (if d=0), plus the variance (sigma^2).
        k <- length(fit.arima$coef) + 1 
        
        # Get effective number of observations
        n <- fit.arima$nobs 
        
        # Calculate AIC
        aic_val <- -2 * loglik + 2 * k
        matrix.AIC[p + 1, q + 1] <- aic_val
        
        # Calculate BIC
        matrix.BIC[p + 1, q + 1] <- -2 * loglik + k * log(n)
        
        # Calculate AICc (AIC corrected for small samples)
        # We add a check for n - k - 1 > 0 to avoid division by zero
        if (n - k - 1 > 0) {
          matrix.AICc[p + 1, q + 1] <- aic_val + (2 * k^2 + 2 * k) / (n - k - 1)
        }}}}
  
  # Find the p and q that minimize the AICc
  # 'na.rm=TRUE' jut in case of fails
  min_AICc <- min(matrix.AICc, na.rm = TRUE)
  position_AICc <- which(matrix.AICc == min_AICc, arr.ind = TRUE)
  
  # Get the first match if there are ties
  opt.p <- position_AICc[1, 1] - 1  # -1 to convert from 1-based index to p-value
  opt.q <- position_AICc[1, 2] - 1  # -1 to convert from 1-based index to q-value
  
  # Store the optimal parameters
  parameters <- c(opt.p, d, opt.q)
  
  final.model <- arima(x, order = parameters, method = "ML")
  
  # Print the model summary
  print(explain_arima(final.model, var_name = var_name))
  
  # Return a list with all the results
  return(final.model)}



#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# This function produces forecasts from the my.auto.arima output

my.forecast = function(model, h = 1) {
  prediction_output = predict(object = model, n.ahead = h)
  values <- as.vector(prediction_output$pred)
  return(values)  }



#--------------------------------------------------------------------------------
