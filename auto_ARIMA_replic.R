library(stats)

my.auto.arima <- function(y, max.p = 4, max.q = 4, max.d = 0, criterion = "aicc") {
  T <- length(y)
  d <- max.d
  
  # 1. Fix: Initialize matrices with +1 dimension for p=0, q=0
  matrix.AIC <- matrix(Inf, max.p + 1, max.q + 1)
  matrix.BIC <- matrix(Inf, max.p + 1, max.q + 1)
  matrix.AICc <- matrix(Inf, max.p + 1, max.q + 1)
  
  # Set row/col names for clarity (optional but helpful)
  rownames(matrix.AICc) <- paste0("p=", 0:max.p)
  colnames(matrix.AICc) <- paste0("q=", 0:max.q)
  
  for (p in 0:max.p) {
    for (q in 0:max.q) {
      
      # 5. Fix: Use tryCatch to handle model fit errors
      fit.arima <- tryCatch({
        arima(y, order = c(p, d, q), method = "ML")
      }, error = function(e) {
        NULL # Return NULL if arima() fails
      })
      
      if (!is.null(fit.arima)) {
        # 3. Fix: Use correct parameter count 'k' and built-in functions
        k <- fit.arima$n.param
        
        # Use built-in AIC/BIC
        current_AIC <- stats::AIC(fit.arima)
        current_BIC <- stats::BIC(fit.arima)
        
        # Calculate AICc from AIC
        # Add a check for T - k - 1 <= 0, which makes AICc undefined
        if (T - k - 1 > 0) {
          current_AICc <- current_AIC + (2 * k^2 + 2 * k) / (T - k - 1)
        } else {
          current_AICc <- Inf # Penalize models that use too many parameters
        }
        
        # 1. Fix: Store results at [p + 1, q + 1]
        matrix.AIC[p + 1, q + 1] <- current_AIC
        matrix.BIC[p + 1, q + 1] <- current_BIC
        matrix.AICc[p + 1, q + 1] <- current_AICc
      }
      # If fit.arima was NULL (failed), the value remains Inf
    }
  }
  
  # 2. Fix: Select ONE criterion matrix to find the minimum
  matrix_to_use <- switch(tolower(criterion),
                          "aic" = matrix.AIC,
                          "bic" = matrix.BIC,
                          "aicc" = matrix.AICc,
                          stop("Invalid criterion specified. Use 'aic', 'bic', or 'aicc'.")
  )
  
  min_val <- min(matrix_to_use, na.rm = TRUE)
  if (min_val == Inf) {
    stop("Could not fit any valid ARIMA models.")
  }
  
  # 4. Fix: Subtract 1 from row/col index to get p/q
  position <- which(matrix_to_use == min_val, arr.ind = TRUE)
  
  # Handle ties by choosing the simplest model (lowest p+q)
  if(nrow(position) > 1) {
    # Calculate sum of p and q (indices - 1) and find row with min sum
    sums <- (position[,1] - 1) + (position[,2] - 1)
    position <- position[which.min(sums), , drop = FALSE]
  }
  
  opt.p <- position[1, 1] - 1
  opt.q <- position[1, 2] - 1
  parameters <- c(opt.p, d, opt.q)
  
  # Refit the final best model
  best_model <- arima(y, order = parameters, method = "ML")
  
  cat(sprintf("Best model (using %s): ARIMA(%d,%d,%d)\n", 
              toupper(criterion), parameters[1], parameters[2], parameters[3]))
  
  # Return a list containing the model and the criteria matrices
  return(list(
    model = best_model,
    parameters = parameters,
    AIC_matrix = matrix.AIC,
    BIC_matrix = matrix.BIC,
    AICc_matrix = matrix.AICc
  ))
}

# Forecast
my.forecast = function(model, h = 1) {
  prediction_output = predict(object = model, n.ahead = h)
  values <- as.vector(prediction_output$pred)
  return(values)
}