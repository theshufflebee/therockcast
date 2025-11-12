# You must have the 'tseries' package installed
# You can install it by running: install.packages("tseries")
library(tseries)
library(stats)

#' Finds the order of differencing (d) using sequential KPSS tests.
#'
#' This replicates the 'ndiffs' function from the 'forecast' package.
#'
#' @param y The input time series.
#' @param max.d The maximum order of differencing to test.
#' @param kpss.alpha The significance level for the KPSS test.
#'
#' @return A list containing:
#'         $d: The recommended order of differencing (e.g., 0, 1, 2)
#'         $stationary.series: The resulting series after differencing d times.
#'
find.d <- function(y, max.d = 2, kpss.alpha = 0.05) {
  
  nd <- 0 # Start with d=0
  x <- y  # Start with the original series
  
  # Check if the original series is stationary
  kpss_stat <- try(kpss.test(x, null = "Level")$p.value, silent = TRUE)
  
  if (inherits(kpss_stat, "try-error")) {
    warning("KPSS test failed on original series. Assuming d=0.")
    return(list(d = 0, stationary.series = y))
  }
  
  # If p-value is low, it's non-stationary. Start differencing.
  if (kpss_stat < kpss.alpha) {
    
    # Loop from d=1 up to max.d
    for (i in 1:max.d) {
      nd <- i # Tentatively set d=i
      x <- diff(y, differences = i) # Always difference from original 'y'
      
      # Test the differenced series
      kpss_stat <- try(kpss.test(x, null = "Level")$p.value, silent = TRUE)
      
      if (inherits(kpss_stat, "try-error")) {
        # Test failed, likely too few observations
        warning(paste("KPSS test failed at d =", i, ". Using d =", i - 1))
        nd <- i - 1 # Use the previous d
        break
      }
      
      if (kpss_stat >= kpss.alpha) {
        # The series is now stationary. We found our d.
        break # Exit the loop
      }
      
      # If we get here, it's still not stationary.
      # The loop will continue, or if i=max.d, it will end,
      # and 'nd' will correctly be max.d.
    }
  }
  
  # 'nd' now holds our final d value
  # Get the final stationary series
  if (nd == 0) {
    final.series <- y
  } else {
    final.series <- diff(y, differences = nd)
  }
  
  return(list(d = nd, stationary.series = final.series))
}


#' A replication of the core auto.arima() logic
#'
#' This function first finds the best 'd' using sequential KPSS tests,
#' then searches for the best 'p' and 'q' on the differenced data.
#'
#' @param y The input time series.
#' @param max.p Max AR order.
#' @param max.q Max MA order.
#' @param max.d Max differencing order.
#' @param criterion "aicc" (default), "aic", or "bic".
#'
#' @return An 'Arima' object containing the best-fitting model.
#'
my.auto.arima <- function(y, max.p = 5, max.q = 5, max.d = 2, criterion = "aicc") {
  
  # --- STEP 1: Determine 'd' using unit root tests ---
  differencing_info <- find.d(y, max.d = max.d, kpss.alpha = 0.05)
  opt.d <- differencing_info$d
  stationary_y <- differencing_info$stationary.series
  
  cat(sprintf("KPSS test determined optimal d = %d\n", opt.d))
  
  # Get effective sample size for p/q search
  T_eff <- length(stationary_y)
  
  # Initialize trackers
  best_criterion_value <- Inf
  best_p <- 0
  best_q <- 0
  
  # --- STEP 2: Find p and q on the *stationary* data ---
  # We loop p and q, fitting ARIMA(p, 0, q) to the *differenced* data
  
  for (p in 0:max.p) {
    for (q in 0:max.q) {
      
      # Fit model to *stationary* data, so d=0
      fit <- try(
        arima(stationary_y, order = c(p, 0, q), method = "ML"),
        silent = TRUE
      )
      
      if (inherits(fit, "try-error")) {
        next # Skip this (p,q) combo
      }
      
      # Calculate the chosen criterion
      n <- fit$nobs # Effective sample size
      k <- length(fit$coef) + 1 # num parameters + 1 for sigma^2
      loglik <- fit$loglik
      aic <- -2 * loglik + 2 * k
      
      current_criterion_value <- NA
      
      if (criterion == "aic") {
        current_criterion_value <- aic
        
      } else if (criterion == "bic") {
        current_criterion_value <- -2 * loglik + k * log(n)
        
      } else { # Default to "aicc"
        # Check if AICc can be calculated
        if (n - k - 1 > 0) {
          current_criterion_value <- aic + (2 * k^2 + 2 * k) / (n - k - 1)
        } else {
          current_criterion_value <- Inf # Penalize models where AICc can't be found
        }
      }
      
      # Check if this model is better
      if (current_criterion_value < best_criterion_value) {
        best_criterion_value <- current_criterion_value
        best_p <- p
        best_q <- q
      }
      
    } # end q loop
  } # end p loop
  
  cat(sprintf("Grid search found optimal p = %d, q = %d\n", best_p, best_q))
  
  # --- STEP 3: Create the final model ---
  # Fit the *one* best model using the *original data 'y'*
  # and the full order (best_p, opt.d, best_q).
  # This gives the correct log-likelihood and model object.
  
  final_order <- c(best_p, opt.d, best_q)
  
  final_model <- try(
    arima(y, order = final_order, method = "ML"),
    silent = TRUE
  )
  
  if (inherits(final_model, "try-error")) {
    warning("Could not fit the final model. Returning NULL.")
    return(NULL)
  }
  
  cat(sprintf(
    "--- Best model: ARIMA(%d, %d, %d) ---\n",
    final_order[1],
    final_order[2],
    final_order[3]
  ))
  
  return(final_model)
}


## Your forecast function (which is correct)
my.forecast = function(model, h = 1) {
  prediction_output = predict(object = model, n.ahead = h)
  values <- as.vector(prediction_output$pred)
  return(values)
}