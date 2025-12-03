#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function replicates the dm.test function from the {forecast} package

dm_test <- function(e1, e2, alternative = c("two.sided", "less", "greater"), 
                    h = 1) {
  
  # 1. Setup and Arguments
  alternative <- match.arg(alternative)
  h <- as.integer(h)
  
  # 2. Calculate Loss Differential
  d <- e1^2 - e2^2
  P <- length(d) 
  
  # 3. Calculate Variance-Covariance Matrix
  d.cov <- acf(d, na.action = na.omit, lag.max = h - 1, type = "covariance", 
               plot = FALSE)$acf[, , 1]
  
  # Variance estimator formula
  d.var <- sum(c(d.cov[1], 2 * d.cov[-1])) / P
  
  # 4. Logic for Negative Variance
  # If variance is negative/zero for h>1, fall back to h=1
  # We couldn't manage to run our code without including this part
  # (taken from the base package)
  # Probable reason is that our R starts quite "late" 
  if (d.var > 0) {
    STATISTIC <- mean(d, na.rm = TRUE) / sqrt(d.var)
  } else if (h == 1) {
    stop("Variance of DM statistic is zero")
  } else {
    warning("Variance is negative. Proceeding with horizon h=1.")
    return(dm.test(e1, e2, alternative = alternative, h = 1))
  }
  
  # 5. Apply Harvey-Leybourne-Newbold correction (like in slides)
  #     (corrects for small samples, useful for us)
  k <- ((P + 1 - 2 * h + (h / P) * (h - 1)) / P)^(1/2)
  STATISTIC <- STATISTIC * k
  
  # 6. pvalue with student distribution (as seen in class)
  #     use (P-1) degrees of freedom
  if (alternative == "two.sided") {
    PVAL <- 2 * pt(-abs(STATISTIC), df = P - 1)
  } else if (alternative == "less") {
    PVAL <- pt(STATISTIC, df = P - 1)
  } else if (alternative == "greater") {
    PVAL <- pt(STATISTIC, df = P - 1, lower.tail = FALSE)
  }
  
  # 7. Output 
  structure(list(
    alternative = alternative, 
    p.value = PVAL, 
    method = "Diebold-Mariano Test", 
    data.name = c(deparse(substitute(e1)), deparse(substitute(e2)))
  ), class = "htest")
}

#--------------------------------------------------------------------------------
