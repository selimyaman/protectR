#' Get Coefficients and Credible Intervals from BPL Model
#'
#' This function extracts the model coefficients and calculates their credible intervals, also checking if zero is contained within these intervals.
#'
#' @param model A list containing at least 'beta_post' and 'mean_beta', which are the posterior distributions and mean values of the coefficients, respectively.
#' @param df A dataframe containing the predictor variables and the outcome variable.
#' @param outcome_var The name of the outcome variable in the dataframe.
#' @param method The method used to calculate the credible intervals (defaults to "HDI" - Highest Density Interval).
#'
#' @return A dataframe with the names of the predictor variables, mean of the beta coefficients, lower and upper bounds of the credible intervals, and a logical flag indicating if zero is contained within the interval.
#'
#' @examples
#' n <- 50
#' x1 <- sample(c("A", "B"), n, replace = TRUE)
#' x1[sample(n, 10)] <- NA  # add missing values to the first column
#' x2 <- rnorm(n)
#' x2[sample(n, 5)] <- NA  # add missing values to the second column
#' x3 <- rnorm(n)
#' y <- as.numeric(x1 == "A") + x2 + rnorm(n)
#' mydata <- data.frame(x1, x2, x3, y)
#' model <- bayesian_lasso(df=mydata, outcome_var = "y", protected_vars = c("X1"))
#' get_coefficients(model, mydata, "y")
#'
#' @importFrom bayestestR ci
#' @export
#'
get_coefficients <- function(model, df, outcome_var, method = "HDI") {
  
  # Validate inputs
  if (!is.data.frame(df)) {
  stop("df must be a data.frame")
} 
  # Determine the names of the predictor variables
  predictor_names <- names(df) %>%
    setdiff(outcome_var) %>%
    keep(~ is.numeric(df[[.]]))
  
  # Calculate credible intervals for each beta
  n_vars <- length(predictor_names)
  lower_bound <- c()
  upper_bound <- c()
  HDI <- list()
  for (i in 1:n_vars) {
    HDI[[i]] <- bayestestR::ci(model$beta_post[, i], method = method)
    lower_bound[i] <- HDI[[i]]$CI_low
    upper_bound[i] <- HDI[[i]]$CI_high
  }

  # Check if credible intervals contain 0
  is.there.zero <- c()
  for (i in 1:n_vars) {
    is.there.zero[i] <- cbind(lower_bound[i] <= 0 & upper_bound[i] >= 0)
  }

  # Create dataframe for plotting
  df1 <- data.frame(names = predictor_names,
                    mean_beta = model$mean_beta,
                    lower_bound,
                    upper_bound,
                    is.there.zero)
  return(df1)
}