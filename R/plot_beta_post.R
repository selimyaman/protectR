#' Plot Posterior Distributions for Model Coefficients
#'
#' This function takes a model object and a dataframe, identifies predictor variables, calculates credible intervals for coefficients, and produces a plot of these statistics.
#'
#' @param model A model object containing at least a `beta_post` matrix with posterior distributions of the coefficients.
#' @param df A dataframe containing the predictor variables and the outcome variable.
#' @param outcome_var The name of the outcome variable in the dataframe.
#' @param color The color to be used for points and lines in the plot.
#' @param linewidth The width of the line to be used in the plot.
#' @param linetype The type of the line to be used in the plot.
#' @param method The method used to calculate credible intervals (defaults to "ETI" - Equal-tailed Interval).
#'
#' @return A ggplot object representing the Bayesian Lasso Coefficients with their credible intervals.
#'
#' @examples
#' # Assuming 'model' is generated using bayesian_lasso function
#' # and 'df' is a dataframe with the required structure
#' # plot_beta_post(model = result1, df = mydata, outcome_var = "y")
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_linerange scale_y_continuous coord_flip theme_minimal theme element_blank ggtitle
#' @importFrom dplyr setdiff
#' @importFrom purrr keep
#' @importFrom bayestestR ci
#' @export
#'
plot_beta_post <- function(model, df, outcome_var, color = "blue", linewidth = 0.7,
                           linetype = "dashed", method = "ETI") {
  
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
  
  # Plot betas and credible intervals
  ggplot(df1, aes(x = reorder(names, -mean_beta), y = mean_beta)) +
    geom_point(size = 3, color = color) +
    geom_linerange(aes(ymin = lower_bound, ymax = upper_bound),
                   color = color, linewidth = linewidth, linetype = linetype) +
    scale_y_continuous(limits = c(min(df1$lower_bound), max(df1$upper_bound))) +
    coord_flip() +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle("Bayesian Lasso Coefficients")
}