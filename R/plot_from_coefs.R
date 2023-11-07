#' Plot Coefficients from a Dataframe
#'
#' This function takes a dataframe containing names, mean_beta, lower_bound, and upper_bound for model coefficients and produces a plot of these statistics.
#'
#' @param df A dataframe containing the names of the predictor variables and the corresponding statistics: mean_beta, lower_bound, upper_bound.
#' @param color The color to be used for points and lines in the plot.
#' @param linewidth The width of the line to be used in the plot.
#' @param linetype The type of the line to be used in the plot.
#' @param ylim Optional vector of length 2 to define the y-axis limits.
#'
#' @return A ggplot object representing the Bayesian Lasso Coefficients with their credible intervals.
#'
#' @examples
#' # Assuming 'df' is a dataframe produced by the `get_coefficients` function
#' # NOT RUN
#' # plot_from_coefs(df = coefficient_df)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_linerange coord_flip theme_minimal theme element_blank ggtitle
#' @export
plot_from_coefs <- function(df, color = "blue", linewidth = 0.7,
                            linetype = "dashed", ylim = NULL) {
  # Plot betas and credible intervals
  p <- ggplot(df, aes(x = reorder(names, -mean_beta), y = mean_beta)) +
    geom_point(size = 3, color = color)
  
  if (!is.null(ylim)) {
    p <- p + ylim(ylim)
  }
  
  p <- p + geom_linerange(aes(ymin = lower_bound, ymax = upper_bound),
                          color = color, linewidth = linewidth, linetype = linetype) +
    coord_flip() +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle("Bayesian Lasso Coefficients")
  
  return(p)
}
