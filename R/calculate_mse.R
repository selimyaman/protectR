#' Calculate MSE for Training and Testing Sets
#'
#' This function computes the Mean Squared Error (MSE) for the training and testing datasets based on the specified model's coefficients.
#'
#' @param model A list containing the `mean_beta` vector which represents the model's coefficients.
#' @param train A dataframe representing the training set with all predictor variables and the outcome variable.
#' @param y The name of the outcome variable in the train and test datasets.
#' @param test A dataframe representing the testing set with all predictor variables and the outcome variable.
#'
#' @return A named vector with the MSE for the training set (`MSE_training`) and the testing set (`MSE_testing`).
#'
#' @examples
#' # NOT RUN
#' # calculate_mse(model = model1, train = train, test = test, y = "OutcomeVariableName")
#'
#' @export
#' @importFrom dplyr select
#' @importFrom stats sd
calculate_mse <- function(model, train, y, test) {
  # Helper function to standardize data
  standardize <- function(oX, oY) {
    J <- matrix(1, nrow = nrow(oX), ncol = 1)
    epsilon <- 1e-10
    X <- (oX - J %*% colMeans(oX)) / (J %*% apply(oX, 2, sd) + epsilon)
    Y_3 <- (oY - colMeans(oY)) / (apply(oY, 2, sd))
    list(X = X, Y_3 = Y_3)
  }
  
  # Standardize training data
  oX_train <- as.matrix(train %>% select(-y))
  oY_train <- as.matrix(train %>% select(y))
  train_standardized <- standardize(oX_train, oY_train)
  
  # Predict and calculate MSE for training data
  yhat_train <- train_standardized$X %*% model$mean_beta
  MSE_training <- mean((yhat_train - train_standardized$Y_3) ^ 2)
  
  # Standardize testing data
  oX_test <- as.matrix(test %>% select(-y))
  oY_test <- as.matrix(test %>% select(y))
  test_standardized <- standardize(oX_test, oY_test)
  
  # Predict and calculate MSE for testing data
  yhat_test <- test_standardized$X %*% model$mean_beta
  MSE_testing <- mean((yhat_test - test_standardized$Y_3) ^ 2)
  
  # Return MSE values
  MSE_values <- c(MSE_training, MSE_testing)
  names(MSE_values) <- c("MSE_training", "MSE_testing")
  return(MSE_values)
}
