library(testthat)
# Install packages if they are not already installed
packages <- c("dplyr", "monomvn", "lars", "SuppDists", "MCMCpack", 
              "grplasso", "dae", "MASS", "tidymodels", "ggplot2", "bayestestR")

# Use a loop to install any packages that are not already installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the libraries
library(dplyr)
library(monomvn)
library(lars)
library(MCMCpack)
library(grplasso)
library(dae)
library(MASS)
library(tidymodels)
library(ggplot2)
library(bayestestR)
library(SuppDists)

# test_that("Bayesian Lasso handles NA values correctly", {
#   # Generate some test data with NAs
#   set.seed(123)
#   n <- 50
#   x1 <- sample(c("A", "B"), n, replace = TRUE)
#   x1[sample(n, 10)] <- NA  # add missing values to the first column
#   x2 <- rnorm(n)
#   x2[sample(n, 5)] <- NA  # add missing values to the second column
#   x3 <- rnorm(n)
#   y <- as.numeric(x1 == "A") + x2 + rnorm(n)
#   test_df <- data.frame(x1, x2, x3, y)

#   # Test if the warning message for NAs is correct
#   expect_message(bayesian_lasso(df=test_df, outcome_var="y"), "Your dataset has NAs")

#   # Check if the function returns correct number of rows after listwise deletion
#   result <- bayesian_lasso(df=test_df, outcome_var="y")
#   expect_true(nrow(test_df) > nrow(result$beta_post))
# })




# test_that("Bayesian Lasso returns correct output structure", {
#   # Generate some data
#   set.seed(123)
#   n <- 50
#   x1 <- rnorm(n)
#   x2 <- rnorm(n)
#   x3 <- rnorm(n)
#   y <- 1 + 2 * x1 + 3 * x2 + rnorm(n)
#   test_df <- data.frame(x1, x2, x3, y)

#   # Run the bayesian lasso
#   result <- bayesian_lasso(df=test_df, outcome_var="y", num.reps = 50, burn = 20)

#   # Check if the function returns a list
#   expect_is(result, "list")

#   # Check if the list contains all the correct elements
#   expect_true(all(c("lambda.lasso", "beta_post", "mean_beta") %in% names(result)))
# })