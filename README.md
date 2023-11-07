# protectR

<img src="man/figures/logo.png" align="right"  height="200"/>

`r badge_lifecycle("maturing", "blue")`
`r badge_last_commit("selimyaman/protectR")`

```{r eval=FALSE, include=FALSE}
#this part normally should be outside of the code chunk:
<!-- badges: start -->
  [![R-CMD-check](https://github.com/selimyaman/protectR/workflows/R-CMD-check/badge.svg)](https://github.com/selimyaman/protectR/actions)
<!-- badges: end -->
# this is the code to renew readme file
  devtools::build_readme()
```

`protectR` is an R package designed for researchers who utilize regularization methods but also want to incorporate theory when applying these methods. This package allows users to partically protect theoretically important variables from shrinkage when using regularization techniques like Lasso.

## Installation

You can install the development version of `protectR` from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("selimyaman/protectR")
```

## Features

* Implements Partially-protected Bayesian Lasso, allowing users to safeguard theoretically significant variables in their models.
* Provides a balance between theoretical robustness and predictive accuracy.
* Includes functions for plotting and interpreting the results within the Bayesian framework.

## Usage

```r
library(protectR)

# create some data
n <- 100
x1 <- sample(c("A", "B"), n, replace = TRUE)
x1[sample(n, 10)] <- NA  # add missing values to the first column
x2 <- rnorm(n)
x2[sample(n, 5)] <- NA  # add missing values to the second column
x3 <- rnorm(n)
y <- as.numeric(x1 == "A") + x2 + rnorm(n)
mydata <- data.frame(x1, x2, x3, y)

# run model where we protect x1 but not x2 or x3
model <- bayesian_lasso(df=mydata, outcome_var = "y", protected_vars = c("X1"))

coefficient_df <- get_coefficients(model, mydata, "y")

plot_from_coefs(coefficient_df)

plot_beta_post(model = model, df = mydata, outcome_var = "y")
```

## Contributing

Contributions to protectR are welcome. 
