# This clears everything in the environment!
rm(list = ls())
# Clear the R Console
cat("\014")
cat("\n------------------\n")

source("ols.r")
source("print_ols_results.r")
source("logit.r")
source("print_logit_results.r")

# Load necessary packages
library(MASS)  # for mvrnorm()

# Set seed for reproducibility
# set.seed(123)

# Simulate data for logit model
N <- 1000  # Number of observations
# P <- 2  # Number of predictors

# Simulate data for OLS model

# True coefficients for the OLS model
beta_true_ols <- c(-1, 2, -3)

# Generate predictors from a multivariate normal distribution
X_ols <- cbind(1, mvrnorm(N, mu = c(0, 0), Sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2)))

# Generate response variable
Y_ols <- X_ols %*% beta_true_ols + rnorm(N, mean = 0, sd = 1)  # Add some noise

# Apply OLS model
ols_results <- ols(Y_ols, X_ols)

# Print OLS results
print_ols_results(ols_results, c("Y", "Intercept", "X1", "X2"))

# True coefficients for the logit model
beta_true_logit <- c(-1, 2, -3)

# Generate predictors from a multivariate normal distribution
X_logit <- cbind(1, mvrnorm(N, mu = c(0, 0), Sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2)))

# Generate binary response variable
pr <- 1 / (1 + exp(-X_logit %*% beta_true_logit))  # probabilities
Y_logit <- rbinom(N, 1, pr)  # binary response

# Apply logit model
logit_results <- logit(Y_logit, X_logit)

# Print logit results
print_logit_results(logit_results, c("Y", "Intercept", "X1", "X2"))



