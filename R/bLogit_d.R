# This clears everything in the environment!
rm(list = ls())
# Clear the R Console
cat("\014")
cat("\n------------------\n")

source("ols.r")
source("print_ols_results.r")
source("logit.r")
source("print_logit_results.r")


n <- 24
y <- rep(0, n)
y[1:14] <- rep(1, 14)

xdata <- c(21, 24, 25, 26, 28, 31, 33, 34, 35, 37, 43, 49,
           51, 55, 25, 29, 43, 44, 46, 46, 51, 55, 56, 58)

iota <- rep(1, n)

x <- cbind(iota, xdata)

vnames <- c('days    ',
            'iota    ',
            'response')

reso <- ols(y, x)
print_ols_results(reso, vnames)

res <- logit(y, x)
print_logit_results(res, vnames)


