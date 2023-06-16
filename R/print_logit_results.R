print_logit_results <- function(results, vnames) {
  cat('\n')
  cat('Logit Regression Results\n')
  
  if (!is.null(vnames)) {
    cat('Dependent Variable: ', vnames[1], '\n')
  }
  
  cat('McFadden R-squared: ', round(results$r2mf, 4), '\n')
  cat('Estrella R-squared: ', round(results$rsqr, 4), '\n')
  cat('LR-ratio (2*(Lu-Lr)): ', round(results$lratio, 4), '\n')
  cat('LR p-value: ', round(1 - pchisq(results$lratio, results$nvar - 1), 4), '\n')
  cat('Log-Likelihood: ', round(results$lik, 4), '\n')
  cat('Number of iterations: ', results$iter, '\n')
  cat('Convergence criterion: ', round(results$convg, 4), '\n')
  cat('Number of observations, variables: ', results$nobs, ', ', results$nvar, '\n')
  cat('Number of 0\'s, 1\'s: ', results$zip, ', ', results$one, '\n')
  
  cat('\n')
  cat('Coefficients, T-statistics, T-probabilities:\n')
  coef_names <- vnames[2:length(vnames)]
  
  max_name_length <- max(nchar(coef_names))
  
  for (i in seq_along(coef_names)) {
    coef_str <- sprintf('%*.4f', max_name_length, results$beta[i])
    tstat_str <- sprintf('%*.4f', max_name_length, results$tstat[i])
    tprob_str <- sprintf('%*.4f', max_name_length, pt(abs(results$tstat[i]), results$nobs - results$nvar, lower.tail = FALSE) * 2)
    cat(paste0(sprintf('%-*s', max_name_length, coef_names[i]), ': Coefficient: ', coef_str, ', T-statistic: ', tstat_str, ', T-probability: ', tprob_str, '\n'))
  }
  
  cat('***************************************************************\n')
}
