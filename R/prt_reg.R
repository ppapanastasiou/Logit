prt_reg <- function(results, vnames = NULL, fid = stdout()) {
  if (!is.list(results))
    stop('prt_reg requires structure argument')
  else if (missing(vnames) && missing(fid)) {
    nflag <- 0
    fid <- stdout()
  } else if (missing(fid)) {
    fid <- stdout()
    nflag <- 1
  } else if (missing(vnames)) {
    nflag <- 0
    vsize <- 0
  } else {
    nflag <- 0
    vsize <- nrow(vnames)
    if (vsize > 0)
      nflag <- 1
  }
  
  nobs <- results$nobs
  nvar <- results$nvar
  
  # Make up some generic variable names
  Vname <- 'Variable'
  for (i in 1:nvar) {
    tmp <- paste('variable', i)
    Vname <- c(Vname, tmp)
  }
  
  if (nflag == 1) { # The user supplied variable names
    tst_n <- nrow(vnames)
    nsize <- ncol(vnames)
    if (tst_n != nvar + 1) {
      cat('Wrong # of variable names in prt_reg -- check vnames argument\n', file = fid)
      cat('Will use generic variable names\n', file = fid)
      nflag <- 0
    } else {
      Vname <- 'Variable'
      for (i in 1:nvar) {
        Vname <- c(Vname, vnames[i + 1, ])
      }
    }
  }
  
  meth <- results$meth
  
  switch(meth,
         ols = {
           cat('Ordinary Least-squares Estimates\n', file = fid)
         },
         hwhite = {
           cat('White Heteroscedastic Consistent Estimates\n', file = fid)
         },
         nwest = {
           cat('Newey-West hetero/serial Consistent Estimates\n', file = fid)
         },
         olsrs = {
           cat('Restricted Least-squares Estimates\n', file = fid)
         },
         logit = {
           cat('Logit Maximum Likelihood Estimates\n', file = fid)
         },
         probit = {
           cat('Probit Maximum Likelihood Estimates\n', file = fid)
         },
         {
           cat('Unknown regression method\n', file = fid)
         }
  )
  
  if (nflag == 1) {
    cat(sprintf('Dependent Variable = %16s\n', vnames[1, ]), file = fid)
  }
  
  if (meth %in% c('ols', 'hwhite', 'nwest', 'olsrs')) {
    cat(sprintf('R-squared      = %9.4f\n', results$rsqr), file = fid)
    cat(sprintf('Rbar-squared   = %9.4f\n', results$rbar), file = fid)
    cat(sprintf('sigma^2        = %9.4f\n', results$sige), file = fid)
    cat(sprintf('Durbin-Watson  = %9.4f\n', results$dw), file = fid)
    cat(sprintf('Nobs, Nvars    = %6d,%6d\n', results$nobs, results$nvar), file = fid)
  } else if (meth %in% c('logit', 'probit')) {
    cat(sprintf('McFadden R-squared     = %9.4f\n', results$r2mf), file = fid)
    cat(sprintf('Estrella R-squared     = %9.4f\n', results$rsqr), file = fid)
    cat(sprintf('LR-ratio, 2*(Lu-Lr)    = %9.4f\n', results$lratio), file = fid)
    cat(sprintf('LR p-value             = %9.4f\n', 1 - pchisq(results$lratio, results$nvar - 1)), file = fid)
    cat(sprintf('Log-Likelihood         = %9.4f\n', results$lik), file = fid)
    cat(sprintf('# of iterations        = %6d\n', results$iter), file = fid)
    cat(sprintf('Convergence criterion  = %16.8g\n', results$convg), file = fid)
    cat(sprintf('Nobs, Nvars            = %6d,%6d\n', results$nobs, results$nvar), file = fid)
    cat(sprintf('# of 0\'s, # of 1\'s     = %6d,%6d\n', results$zip, results$one), file = fid)
  }
  
  cat('***************************************************************\n', file = fid)
}
