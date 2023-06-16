logit <- function(y, x, maxit = 100, tol = 1e-6) {
  if (missing(y) || missing(x) || length(args) > 4)
    stop("Wrong number of arguments to logit")
  
  # Check for all 1's or all 0's
  tmp <- which(y == 1)
  chk <- length(tmp)
  nobs <- length(y)
  if (chk == nobs)
    stop("logit: y-vector contains all ones")
  else if (chk == 0)
    stop("logit: y-vector contains no ones")
  
  # Maximum likelihood logit estimation
  result <- list()
  result$meth <- "logit"
  
  res <- lm(y ~ x - 1)  # Use OLS values as a start
  b <- coef(res)
  k <- ncol(x)
  
  if (missing(maxit))
    maxit <- 100
  if (missing(tol))
    tol <- 1e-6
  
  crit <- 1.0
  t <- nrow(x)
  i <- rep(1, t)
  tmp1 <- matrix(0, t, k)
  tmp2 <- matrix(0, t, k)
  
  iter <- 1

  while (iter < maxit & crit > tol) {
    tmp <- i + exp(-x %*% b)
    pdf <- exp(-x %*% b) / (tmp * tmp)
    cdf <- i / (i + exp(-x %*% b))
    
    cdf[cdf <= 0] <- 0.00001
    cdf[cdf >= 1] <- 0.99999
    
    # Gradient vector for logit
    term1 <- y * (pdf / cdf)
    term2 <- (i - y) * (pdf / (i - cdf))
    for (kk in 1:k) {
      tmp1[, kk] <- term1 * x[, kk]
      tmp2[, kk] <- term2 * x[, kk]
    }

    g <- tmp1 - tmp2
    gs <- colSums(g)
    delta <- exp(x %*% b) / (i + exp(x %*% b)) # see page 883 Green, 1997
    
    H <- matrix(0, k, k)
    for (ii in 1:t) {
      xp <- x[ii, , drop = FALSE]
      H <- H - delta[ii] * (1 - delta[ii]) * (t(xp) %*% x[ii,])
    }
    # attention include a negative sign
    db <- -solve(H) %*% gs
    s <- 2
    term1 <- 0
    term2 <- 1

    while (term2 > term1) {
      s <- s / 2
      term1 <- lo_like(b + s * db, y, x)
      term2 <- lo_like(b + s * db / 2, y, x)
    }
    
    bn <- b + s * db
    crit <- max(abs(db))
    b <- bn
    iter <- iter + 1
  }
  
  # Compute Hessian for inferences
  delta <- exp(x %*% b) / (i + exp(x %*% b))
  H <- matrix(0, k, k)
  for (i in 1:t) {
    xp <- x[i, , drop = FALSE]
    H <- H - delta[i] * (1 - delta[i]) * (t(xp) %*% x[i,])
  }
  
  # Now compute regression results

  # attention include a negative sign
  covb <- -solve(H)
  stdb <- sqrt(diag(covb))
  result$tstat <- b / stdb
  
  prfit <- 1 / (i + exp(-x %*% b))
  result$resid <- y - prfit
  result$sige <- sum(result$resid^2) / t
  
  tmp <- which(y == 1)
  P <- length(tmp)
  cnt0 <- t - P
  cnt1 <- P
  P <- P / t
  like0 <- t * (P * log(P) + (1 - P) * log(1 - P))  # Restricted likelihood
  like1 <- lo_like(b, y, x)  # Unrestricted likelihood
  
  result$r2mf <- 1 - abs(like1) / abs(like0)  # McFadden pseudo-R2
  term0 <- (2 / t) * like0
  term1 <- 1/(abs(like1)/abs(like0))^term0
  result$rsqr <- 1 - term1  # Estrella R2
  
  result$beta <- b
  result$yhat <- prfit
  result$lratio <- 2 * (like1 - like0)  # LR-ratio test against intercept model
  result$lik <- like1  # Unrestricted likelihood
  result$nobs <- t  # Number of observations
  result$nvar <- k  # Number of variables
  result$zip <- cnt0  # Number of 0's
  result$one <- cnt1  # Number of 1's
  result$iter <- iter  # Number of iterations
  result$convg <- crit  # Convergence criterion max(max(-inv(H) * g))
  result$y <- y  # Y data vector
  
  return(result)
}

lo_like <- function(b, y, x) {
  i <- rep(1, length(y))
  cdf <- i / (i + exp(-x %*% b))
  
  cdf[cdf <= 0] <- 0.00001
  cdf[cdf >= 1] <- 0.99999
  
  like <- y * log(cdf) + (i - y) * log(i - cdf)
  out <- sum(like)
  
  return(out)
}


