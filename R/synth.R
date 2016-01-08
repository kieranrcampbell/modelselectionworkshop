

generateLinearSynthetic <- function(x) {
  x <- runif(50, 0, 10)
  y <- rnorm(50, x, 3)

  null_mean <- mean(y)
  null_sd <- sd(y)

  null_loglik <- as.numeric(logLik(lm(y ~ 1)))
  alt_loglik <- as.numeric(logLik(lm(y ~ x)))

  r <- list()
  r$x <- x ; r$y <- y
  r$null_loglik <- null_loglik
  r$alt_loglik <- alt_loglik
  linearExample <- r
  devtools::use_data(linearExample)
}
