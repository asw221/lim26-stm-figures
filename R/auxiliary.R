
format.pval <- function( p ) {
  case_when(
    p < 1e-3 ~ "p<0.001",
    .default = sprintf("p=%.3f", p)
  )
}


## Weibull model survival function
weibull.S <- function(
  fit,  # survival::survreg object (dist = "weibull")
  T,    # time T
  x     # covariates x
) {
  shape <- 1 / fit$scale
  scale <- exp( x %*% coef(fit) )
  1 - pweibull( T, shape, scale )
}



## Restricted cubic regression splines
rcs <- function(
  x, df = 4L,
  intercept.term = FALSE, linear.term = FALSE,
  v = NULL
  ) {
  if (is.null(v)) {
    w <- if (df > 6L) c(0.025, 0.975)
    else c(0.05, 0.95)
    v <- quantile(x, seq(w[1], w[2], length.out = df), na.rm = TRUE)
  }
  m <- length(v)
  stopifnot(m > 2L)
  z <- matrix(0, length(x), m - 2L)
  g <- function(x, d = 3) pmax(0, x)^d
  s <- ( v[m] - v[1L] )^(2/3)
  for ( j in seq.int(m - 2L) ) {
    z[, j] <- g((x - v[j])/s) -
      g((x - v[m - 1L])/s) * (v[m] - v[j])/(v[m] - v[m - 1L]) +
      g((x - v[m])/s) * (v[m - 1L] - v[j])
  }
  if (linear.term)
    z <- cbind(x, z)
  if (intercept.term)
    z <- cbind(1, z)
  attr(z, "knots") <- v
  attr(z, "terms") <- list(inter = intercept.term, lin = linear.term)
  class(z) <- "rcs"
  z
}


## Predict method for rcs bases
predict.rcs <- function( object, x, ... ) {
  v <- attr(object, "knots")
  m <- length(v)
  z <- matrix(0, length(x), m - 2L)
  g <- function(x, d = 3) pmax(0, x)^d
  s <- (v[m] - v[1L])^(2/3)
  for (j in seq.int(m - 2L)) {
    z[, j] <- g((x - v[j])/s) -
      g((x - v[m - 1L])/s) * (v[m] - v[j])/(v[m] - v[m - 1L]) +
      g((x - v[m])/s) * (v[m - 1L] - v[j])
  }
  if (attr(object, "terms")$lin)
    z <- cbind(x, z)
  if (attr(object, "terms")$inter)
    z <- cbind(1, z)
  colnames(z) <- colnames(object)
  z
}


