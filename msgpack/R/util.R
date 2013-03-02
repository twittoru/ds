deepclass <- function(x) {
  if(length(unlist(x)) > 1) return(lapply(x,class))
  ifelse(is.na(x),list(),class(x))
}
doubleToBits <- function(x) fractionalToBits(x, 11, 52, 1023)
floatToBits <- function(x) fractionalToBits(x, 8, 23, 127)
fractionalToBits <- function(x, expSize, fracSize, bias, subnormal=F) {
  signbit <- sign(x) < 0
  abs_x <- abs(x)
  if(abs_x == 0) return(rep(F,64))
  if(signbit) {
    msb <- floor(log2(abs_x))
  } else {
    msb <- ceiling(log2(abs_x)) - 1
  }
  if(msb < -bias) {
    warning(sprintf("input x:%g is subnormal number",x))
    return(fractionalToBits(x*2^(bias-1),expSize,fracSize,bias,subnormal=T))
  }
  if(msb >  bias+1) stop("too big number")
  bin <- logical(fracSize)
  ax <- abs_x %% 2^msb
  if(ax > 0) {
    for(i in (1:fracSize)){
      n <- msb - i
      bin[i] <- ax %/% 2^n == 1
      ax <- ax %% 2^n
    }
  } else {
    # x = 2^msb
    msb <- msb + 1
    if(subnormal) bin[-msb] <- T
  }
  if(subnormal) msb <- - bias
  return(c(
    signbit,
    rev(intToBits(msb + bias)[1:expSize]),
    bin
  ))
}
# the language that has promotion-style integer variable, (x*x*x)^(1/3) is undoubtedly floating-point number
# but the language has demotion-style integer variable, like R, there are no way to differentiate pre-demote integer from no-exponent-part double and so we shuld use is.wholenumber
# if using "?integer" version is.wholenumber (tol = sqrt(eps)), this test can pass some cases such as .Machine$double.eps, .Machine$double.xmin, and (x^(2n+1))^(1/(2n+1)) where x > n
is.wholenumber <- function(x, tol = .Machine$double.eps)  abs(x - round(x)) < tol
is.outofint    <- function(x) {
  e <- NULL
  ret <- suppressWarnings(
    withCallingHandlers(
      { as.integer(x) },
      warning = function(w) e <<- w
    )
  )
  # e = NULL -> noerror
  # ret = NA -> x = NaN
  if(is.null(e) && !is.na(ret)) {
    if( 0 < x && x < 1) return(T)
    return(!is.wholenumber(x))
  } else {
    # out of (intmin,intmax)
    return(T)
  }
}

