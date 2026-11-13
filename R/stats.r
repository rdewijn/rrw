#' Convert sd on log scale to CV on original scale
#'
#' @param s standard deviation on log scale
#' @param logbase base of the log scale (default = 2)
#'
#' @export
logsd2cv = function(s, base = 2){
  shat = (s * log(base))
  sqrt(exp(shat^2) -1)
}

#' Convert CV on original scale to sd on log scale
#'
#' @param cv cv on original scale
#' @param logbase base of the log scale (default = 2)
#'
#' @export
cv2logsd = function(cv, base = 2){
  sqrt(log(cv^2 + 1))/log(base)
}

#' Calculate the population sd
#'
#' @param x vector of data points
#' @return the sample sd multiplied by sqrt((n-1)/n) to get the population as
#'
#' @export
popsd = function(x, na.rm = TRUE){
  n = length(x)
  sd(x, na.rm = na.rm)*sqrt((n-1)/n)
}

