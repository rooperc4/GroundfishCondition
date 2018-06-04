#' A function to compute length-weight residuals
#'
#' This function makes a log-log regression of length and weight for individual fish
#' and then calculates a residual.
#' @param length Set of individual fish lengths
#' @param weight Corresponding set of individual fish weights
#' @keywords length, weight, groundfish condition
#' @export
#' @examples
#' lw.resids()

lw.resids<-function(length,weight){
  loglength<-log(length)
  logwt<-log(weight)
  lw.res<-lm(logwt~loglength)
  lw.res<-lw.res$residuals
  return(lw.res)}