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

lw.resids<-function(length,weight,outlier.rm=FALSE){
 # length<-tempdata$LENGTH
#  weight<-tempdata$WEIGHT
 # outlier.rm<-TRUE
  
  require(car)
  loglength<-log(length)
  logwt<-log(weight)
    lw.res<-lm(logwt~loglength)
  # Assessing Outliers using Bonferoni Outlier Test
  #Identify if there are any outliers in your data that exceed cutoff = 0.05 (default)
  if(outlier.rm==TRUE){
  outlierTest(lw.res,n.max=Inf)
    #QQ residual plot with SD's 
  qqPlot(lw.res, main="QQ Plot") #qq plot for studentized resid
  #Produce a bonferoni value for each point in your data
  test1<-outlierTest(lw.res,n.max=Inf,cutoff=Inf,order=FALSE)$bonf.p 
  remove<-which(test1<.7)
  logwt[remove]<-NA
  lw.res<-lm(logwt~loglength,na.action=na.exclude)
  lw.res<-residuals(lw.res) 
  }
    
 if(outlier.rm==FALSE){ lw.res<-residuals(lw.res)}
  return(lw.res)}