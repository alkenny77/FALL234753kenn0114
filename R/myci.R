#' My ci
#'
#' @param x a sample of x
#'
#' @return A 95% confidence interval
#' @export
#'
#' @examples myci(rnorm(30,10,2))
myci =function(x){
 return(mean(x)+c(-1,1)*qt(1-.05/2,length(x)-1)*sd(x)/sqrt(length(x)))
  qt=sd=NULL
}

