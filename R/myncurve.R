#' myncurve
#'
#' @param mu Mean of the normal distribution
#' @param sigma Standard deviation of the normal distribution
#' @param a Numerical value that y is less than
#'
#' @return Graph of probability X <= a in the normal distribution, the probability
#' @export
#'
#' @examples myncurve(10,5,2)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma), ylab="Normal density")
  xnormcurve = seq(-1000,a, length = 1000000)
  ynormcurve = dnorm(xnormcurve,mu,sigma)
  polygon(x=c(-1000, xnormcurve, a), y=c(0,ynormcurve,0), col = "red")
  prob = pnorm(a,mu,sigma)
  text(0,0.5*dnorm(0,0,1), paste0(area = 0,round(prob,4)))
  list(prob)
  curve=dnorm=polygon=pnorm=text=x=NULL
}

