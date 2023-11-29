
#'Piecewise Function
#'
#' @param x X value, as a vector
#' @param xk Xk value, the palce where the pieceline line changes
#' @param coef Coefficients of the piecewise line equation, as a vector
#' @return  A piecewise line regression regression equation
#' @export
#'
#' @examples
#' \dontrun{myf(c(1:21), 17, quad.lm$coef)}
myf <- function(x,xk,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}
