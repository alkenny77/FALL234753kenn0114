#' ntickets
#'
#' @param N The number of tickets sold on the flight
#' @param gamma The probability of overbooking
#' @param p The probability a given person shows up
#'
#' @return A named list of nd,nc,N,,gamma, p
#' @export
#'
#' @examples ntickets(200,.02,.95)
ntickets <- function(N,gamma,p){
  n <- c(N:1000)
  nc <- seq(N,1000, length=100000)
  layout(c(1,2))
  discobj <- 1-gamma-pbinom(N,n,p)
  discsolution <- which.min(abs(discobj))
  plot(discobj~n, xlab = "n", ylab = "Objective", xlim = c(n[discsolution]-20,n[discsolution]+20),main= "Objective vs. n to find the number of tickets for a Discrete Distribution")
  abline(a=0,b=0, col="red")
  abline(v=n[discsolution], col="red")
  contobj <- 1-gamma-pnorm(N+0.5,nc*p,sqrt(nc*p*(1-p)))
  contsolution <- which.min(abs(contobj))
  plot(contobj~nc, xlab = "n", ylab = "Objective", main = "Objective vs. n to find the number of tickets for a Continuous Distribution", xlim=c(nc[contsolution]-20,nc[contsolution]+20),type="l")
  abline(a=0,b=0, col="blue")
  abline(v=nc[contsolution], col="blue", )
  list <- list("nd"=n[discsolution], "nc"=nc[contsolution], "N"=N, "p"=p , "gamma"=gamma)
  list
  layout=pbinom=pnorm=abline=NULL #Simply eliminates global variable note, has not effect on code
}
