#' Multiple iterations of a sample 1-10
#'
#' @param n Size of the population
#' @param iter How many times the sample is run
#' @param time Time betwene the generation of each plot
#'
#' @return Barplots for each iteration of the random sample
#' @export
#'
#' @examples \dontrun{mynewsample(n=1000,iter=30,time=1)}
mynewsample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
    barplot=rainbow=NULL
  }
}
