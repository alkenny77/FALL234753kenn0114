
#' @title CSV and table reading function
#'
#' @param csv csv file
#' @return  A read csv file with headers
#' @export
#'
#' @examples
#' \dontrun{myread("DDT.csv)}
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")}
