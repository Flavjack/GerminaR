
#' Repeated Rows in a data matrix
#' @description This function made a data table with the values of germination variables
#' @param x Row sequance for the data matrix
#' @param n Number of rows for the data matrix
#' @return Data MAtrix with day of the germination
#' @export
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
