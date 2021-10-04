#' write table to tab delimited file
#'
#' @param df A data frame
#' @param filename output file name
#' @examples
#' write.delim(iris, filename = "iris.txt")
#' @export
write.delim = function(df, filename){
  write.table(x = df, file = filename, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
}
