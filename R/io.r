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

#' read tab file of Avania CRF
#' @param dbexport base name of the export
#' @param tabstr str identifying the table to import
#' @param ext extension
#' @param csep separator
#' @import dplyr
#' @export
read.crf.table = function(dbExport, tabstr,ext = ".txt", csep = "\t"){
  fname = paste(dbExport," ",tabstr,ext, sep = "")
  print(fname)
  df = read.table(fname, header = TRUE, sep = csep)
  df = df %>% mutate(Subject.ID = factor(Subject.Number))
}

#' read sheetv from excel file of Avania CRF
#' @param dbFile file name of the export
#' @param sheet str identifying the table to import
#' @import dplyr readxl
#' @export
read.crf.sheet = function(dbFile, sheet){
  df = read_excel(dbFile, sheet)
  colnames(df) = colnames(df) %>%
    make.names()
  df %>%
    mutate(Subject.ID = factor(Subject.Number))
}
