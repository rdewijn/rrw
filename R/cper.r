#' data frame wrapper for caret::confusionMatrix
#'
#' @param df A data frame with first column the data, second column the reference
#' @param posclass (optional) output name of positive class
#' @examples
#' df %>% select(prediction, ref) %>% cmat()
#' @import caret
#' @export
cmat = function(df, posclass = NULL){
  caret::confusionMatrix(df[,1], reference = df[,2], positive = posclass)
}

#' @import pROC
#' @export
droc = function(df){
  aRoc = roc(response = df[,1], predictor = df[,2])
}

#' @import ggplot2 dplyr ggsci
#' @export
snsp_plot = function(aRoc){
    pdf = data.frame(sn = aRoc$sensitivities, sp= aRoc$specificities, thr = aRoc$thresholds) %>%
      pivot_longer(c(sn, sp))
     p = ggplot(pdf, aes(x = thr, y = value, colour = name)) + geom_point()
     p = p + scale_colour_jama()
}
