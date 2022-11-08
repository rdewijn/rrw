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
#' performance metrics with CI
#' @param cmat confusionMatrix object as returned by caret::confusionMatrix
#' @import binom
#' @export
perfci = function(cmat){
  levels = rownames(cmat$table)
  posidx = match(cmat$positive, levels)
  negidx = 1
  if(posidx == 1) negidx = 2
  cm = cmat$table
  acc = binom.confint(cm[1,1] + cm[2,2], sum(cm), method = "exact")
  sn = binom.confint(cm[posidx, posidx], sum(cm[,posidx]), method = "exact")
  sp = binom.confint(cm[negidx, negidx], sum(cm[,negidx]), method = "exact")
  ppv =  binom.confint(cm[posidx, posidx], sum(cm[posidx,]), method = "exact")
  npv =  binom.confint(cm[negidx, negidx], sum(cm[negidx,]), method = "exact")

  bind_rows(acc, sn, sp, ppv, npv) %>%
    mutate(parameter = c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV")) %>%
    select(parameter, x,n,mean, lower, upper, method)

}
