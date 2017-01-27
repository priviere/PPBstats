#' Get ggplot objects from cross_validation_model_2
#'
#' @description
#' \code{ggplot_cross_validation_model_2} returns ggplot objects from cross_validation_model_2
#' 
#' @param out_cross_validation_model_2 outputs from cross_validation_model_2 function
#' 
#' @details See get_ggplot
#' 
#' @return See get_ggplot
#' 
#' @seealso \code{\link{get_ggplot}}, \code{\link{cross_validation_model_2}}
#' 
ggplot_cross_validation_model_2 = function(
  out_cross_validation_model_2
){
  
  # Get the regression ----------
  p = ggplot(out_cross_validation_model_2, aes(x = observed.value, y = estimated.value)) 
  p = p + stat_smooth(method = "lm") + geom_point()
  model = lm(observed.value ~ estimated.value, data = out_cross_validation_model_2)
  
  # Get the confidence in the estimation ----------
  bias = out_cross_validation_model_2$observed.value - out_cross_validation_model_2$estimated.value
  test = t.test(bias, mu = 0)
  proba_mean_equal_zero = round(test$p.value * 100, 1)
  p = p + ggtitle("Cross validation", paste("Probability mean = 0 : ", proba_mean_equal_zero))
  
  out = list("plot" = p, "regression" = model)
  
  return(out)
}
