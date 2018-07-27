#' Get ggplot to visualize output from \code{\link{cross_validation_model_bh_GxE}}
#'
#' @description
#' \code{plot.cross_validation_model_bh_GxE} returns ggplot to visualize outputs from \code{\link{cross_validation_model_bh_GxE}}
#'
#' @param x Output from \code{\link{cross_validation_model_bh_GxE}}
#' 
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' For plot : plot estimated.value = f(observed.value). 
#' The probability mean = 0 is coming from a \code{t.test} performed with the null hypothesis H0: the bias of estimated values in relation to real values = 0.
#' 
#' See example in the book: https://priviere.github.io/PPBstats_book/family-2.html#model-2
#' 
#' @return 
#'   \itemize{
#'    \item plot : plot estimated.value = f(observed.value). 
#'    The probability mean = 0 is display (see Details for more information).
#'    \item regression : output of the model observed.value = a x estimated.value + b
#'   }
#'   
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{cross_validation_model_bh_GxE}}
#' 
#' @export
#' 
#' @import stats
#' @import ggplot2
#' 
plot.cross_validation_model_bh_GxE <- function(
  x, ...
){
  observed.value = estimated.value = NULL  # to avoid no visible binding for global variable
  
  # Get the regression ----------
  p = ggplot(x, aes(x = observed.value, y = estimated.value)) 
  p = p + stat_smooth(method = "lm") + geom_point()
  model = stats::lm(observed.value ~ estimated.value, data = x)
  
  # Get the confidence in the estimation ----------
  bias = x$observed.value - x$estimated.value
  test = t.test(bias, mu = 0)
  proba_mean_equal_zero = round(test$p.value * 100, 1)
  p = p + ggtitle("Cross validation", paste("Probability mean = 0 : ", proba_mean_equal_zero))
  
  out = list("plot" = p, "regression" = model)
  
  return(out)
}
