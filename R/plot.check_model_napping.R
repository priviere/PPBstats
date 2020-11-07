#' Get ggplot to visualize output from \code{\link{check_model.fit_model_napping}}
#'
#' @description
#' \code{plot.check_model_napping} returns ggplot to visualize outputs from \code{\link{check_model.fit_model_napping}}
#'
#' @param x Output from \code{\link{check_model.fit_model_napping}}
#' 
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' See example in the book : https://priviere.github.io/PPBstats_book/napping.html#check-and-visualize-model-outputs-7 
#' 
#' @return 
#' A plot with the variance caught by the dimension of the MFA
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{check_model.fit_model_napping}}
#' 
#' @export
#' 
#' @import factoextra
#' 
plot.check_model_napping <- function(
  x, ...
){
  out = fviz_eig(x$out_MFA) + ggtitle("")
  return(out)
}
