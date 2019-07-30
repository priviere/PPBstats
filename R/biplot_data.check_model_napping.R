#' Format data for MFA biplot for napping model
#'
#' @description
#' \code{biplot_data.check_model_napping} format data for MFA plot from \code{\link{check_model.fit_model_napping}}
#' 
#' @param x Output from \code{\link{check_model.fit_model_napping}}
#' 
#' @return 
#' The function return a MFA object
#'  
#' @details 
#' S3 method.
#' 
#' @author Pierre Riviere
#'
#' @seealso 
#' \itemize{
#' \item \code{\link{biplot_data}}
#' \item \code{\link{check_model.fit_model_napping}}
#' \item \code{\link{plot.biplot_napping}}
#' }
#' 
#' @export
#' 
biplot_data.check_model_napping = function(x){
  if( !inherits(x, "check_model_napping") ) {
    stop("data must come from PPBstats::check_model() with output from PPBstats:model_napping.")
  }
  out = x
  class(out) <- c("PPBstats", "biplot_napping")  
  return(out)
}
