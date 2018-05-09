#' Format data for MFA biplot
#'
#' @description
#' \code{biplot_data.check_model_napping} format data for MFA plot from \code{\link{check_model}} with \code{\link{model_napping}}
#' 
#' @param x Output from \code{\link{check_model}} with \code{\link{model_napping}}
#' 
#' @return 
#' The function return a MFA object
#'  
#' @author Pierre Riviere
#'
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model.fit_model_napping}},
#' \item \code{\link{plot.biplot_napping}}
#' }
#' 
biplot_data.check_model_napping = function(x){
  out = x 
  class(out) <- c("PPBstats", "biplot_napping", "MFA")  
  return(out)
}
