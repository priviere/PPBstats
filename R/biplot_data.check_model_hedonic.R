#' Format data for CA biplot
#'
#' @description
#' \code{biplot_data.check_model_hedonic} format data for CA plot from \code{\link{check_model}} with \code{\link{model_hedonic}}
#' 
#' @param x Output from \code{\link{check_model}} with \code{\link{model_hedonic}}
#' 
#' @return 
#' The function return a CA object
#'  
#' @author Pierre Riviere
#'
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model.fit_model_hedonic}}, 
#' \item \code{\link{plot.biplot_hedonic}}
#' }
#'  
biplot_data.check_model_hedonic = function(x){
  out = x$hedonic$CA 
  class(out) <- c("PPBstats", "biplot_hedonic", "CA")
  return(out)
}

