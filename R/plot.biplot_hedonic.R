#' Get ggplot to visualize output from \code{\link{biplot_data.check_model_hedonic}}
#' 
#' @description
#' \code{plot.biplot_hedonic} returns ggplot to visualize outputs from \code{\link{biplot_data.check_model_hedonic}}
#' 
#' @param x Output from \code{\link{biplot_data.check_model_hedonic}}
#' 
#' @param ... further arguments passed to or from other methods
#' 
#' @details
#' S3 method.
#' The plot are done with the factoextra package
#' 
#' @return 
#' It returns a list with 3 elements,  which are outputs from CA analysis:
#'   \itemize{
#'    \item ind
#'    \item var
#'    \item axes
#'   }
#'   
#' @author Pierre Riviere
#' 
#' @seealso
#' \itemize{
#'  \item \code{biplot_data}
#' }
#' 
#' @export
#' 
#' @import factoextra
#' 
plot.biplot_hedonic = function(x, ...){
  # see http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_ca.html
  
  p = fviz_ca_biplot(x)
  p$data$sample = x$call$Xtot$sample
  p = p + geom_point(aes(color = sample))
  
  out = list("ca_biplot" = fviz_ca_biplot(x))
  return(out)
}