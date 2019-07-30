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
#' 
#' The plot are done with the factoextra package
#' 
#' @return 
#' It returns a list with 3 elements, which are outputs from MFA analysis:
#'   \itemize{
#'    \item var
#'    \item p_ind_germplasm
#'    \item p_ind_location
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
plot.biplot_napping = function(x, ...){
  # see http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_mfa.html
  out_MFA = x$out_MFA
  data = x$data
  
  p_var = fviz_mfa_var(out_MFA, repel = TRUE)
  
  grp = as.factor(data[, "germplasm"])
  p_ind_germplasm = fviz_mfa_ind(out_MFA, repel = TRUE, habillage = grp)
  
  grp = as.factor(data[, "location"])
  p_ind_location = fviz_mfa_ind(out_MFA, repel = TRUE, habillage = grp)
  
  out = list(
    var = p_var,
    ind_germplasm = p_ind_germplasm,
    ind_location = p_ind_location
    )
  
  return(out)
}


