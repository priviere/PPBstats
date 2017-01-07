#' Compute ecovalence and format PCA results from GxE
#'
#' @description
#' \code{biplot_GxE} computes ecovalence and format PCA results from \code{GxE}
#'
#' @param out_check_model_GxE Output from \code{GxE}
#' 
#' @details
#' The ecovalence is the interaction matrix squared.
#' 
#' @return 
#' The function returns a list with:
#' \itemize{
#' \item variable : the variable studied
#' \item data_ecovalence : the ecovalence matrix
#' \item pca : the pca object
#' }
#'  
#' @author Pierre Riviere
#' 
#' @examples
#' See the vignette for examples 
#' 
#' @seealso \code{\link{check_model}}, \code{\link{check_model_GxE}}, \code{\link{get.ggplot}}, \code{\link{ggplot_biplot_GxE}}
#' 
#' 
biplot_GxE = function(out_check_model_GxE){
  
  # 1. Error message
  if( attributes(out_check_model_GxE)$PPBstats.object != "check_model_GxE" ) { stop("data must come from check_model and GxE") }
  
  data_interaction = out_check_model_GxE$GxE$ANOVA$interaction_matrix
  
  # 2. Ecovalence ----------
  m_eco = data_interaction^2
  
  data_ecovalence = data.frame(
    germplasm = rep(rownames(m_eco), times = ncol(m_eco)), 
    location = rep(colnames(m_eco), each = nrow(m_eco)),
    variable = as.vector(m_eco)
  )
  
  out = list(
    "variable" = out_check_model_GxE$GxE$variable,
    "data_ecovalence" = data_ecovalence,
    "pca" = out_check_model_GxE$GxE$PCA
  )
  
  attributes(out)$PPBstats.object = "biplot_GxE"
  
  return(out)
}



