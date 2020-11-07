#' Get ggplot to visualize output from \code{\link{biplot_data.check_model_GxE}}
#' 
#' @description
#' \code{plot.biplot_GxE} returns ggplot to visualize outputs from \code{\link{biplot_data.check_model_GxE}}
#' 
#' @param x Output from \code{\link{biplot_data.check_model_GxE}}
#' 
#' @param ... further arguments passed to or from other methods
#' 
#' @details
#' S3 method.
#' The plot are done with the factoextra package
#' 
#' @return 
#' It returns a list with 3 elements:
#'   \itemize{
#'    \item ecovalence
#'    \item interaction which display the interaction matrix
#'    \item biplot, a list of four elements :
#'     \itemize{
#'      \item simple_biplot : the biplot of location and germplasm
#'      \item which_won_where : plot to assess which germplasm win in which location (see \code{\link{ggplot_which_won_where}} for more information). Done only if gxe_analysis is GGE in \code{\link{model_GxE}}.
#'      \item mean_vs_stability : a list of two elements (see \code{\link{ggplot_mean_vs_stability}} for more information). Done only if gxe_analysis is GGE in \code{\link{model_GxE}}.
#'       \itemize{
#'        \item mean : assess mean of each germplasm (see Details for more information).
#'        \item stability : assess stability of each germplasm (see Details for more information).
#'       }
#'      \item discrimitiveness_vs_representativeness (see \code{\link{ggplot_discrimitiveness_vs_representativeness}} for more information) : a list of two elements. Done only if gxe_analysis is GGE in \code{\link{model_GxE}}.
#'       \itemize{
#'        \item discrimitiveness : assess discrimitiveness of each location (see Details for more information).
#'        \item representativeness : assess representativeness of each location (see Details for more information).
#'       }
#'      \item discrimitiveness_vs_representativeness :represents discrimitiveness versus representativeness (see Details for more information). 
#'     }
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
#' @import ggplot2
#' 
plot.biplot_GxE <- function(x, ...){
  
  germplasm  = NULL  # to avoid no visible binding for global variable
  
  variable = x$info$variable
  gxe_analysis =x$info$gxe_analysis
  data_ecovalence = x$data_ecovalence
  data_interaction = x$data_interaction
  data_pca = x$pca
  
  # Ecovalence ----------
  p_eco = ggplot(data_ecovalence, aes(x = location, y = germplasm, fill = variable)) + geom_raster()
  p_eco = p_eco + scale_fill_gradient2(low = "red", high = "blue", mid = "white") 
  p_eco = p_eco + ggtitle(paste("Wrick ecovalence for", variable)) + theme(plot.title=element_text(hjust=0.5), axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Interaction ----------
  p_inter = ggplot(data_interaction, aes(x = location, y = germplasm, fill = variable)) + geom_raster()
  p_inter = p_inter + scale_fill_gradient2(low = "red", high = "blue", mid = "white") 
  p_inter = p_inter + ggtitle(paste("Interaction matrix for", variable)) + theme(plot.title=element_text(hjust=0.5), axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  # Biplots ----------
  simple_biplot = get_biplot(data_pca)
  if( gxe_analysis == "GGE" ){
    which_won_where = ggplot_which_won_where(data_pca) + theme(plot.title=element_text(hjust=0.5))
    mean_vs_stability = ggplot_mean_vs_stability(data_pca)
    discrimitiveness_vs_representativeness = ggplot_discrimitiveness_vs_representativeness(data_pca)
  } else {
    which_won_where = NULL
    mean_vs_stability = NULL
    discrimitiveness_vs_representativeness = NULL
  }
  
  # return results
  out = list(
    "ecovalence" = p_eco,
    "interaction" = p_inter,
    "biplot" = list(
      "simple_biplot" = simple_biplot,
      "which_won_where" = which_won_where,
      "mean_vs_stability" = mean_vs_stability,
      "discrimitiveness_vs_representativeness" = discrimitiveness_vs_representativeness
    )
  )
  
  return(out)
}