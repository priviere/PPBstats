#' Get ggplot from biplot_GxE
#'
#' @description
#' \code{ggplot_biplot_GxE} returns ggplot from \code{\link{biplot_GxE}}
#' 
#' @param x outputs from \code{\link{biplot_GxE}} function
#' 
#' @details See \code{\link{get_ggplot}}
#' 
#' @return See \code{\link{get_ggplot}}
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{get_ggplot}}, 
#' \item \code{\link{biplot_GxE}}
#' }
#' 
#' 
plot.biplot_GxE <- function(x){
  
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