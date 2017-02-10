#' Get ggplot from biplot_GxE
#'
#' @description
#' \code{ggplot_biplot_GxE} returns ggplot from \code{\link{biplot_GxE}}
#' 
#' @param out_biplot_GxE outputs from \code{\link{biplot_GxE}} function
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
ggplot_biplot_GxE = function(out_biplot_GxE){
  
  variable = out_biplot_GxE$info$variable
  gxe_analysis =out_biplot_GxE$info$gxe_analysis
  data_ecovalence = out_biplot_GxE$data_ecovalence
  data_pca = out_biplot_GxE$pca
  
  # Ecovalence ----------
  p_eco = ggplot(data_ecovalence, aes(x = location, y = germplasm, fill = variable)) + geom_raster()
  p_eco = p_eco + scale_fill_gradient(low = "green", high = "red") 
  p_eco = p_eco + ggtitle(paste("Wrick ecovalence for", variable)) + theme(plot.title=element_text(hjust=0.5))
  
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
    "biplot" = list(
      "simple_biplot" = simple_biplot,
      "which_won_where" = which_won_where,
      "mean_vs_stability" = mean_vs_stability,
      "discrimitiveness_vs_representativeness" = discrimitiveness_vs_representativeness
    )
  )
  
  return(out)
}