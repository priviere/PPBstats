ggplot_out_biplot_GxE = function(out_biplot_GxE){
  
  variable = out_biplot_GxE$variable
  data_ecovalence = out_biplot_GxE$data_ecovalence
  data_pca = out_biplot_GxE$pca
  
  # Ecovalence ----------
  p_eco = ggplot(data_ecovalence, aes(x = location, y = germplasm, fill = variable)) + geom_raster()
  p_eco = p_eco + scale_fill_gradient(low = "green", high = "red") 
  p_eco = p_eco + ggtitle(paste("Wrick ecovalence for", variable)) + theme(plot.title=element_text(hjust=0.5))
  
  # Biplots ----------
  which_won_where = ggplot_which_won_where(data_pca) + theme(plot.title=element_text(hjust=0.5))
  mean_vs_stability = ggplot_mean_vs_stability(data_pca)
  discrimitiveness_vs_representativeness = ggplot_discrimitiveness_vs_representativeness(data_pca)
  
  # return results
  out = list(
    "ecovalence" = p_eco,
    "biplot" = list(
      "which_won_where" = which_won_where,
      "mean_vs_stability" = mean_vs_stability,
      "discrimitiveness_vs_representativeness" = discrimitiveness_vs_representativeness
    )
  )
  
  return(out)
}