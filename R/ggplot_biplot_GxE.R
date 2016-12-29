ggplot_biplot_GxE = function(biplot_GxE){
  
  data_ecovalence = biplot_GxE$data_ecovalence
  data_pca = biplot_GxE$pca
  
  # Ecovalence ----------
  p_eco = ggplot(data_ecovalence, aes(x = location, y = germplasm, fill = variable)) + geom_raster()
  p_eco = p_eco + scale_fill_gradient(low = "green", high = "red") 
  p_eco = p_eco + ggtitle(paste("Wrick ecovalence for", variable)) + theme(plot.title=element_text(hjust=0.5))
  
  # Biplots ----------
  which_won_where = ggplot_which_won_where(pca) + theme(plot.title=element_text(hjust=0.5))
  mean_vs_stability = ggplot_mean_vs_stability(pca)
  discrimitiveness_vs_representativeness = ggplot_discrimitiveness_vs_representativeness(pca)
  
  
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