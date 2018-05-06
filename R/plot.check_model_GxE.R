plot.check_model_GxE <- function(
  x
){
  # anova  
  out = plot_check_freq_anova(x, variable = x$GxE$info$variable)

  # pca composante variance
  data_ggplot_pca = x$GxE$PCA
  p = fviz_eig(data_ggplot_pca) + ggtitle("")
  
  # return results
  out = c(out, list("pca_composante_variance" = p))
  return(out)
}
