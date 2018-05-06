plot.check_model_hedonic <- function(
  x
){
  # anova  
  out = plot_check_freq_anova(x, variable = "note")
  
  # pca composante variance
  data_ggplot_pca = x$hedonic$CA
  p = fviz_eig(data_ggplot_pca) + ggtitle("")
  
  # return results
  out = c(out, list("pca_composante_variance" = p))
  return(out)
}