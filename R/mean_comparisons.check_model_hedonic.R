mean_comparisons.check_model_hedonic <- function(
  x, 
  alpha = 0.05,
  p.adj = "none"
){
  out = mean_comparisons_freq_anova(model = x$hedonic$model, variable = "note", alpha, p.adj)
  out = list(data_ggplot_LSDbarplot_germplasm = out$data_ggplot_LSDbarplot_germplasm)
  class(out) <- c("PPBstats", "mean_comparisons_model_GxE")
  return(out)
}