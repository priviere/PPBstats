plot.mean_comparisons_model_hedonic <- function(
  x,
  nb_parameters_per_plot = 8
){
  out = plot_mean_comparisons_freq_anova(x, variable = "note", nb_parameters_per_plot)
  out = list(germplasm = out$germplasm)
  return(out)
}

