plot.mean_comparisons_model_GxE <- function(
  x,
  nb_parameters_per_plot = 8
  ){
  out = plot_mean_comparisons_freq_anova(x, variable = x$info$variable, nb_parameters_per_plot)
  return(out)
}

