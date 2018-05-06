mean_comparisons.check_model_GxE <- function(
  x, 
  alpha = 0.05,
  p.adj = "none"
){
  out = mean_comparisons_freq_anova(model = x$GxE$ANOVA$model, variable = x$GxE$info$variable, 
                                    alpha, p.adj, info = x$info)
  class(out) <- c("PPBstats", "mean_comparisons_model_GxE")
  return(out)
}