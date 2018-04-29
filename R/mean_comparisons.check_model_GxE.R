mean_comparisons.check_model_GxE <- function(
  x, 
  alpha = 0.05,
  p.adj = "none"
){
  out = mean_comparisons_freq_anova(x, alpha, p.adj)
  class(out) <- c("PPBstats", "mean_comparisons_model_GxE")
  return(out)
}