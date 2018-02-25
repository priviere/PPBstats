mean_comparisons.predict_the_past_model_bh_GxE <- function(
  x,
  alpha = 0.05,
  type = 1,
  get.at.least.X.groups = 2,
  precision = 0.0005,
  threshold = 1,
  p.adj = "soft.bonf"
){

  out <- mean_comparisons.check_model_bh_intra_location(
    x, parameter = "mu", alpha = alpha, type = type, threshold = threshold,
    p.adj = p.adj, get.at.least.X.groups = get.at.least.X.groups,
    precision = precision)
  
  out <- out[1]
  
  d <- out$data_mean_comparisons[[1]]$mean.comparisons
  d$parameter_statuts <- x$parameter_statuts[d$parameter]
  
  out$data_mean_comparisons[[1]]$mean.comparisons <- d
  
  class(out) <- c("PPBstats", "mean_comparisons_predict_the_past_model_bh_GxE")
  
  return(out)
}

