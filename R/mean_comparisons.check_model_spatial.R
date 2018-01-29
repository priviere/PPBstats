mean_comparisons.check_model_spatial <- function(
  x, 
  alpha = 0.05,
  p.adj = "none"
){
  # 1. Get data ----------
  data = x$spatial$info$data
  variable = x$spatial$info$variable
  summary_model = x$spatial$model$summary
  
  # 2. Mean comparison on germplasm ----------
  lsd = LSD.test(
    y = data[,variable],
    trt = data$germplasm,
    DFerror = as.numeric(as.character(summary_model$p.table.dim["Residual", "Effective"])), 
    MSerror = x$spatial$model$var_res, 
    alpha = alpha, 
    p.adj = p.adj
    )
  
  parameter = factor(rownames(lsd$groups), levels = rownames(lsd$groups))
  means = lsd$groups[,1]
  groups = lsd$groups[,2]
  alpha = rep(alpha, length(parameter))
  alpha.correction = rep(p.adj, length(parameter))
    
  data_ggplot_LSDbarplot_germplasm = data.frame(parameter, means, groups, alpha, alpha.correction)
  if( nrow(data_ggplot_LSDbarplot_germplasm) == 0 ) { data_ggplot_LSDbarplot_germplasm = NULL }
  
  # 3. return results
  out <- list(
    "info" = x$info,
    "data_ggplot_LSDbarplot_germplasm" = data_ggplot_LSDbarplot_germplasm
  )
  
  class(out) <- c("PPBstats", "mean_comparisons_model_spatial")
  
  return(out)
}