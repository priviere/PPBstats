#' Get mean comparisons from check_model_spatial
#'
#' @description
#' \code{mean_comparisons.check_model_spatial} performs mean comparisons from object coming from \code{\link{check_model_spatial}}
#' See \code{\link{mean_comparisons}} for more information.
#' 
#' @param x 
#' 
#' @param alpha
#' 
#' @param p.adj
#' 
#' @details See \code{\link{mean_comparisons}}
#' 
#' @return See \code{\link{mean_comparisons}}
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{get_ggplot}}
#' }
#'
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
  
  parameter = factor(lsd$groups$trt, levels = lsd$groups$trt)
  means = lsd$groups$means
  groups = lsd$groups$M
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