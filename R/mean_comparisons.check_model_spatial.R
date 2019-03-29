#' Get mean comparisons from \code{\link{check_model.fit_model_spatial}} object
#'
#' @description
#' \code{mean_comparisons} performs mean comparisons from object coming from \code{\link{check_model.fit_model_spatial}}
#'
#' @param x outputs from \code{\link{check_model.fit_model_spatial}}
#' 
#' @param alpha level of type one error. 0.05 (5\%) by default
#' 
#' @param p.adj For all except type = 2. 
#' NULL for no adjustement of the type one error. 
#' p.adj can "holm", "hochberg", "bonferroni", "BH", "BY" or "fdr"
#' p-adj = "none" is t-student.
#' See p.adjust() for more details.
#'
#' @param ... further arguments passed to or from other methods#' 
#'   
#' @details
#' S3 method.
#' See in the book for more details : https://priviere.github.io/PPBstats_book/intro-agro.html#section-freq
#' 
#' @return 
#'  A list of two elements : 
#'   \itemize{
#'    \item info : a list with variable and data
#'    \item data_ggplot_LSDbarplot_germplasm
#'   }
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{plot.PPBstats}}
#'  \item \code{\link{plot.mean_comparisons_model_spatial}}
#' }
#' 
#' @export
#' 
#' @import agricolae
#' 
#' @importFrom SpATS predict.SpATS
#' 
mean_comparisons.check_model_spatial <- function(
  x, 
  alpha = 0.05,
  p.adj = "none",
  ...
){
  # 1. Get data ----------
  info = x$spatial$info
  data = info$data
  variable = info$variable
  summary_model = x$spatial$model$summary
  
  # 2. Get prediction of BLUPs
  pre = predict.SpATS(out_spatial$model$model, which = "germplasm")
  pre$germplasm = factor(pre$germplasm, levels = pre$germplasm[order(pre$predicted.values)])
  pre$lower = pre$predicted.values-pre$standard.errors
  pre$upper = pre$predicted.values+pre$standard.errors

  # 3. Mean comparison on germplasm ----------
  lsd = agricolae::LSD.test(
    y = pre$predicted.values,
    trt = pre$germplasm,
    DFerror = x$spatial$model$df_residual, 
    MSerror = x$spatial$model$MSerror, 
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
    "info" = info,
    "blups_prediction" = pre,
    "data_ggplot_LSDbarplot_germplasm" = data_ggplot_LSDbarplot_germplasm
  )
  
  class(out) <- c("PPBstats", "mean_comparisons_model_spatial")
  
  return(out)
}