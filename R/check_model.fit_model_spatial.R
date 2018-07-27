#' Check if the spacial model went well 
#'
#' @description
#' \code{check_model.fit_model_spatial} computes tests to assess if the model went well. 
#' It is important to run this step before going ahead with the analysis otherwise you may make mistakes in the interpretation of the results.
#' 
#' @param x outputs from \code{\link{model_spatial}}
#' 
#' @details
#' S3 method.
#' The different test apply to the model are explained in the book \href{https://priviere.github.io/PPBstats_book/intro-agro.html#section-freq}{here}:
#' 
#' @return It returns a list with the following elements:
#' 
#' \itemize{
#'  \item model_spatial : the output from the model
#'  \item data_ggplot a list containing information for ggplot:
#'  \itemize{
#'   \item data_ggplot_residuals a list containing :
#'    \itemize{
#'     \item data_ggplot_normality
#'     \item data_ggplot_skewness_test
#'     \item data_ggplot_kurtosis_test
#'     \item data_ggplot_qqplot
#'     }
#'   \item data_ggplot_variability_repartition_pie
#'   }
#' }
#' 
#' @author Pierre Riviere
#'
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}
#' \item \code{\link{plot.check_model_spatial}}
#' \item \code{\link{mean_comparisons}}
#' \item \code{\link{mean_comparisons.check_model_hedonic}}
#' }
#'  
#' @export
#'
#' @import agricolae
#'
check_model.fit_model_spatial <- function(
  x
){
  model = x$model$model
  summary_model = x$model$summary
  
  # 1. Check residuals (qqplot, Skewness & Kurtosis tests) ----------
  r = residuals(model)
  
  # 1.1. Normality ----------
  data_ggplot_normality = data.frame(r)
  data_ggplot_skewness_test = agricolae::skewness(r)
  data_ggplot_kurtosis_test = agricolae::kurtosis(r)
  
  # 1.2. Standardized residuals vs theoretical quantiles ----------
  df.res = as.numeric(as.character(summary_model$p.table.dim["Residual", "Effective"]))
  s = sqrt(stats::deviance(model)/df.res)
  rs = r/s
  data_ggplot_qqplot = data.frame(x = qnorm(ppoints(rs)), y = sort(rs))
  
  # 2. repartition of variability among factors ----------
  var_comp = summary_model$p.table.vc
  var_comp = var_comp[!is.na(var_comp[,"Variance"]),] # In case there is NA because of non estimation of f(col):row and col:f(row)
  total_Sum_Sq = sum(as.numeric(as.character(var_comp[,"Variance"])))
  Sum_sq = as.numeric(as.character(var_comp[,"Variance"]))
  percentage_Sum_sq = Sum_sq/total_Sum_Sq*100
  factor = rownames(var_comp)
  # rename col_f and row_f to col and row, f was put for factor for the model 
  # but is not useful for the user to know
  factor[which(factor == "col_f")] = "col"
  factor[which(factor == "row_f")] = "row"
  
  data_ggplot_variability_repartition_pie = cbind.data.frame(factor, pvalue = NA, Sum_sq, percentage_Sum_sq)
  
  # 3. Return results ----------
  out = list(
    "spatial" = x,
    "data_ggplot" = list(
      "data_ggplot_residuals" = list(
        "data_ggplot_normality" = data_ggplot_normality,
        "data_ggplot_skewness_test" = data_ggplot_skewness_test,
        "data_ggplot_kurtosis_test" = data_ggplot_kurtosis_test,
        "data_ggplot_qqplot" = data_ggplot_qqplot
      ),
      "data_ggplot_variability_repartition_pie" = data_ggplot_variability_repartition_pie
    )
  )
  
  class(out) <- c("PPBstats", "check_model_spatial")
  
  return(out)
}
