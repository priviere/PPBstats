check_model.fit_model_spatial <- function(
  x
){
  model = x$model$model
  summary_model = x$model$summary
  
  # 1. Check residuals (qqplot, Skewness & Kurtosis tests) ----------
  r = residuals(model)
  
  # 1.1. Normality ----------
  data_ggplot_normality = data.frame(r)
  data_ggplot_skewness_test = skewness(r)
  data_ggplot_kurtosis_test = kurtosis(r)
  
  # 1.2. Standardized residuals vs theoretical quantiles ----------
  df.res = as.numeric(as.character(summary_model$p.table.dim["Residual", "Effective"]))
  s = sqrt(deviance(model)/df.res)
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
