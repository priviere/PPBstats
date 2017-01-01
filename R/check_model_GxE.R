#' Check if the GxE model went well 
#'
#' @description
#' \code{check_model_GxE} compute test to assess if the GxE model went well
#' 
#' @param out_GxE outputs from GxE function
#' 
#' @details See check_model for further information
#' 
#' @seealso \code{\link{GxE}}, \code{\link{check_model}}
#' 
check_model_GxE = function(
  out_GxE
){
  model = out_GxE$ANOVA$model
  anova_model = out_GxE$ANOVA$anova_model
  
  # 1. Check residuals (qqplot, Skewness & Kurtosis tests) ----------
  r = residuals(model)
  
  # 1.1. Normality ----------
  data_ggplot_normality = data.frame(r)
  data_ggplot_skewness_test = skewness(r)
  data_ggplot_kurtosis_test = kurtosis(r)
  
  #   Skewness: indicator used in distribution analysis as a sign of asymmetry and deviation from a normal distribution. 
  #   
  #   Interpretation: 
  #   Skewness > 0 - Right skewed distribution - most values are concentrated on left of the mean, with extreme values to the right.
  #   Skewness < 0 - Left skewed distribution - most values are concentrated on the right of the mean, with extreme values to the left.
  #   Skewness = 0 - mean = median, the distribution is symmetrical around the mean.
  #   
  #   
  #   Kurtosis - indicator used in distribution analysis as a sign of flattening or "peakedness" of a distribution. 
  #   
  #   Interpretation: 
  #   Kurtosis > 3 - Leptokurtic distribution, sharper than a normal distribution, with values concentrated around the mean and thicker tails. This means high probability for extreme values.
  #   Kurtosis < 3 - Platykurtic distribution, flatter than a normal distribution with a wider peak. The probability for extreme values is less than for a normal distribution, and the values are wider spread around the mean.
  #   Kurtosis = 3 - Mesokurtic distribution - normal distribution for example.
  
  
  # 1.2. Standardized residuals vs theoretical quantiles ----------
  s = sqrt(deviance(model)/df.residual(model))
  rs = r/s
  data_ggplot_qqplot = data.frame(x = qnorm(ppoints(rs)), y = sort(rs))
  
  # Test for homogeneity of variances
  #ft = fligner.test(variable ~ interaction(germplasm,location), data=data)
  #print(ft)
  
  # 2. repartition of variability among factors ----------
  total_Sum_Sq = sum(anova_model$"Sum Sq")
  Sum_sq = anova_model$"Sum Sq"
  pvalue = anova_model$"Pr(>F)"
  percentage_Sum_sq = Sum_sq/total_Sum_Sq*100
  factor = rownames(anova_model)
  data_ggplot_variability_repartition_pie = cbind.data.frame(factor, pvalue, Sum_sq, percentage_Sum_sq)
  
  # 3. variance intra germplasm
  var_intra = tapply(model$residuals, model$model$germplasm, var, na.rm = TRUE)
  data_ggplot_var_intra = data.frame(x = model$model$germplasm, y = model$residuals)
  
  out = list(
    "GxE" = out_GxE,
    "data_ggplot" = list(
      "data_ggplot_residuals" = list(
        "data_ggplot_normality" = data_ggplot_normality,
        "data_ggplot_skewness_test" = data_ggplot_skewness_test,
        "data_ggplot_kurtosis_test" = data_ggplot_kurtosis_test,
        "data_ggplot_qqplot" = data_ggplot_qqplot
      ),
      "data_ggplot_variability_repartition_pie" = data_ggplot_variability_repartition_pie,
      "data_ggplot_var_intra" = data_ggplot_var_intra
    )
  )
  
  attributes(out)$PPBstats.object = "check_model_GxE"
  
  return(out)
}

