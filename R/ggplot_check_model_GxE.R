#' Get ggplot from check_model_GxE
#'
#' @description
#' \code{ggplot_check_model_GxE} returns ggplot from \code{\link{check_model_GxE}}
#' 
#' @param x outputs from \code{\link{check_model_GxE}} function
#' 
#' @details See \code{\link{get_ggplot}}
#' 
#' @return See \code{\link{get_ggplot}}
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{get_ggplot}}, 
#' \item \code{\link{check_model_GxE}}
#' }
#' 
plot.check_model_GxE <- function(
  x,
  nb_parameters_per_plot = 10
){
  # Get data ----------
  
  variable = x$GxE$info$variable
  
  data_ggplot = x$data_ggplot
  
  data_ggplot_normality = data_ggplot$data_ggplot_residuals$data_ggplot_normality
  data_ggplot_skewness_test = data_ggplot$data_ggplot_residuals$data_ggplot_skewness_test
  data_ggplot_kurtosis_test = data_ggplot$data_ggplot_residuals$data_ggplot_kurtosis_test
  data_ggplot_qqplot = data_ggplot$data_ggplot_residuals$data_ggplot_qqplot
  data_ggplot_variability_repartition_pie = data_ggplot$data_ggplot_variability_repartition_pie
  data_ggplot_var_intra = data_ggplot$data_ggplot_var_intra
  
  data_ggplot_pca = x$GxE$PCA
  
  
  # 1. Normality ----------
  # 1.1. Histogramm ----------
  p = ggplot(data_ggplot_normality, aes(x = r), binwidth = 2)
  p = p + geom_histogram() + geom_vline(xintercept = 0)
  p = p + ggtitle("Test for normality", paste("Skewness:", signif(data_ggplot_skewness_test, 3), "; Kurtosis:", signif(data_ggplot_kurtosis_test, 3)))
  p1.1 = p + theme(plot.title=element_text(hjust=0.5))
  
  # 1.2. Standardized residuals vs theoretical quantiles ----------
  p = ggplot(data_ggplot_qqplot, aes(x = x, y = y)) + geom_point() + geom_line() 
  p = p + geom_abline(slope = 1, intercept = 0, color = "red")
  p = p + xlab("Theoretical Quantiles") + ylab("Standardized residuals")
  p1.2 = p + ggtitle("QQplot") + theme(plot.title=element_text(hjust=0.5))

  # 2. repartition of variability among factors ----------
  p = ggplot(data_ggplot_variability_repartition_pie, aes(x = "", y = percentage_Sum_sq, fill = factor)) 
  p = p + ggtitle(paste("Total variance distribution for", variable))
  p = p + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
  #pie = pie + geom_text(data=DFtemp, aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = paste("  ",round(valuep*100), "%")))
  p2 = p + ylab("") + xlab("") + theme(plot.title=element_text(hjust=0.5))
  
  
  # 3. variance intra germplasm
  p = ggplot(data_ggplot_var_intra, aes(x = x, y = y))  + geom_boxplot(aes(color=x))
  p = p + ggtitle("Distribution of residuals") + xlab("germplasm") + ylab(variable)
  p = p + theme(legend.position = "none", axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5))
  p3 = p 
  
  # 4. pca composante variance ----------
  p4 = fviz_eig(data_ggplot_pca) + ggtitle("")
  
  # 5. return results
  
  out = list(
    "residuals" = list(
      "histogramm" = p1.1,
      "qqplot" = p1.2),
    "variability_repartition" = p2,
    "variance_intra_germplasm" = p3,
    "pca_composante_variance" = p4
  )
  
  return(out)
}
