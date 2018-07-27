#' Get ggplot to visualize output from \code{\link{check_model.fit_model_hedonic}}
#'
#' @description
#' \code{plot.check_model_hedonic} returns ggplot to visualize outputs from \code{\link{check_model.fit_model_hedonic}}
#'
#' @param x Output from \code{\link{check_model.fit_model_hedonic}}
#' 
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' 
#' variance_intra_germplasm display the repartition of the residuals for each germplasm which represent the person assessor variation plus the intra-germplasm variance
#' 
#' See example in the book : https://priviere.github.io/PPBstats_book/hedonic.html#check-and-visualize-model-outputs-6
#' 
#' @return 
#' 
#' \itemize{
#'  \item residuals
#'  \itemize{
#'   \item histogram : histogram with the distribution of the residuals
#'   \item qqplot
#'   }
#'  \item variability_repartition : pie with repartition of SumSq for each factor
#'  \item variance_intra_germplasm : repartition of the residuals for each germplasm (see Details for more information)
#'  \item ca_composante_variance : variance caught by each dimension of the CA
#'  }
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{check_model.fit_model_hedonic}}
#' 
#' @export
#' 
#' @import factoextra
#' 
plot.check_model_hedonic <- function(
  x, ...
){
  # anova  
  out = plot_check_freq_anova(x, variable = "note")
  
  # pca composante variance
  data_ggplot_pca = x$hedonic$CA
  p = fviz_eig(data_ggplot_pca) + ggtitle("")
  
  # return results
  out = c(out, list("ca_composante_variance" = p))
  return(out)
}