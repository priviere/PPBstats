#' Get ggplot to visualize output from \code{\link{check_model.fit_model_GxE}}
#'
#' @description
#' \code{plot.check_model_GxE} returns ggplot to visualize outputs from \code{\link{check_model.fit_model_GxE}}
#'
#' @param x Output from \code{\link{check_model.fit_model_GxE}}
#' 
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' 
#' variance_intra_germplasm represents the repartition of the residuals for each germplasm.
#'    This has to been seen with caution:
#'    \itemize{
#'     \item If germplasm have no intra-germplasm variance (i.e. pure line or hybrides) then the distribution of each germplasm represent only the micro-environmental variation.
#'     \item If germplasm have intra-germplasm variance (i.e. population such as landraces for example) then the distribution of each germplasm represent the micro-environmental variation plus the intra-germplasm variance.
#'     With the hypothesis than the micro-environmental variation is equaly distributed on all the individuals (i.e. all the plants), the distribution of each germplasm represent the intra-germplasm variance.
#'    }
#'    
#' See example in the book: 
#' \itemize{
#'  \item for AMMI : https://priviere.github.io/PPBstats_book/family-2.html#ammi
#'  \item for GGE : https://priviere.github.io/PPBstats_book/family-2.html#gge
#' }
#' 
#' @return 
#' \itemize{
#'  \item residuals
#'  \itemize{
#'   \item histogram : histogram with the distribution of the residuals
#'   \item qqplot
#'   }
#'  \item variability_repartition : pie with repartition of SumSq for each factor
#'  \item variance_intra_germplasm : repartition of the residuals for each germplasm (see Details for more information)
#'  \item pca_composante_variance : variance caught by each dimension of the PCA run on the interaction matrix
#'  }
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{check_model.fit_model_GxE}}
#' 
#' @export
#' 
#' @import factoextra
#' 
plot.check_model_GxE <- function(
  x, ...
){
  # anova  
  out = plot_check_freq_anova(x, variable = x$GxE$info$variable)

  # pca composante variance
  data_ggplot_pca = x$GxE$PCA
  p = fviz_eig(data_ggplot_pca) + ggtitle("")
  
  # return results
  out = c(out, list("pca_composante_variance" = p))
  return(out)
}
