#' Get ggplot to visualize output from \code{\link{check_model.fit_model_anova}}
#'
#' @description
#' \code{plot.check_model_anova} returns ggplot to visualize outputs from \code{\link{check_model.fit_model_anova}}
#'
#' @param x Output from \code{\link{check_model.fit_model_anova}}
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
#' @return 
#' \itemize{
#'  \item residuals
#'  \itemize{
#'   \item histogram : histogram with the distribution of the residuals
#'   \item qqplot
#'   \item points
#'   }
#'  \item variability_repartition : pie with repartition of SumSq for each factor
#'  \item variance_intra_germplasm : repartition of the residuals for each germplasm (see Details for more information)
#'  }
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{check_model.fit_model_anova}}
#' 
#' @export
#' 
plot.check_model_anova <- function(
  x, ...
){
  out = plot_check_freq_anova(x, variable = x$model_anova$info$variable)
  return(out)
}
