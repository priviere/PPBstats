#' Get ggplot to visualize output from \code{\link{check_model.fit_model_home_away}}
#'
#' @description
#' \code{plot.check_model_home_away} returns ggplot to visualize outputs from \code{\link{check_model.fit_model_home_away}}
#'
#' @param x Output from \code{\link{check_model.fit_model_home_away}}
#'
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#'
#' variance_intra_location represents the repartition of the residuals for each location.
#'    This has to been seen with caution:
#'    \itemize{
#'     \item If location have no intra-location variance (i.e. pure line or hybrides) then the distribution of each location represent only the micro-environmental variation.
#'     \item If location have intra-location variance (i.e. population such as landraces for example) then the distribution of each location represent the micro-environmental variation plus the intra-location variance.
#'     With the hypothesis than the micro-environmental variation is equaly distributed on all the individuals (i.e. all the plants), the distribution of each location represent the intra-location variance.
#'    }
#'
#' See example in the book \href{https://priviere.github.io/PPBstats_book/family-4.html#family-4}{here}
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
#'  \item variance_intra_location : repartition of the residuals for each location (see Details for more information)
#'  }
#'
#' @author Pierre Riviere and Baptiste Rouger
#'
#' @seealso \code{\link{check_model.fit_model_home_away}}
#'
#' @export
#'
plot.check_model_home_away <- function(x, ...){
  out = plot_check_freq_anova(x, variable = x$model_home_away$info$variable)
  return(out)
}
