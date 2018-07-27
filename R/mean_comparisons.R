#' Get mean comparisons from \code{\link{check_model}} object
#'
#' @description
#' \code{mean_comparisons} performs mean comparisons from object coming from \code{\link{check_model}} or \code{\link{predict_the_past_model_bh_GxE}}
#'
#' @param x outputs from 
#' \itemize{
#'  \item \code{\link{check_model.fit_model_bh_GxE}}
#'  \item \code{\link{check_model.fit_model_bh_intra_location}}
#'  \item \code{\link{check_model.fit_model_bh_variance_intra}}
#'  \item \code{\link{check_model.fit_model_GxE}}
#'  \item \code{\link{check_model.fit_model_hedonic}}
#'  \item \code{\link{check_model.fit_model_spatial}}
#'  \item \code{\link{predict_the_past_model_bh_GxE}}
#' }
#' 
#' @param ... See details
#' 
#' 
#' @details
#' S3 method.
#' See for more details :
#' \itemize{
#'  \item \code{\link{mean_comparisons.check_model_GxE}}
#'  \item \code{\link{mean_comparisons.check_model_bh_intra_location}}
#'  \item \code{\link{mean_comparisons.check_model_bh_GxE}}
#'  \item \code{\link{mean_comparisons.check_model_hedonic}}
#'  \item \code{\link{mean_comparisons.check_model_spatial}}
#'  \item \code{\link{mean_comparisons.check_model_bh_variance_intra}}
#' }
#' 
#' @return 
#' It returns a list of elements ready to be plotted
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{check_model}}
#'  \code{\link{predict_the_past_model_bh_GxE}}
#'  \code{\link{plot.PPBstats}}
#' }
#'
#' @export
#'  
mean_comparisons <- function(x, ...) UseMethod("mean_comparisons")

mean_comparisons.default <- function(x, ...) {
  ## No method found
  mess = paste(substitute(x),
               "must come from check_model() or predict_the_past_model_bh_GxE()."
  )
  stop(mess)
}

