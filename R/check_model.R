#' Check if the model went well 
#'
#' @description
#' \code{check_model} computes tests to assess if the model went well. 
#' It is important to run this step before going ahead with the analysis otherwise you may make mistakes in the interpretation of the results.
#' 
#' @param x outputs from 
#' \itemize{
#'  \item \code{\link{model_GxE}}
#'  \item \code{\link{model_bh_intra_location}}
#'  \item \code{\link{model_bh_GxE}}
#'  \item \code{\link{model_spatial}}
#'  \item \code{\link{model_bh_variance_intra}}
#'  \item \code{\link{model_hedonic}}
#'  \item \code{\link{model_napping}}
#' }
#' 
#' @details
#' S3 method.
#' See for more details :
#' \itemize{
#'  \item \code{\link{check_model.fit_model_GxE}} regarding \code{\link{model_GxE}}
#'  \item \code{\link{check_model.fit_model_bh_intra_location}} regarding \code{\link{model_bh_intra_location}}
#'  \item \code{\link{check_model.fit_model_bh_GxE}} regarding \code{\link{model_bh_GxE}}
#'  \item \code{\link{check_model.fit_model_spatial}} regarding \code{\link{model_spatial}}
#'  \item \code{\link{check_model.fit_model_bh_variance_intra}} regarding \code{\link{model_bh_variance_intra}}
#'  \item \code{\link{check_model.fit_model_hedonic}} regarding \code{\link{model_hedonic}}
#'  \item \code{\link{check_model.fit_model_napping}} regarding \code{\link{model_napping}}
#' }
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{model_GxE}}
#'  \item \code{\link{model_bh_intra_location}}
#'  \item \code{\link{model_bh_GxE}}
#'  \item \code{\link{model_spatial}}
#'  \item \code{\link{model_bh_variance_intra}}
#'  \item \code{\link{model_hedonic}}
#'  \item \code{\link{model_napping}}
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{biplot_data}}
#'  \item \code{\link{plot.PPBstats}}
#' }
#' 
#' @export
#' 
check_model <- function(x) UseMethod("check_model")

check_model.default <- function(x) {
  ## Method not found
  ## This message needs updating whenever new models are included in PPBstats
  ## TODO: have a list of supported models somewhere, and use it to compose
  ## the error message automatically, or use a more generic message.
  mess = paste(substitute(x),
               "is a model type not yet fully supported by PPBstats")
  stop(mess)
}
