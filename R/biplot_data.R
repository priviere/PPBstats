#' Format data for biplot representation 
#'
#' @description
#' \code{biplot_data} formats data for biplot representation 
#' 
#' @param x outputs from \code{\link{model_GxE}}, \code{\link{model_hedonic}},  \code{\link{model_napping}}
#' 
#' @details
#' S3 method.
#' See for more details :
#' \itemize{
#'  \item biplot_data.check_model_GxE
#'  \item biplot_data.check_model_hedonic
#'  \item biplot_data.check_model_napping
#' }
#' 
biplot_data <- function(x) UseMethod("biplot_data")

biplot_data.default <- function(x) {
  ## Method not found
  ## This message needs updating whenever new models are included in PPBstats
  ## TODO: have a list of supported models somewhere, and use it to compose
  ## the error message automatically, or use a more generic message.
  mess = paste(substitute(x),
               "is a model type not yet fully supported by PPBstats")
  stop(mess)
}

