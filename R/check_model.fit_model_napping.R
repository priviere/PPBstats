#' Check if the napping model went well 
#'
#' @description
#' \code{check_model.fit_model_napping} computes tests to assess if the model went well. 
#' It is important to run this step before going ahead with the analysis otherwise you may make mistakes in the interpretation of the results.
#' 
#' @param x outputs from \code{\link{model_napping}}
#' 
#' @details
#' S3 method.
#' 
#' @return It returns a list with the same elements as \code{\link{model_napping}} as the MFA object is already computed
#' 
#' @author Pierre Riviere
#'
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}
#' \item \code{\link{plot.check_model_napping}}
#' \item \code{\link{biplot_data}}
#' \item \code{\link{biplot_data.check_model_napping}}
#' }
#' 
#' @export
#' 
check_model.fit_model_napping = function(x){
  out = x
  class(out) <- c("PPBstats", "check_model_napping")
  return(out) 
}