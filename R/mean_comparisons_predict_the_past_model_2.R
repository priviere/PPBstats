#' Get mean comparisons from predict_the_past_model_2
#'
#' @description
#' \code{mean_comparisons_predict_the_past_model_2} performs mean comparisons from object coming from \code{\link{predict_the_past_model_2}}
#' See \code{\link{mean_comparisons}} for more information.
#' 
#' @param x
#' 
#' @param alpha
#' 
#' @param type
#' 
#' @param get.at.least.X.groups
#' 
#' @param precision
#' 
#' @param threshold
#' 
#' @param p.adj
#' 
#' @details See \code{\link{mean_comparisons}}
#' 
#' @return See \code{\link{mean_comparisons}}
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{get_ggplot}}
#' }
#'
mean_comparisons.predict_the_past_model_2 <- function(
  x,
  alpha = 0.05,
  type = 1,
  get.at.least.X.groups = 2,
  precision = 0.0005,
  threshold = 1,
  p.adj = "soft.bonf"
){

  out <- mean_comparisons.check_model_1(
    x, parameter = "mu", alpha = alpha, type = type, threshold = threshold,
    p.adj = p.adj, get.at.least.X.groups = get.at.least.X.groups,
    precision = precision)
  
  out <- out[1]
  
  d <- out$data_mean_comparisons[[1]]$mean.comparisons
  d$parameter_statuts <- x$parameter_statuts[d$parameter]
  
  out$data_mean_comparisons[[1]]$mean.comparisons <- d
  
  class(out) <- c("PPBstats", "mean_comparisons_predict_the_past_model_2")
  
  return(out)
}

