#' Get mean comparisons from check_model_model_2
#'
#' @description
#' \code{mean_comparisons_model_2} performs mean comparisons from object coming from \code{\link{check_model model_2}}
#' See \code{\link{mean_comparisons}} for more information.
#' 
#' @param x
#' 
#' @param parameter
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
mean_comparisons.check_model_2 <- function(
  x, 
  parameter,
  alpha = 0.05,
  type = 1,
  get.at.least.X.groups = 2,
  precision = 0.0005,
  threshold = 1,
  p.adj = "soft.bonf"
  ){
  
  # 1. Error message
  if(!is.element(parameter, c("alpha", "beta", "theta"))) {
    stop("With outputs from model 2, the parameters must be alpha, beta or theta.")
  }
  
  # 2. Get square matrice with pvalue or vector with pvalue ----------
  MCMC_par = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
    MCMC_par = MCMC[,grep(paste("^", parameter, "\\[", sep = ""), colnames(MCMC))]
    out = get_mean_comparisons_and_Mpvalue(MCMC_par, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) 
    return(out)
  }
  
  out <- MCMC_par(x$MCMC, parameter, type, threshold, alpha,
                  p.adj, precision, get.at.least.X.groups)

  # return results
  class(out) <- c("PPBstats", "mean_comparisons_model_2")
  return(out)
}