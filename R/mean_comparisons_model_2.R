#' Get mean comparisons from object coming from check_model model_2
#'
#' @description
#' \code{mean_comparisons_model_2} performs mean comparisons from object coming from check_model model_2
#' See \code{mean_comparisons} for more information.
#' 
#' @param out_check_model_2
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
#' @details See mean_comparisons
#' 
#' @return See mean_comparisons
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{get_ggplot}}
#' }
#'
mean_comparisons_model_2 = function(
  out_check_model_2, 
  parameter,
  alpha = 0.05,
  type = 1,
  get.at.least.X.groups = 2,
  precision = 0.0005,
  threshold = 1,
  p.adj = "soft.bonf"
  ){
  # 1. Error message
  if(!is.element(parameter, c("alpha", "beta", "theta"))) { stop("With outputs from model 2, the parameters must be alpha, beta or theta.") }
  
  # 2. Get square matrice with pvalue or vector with pvalue ----------
  MCMC_par = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
    MCMC_par = MCMC[,grep(paste("^", parameter, "\\[", sep = ""), colnames(MCMC))]
    out = get_mean_comparisons_and_Mpvalue(MCMC_par, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) 
    return(out)
  }
  
  if(parameter == "alpha") { out = MCMC_par(out_check_model_2$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) }
  
  if(parameter == "beta") { out = MCMC_par(out_check_model_2$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) }

  if(parameter == "theta") { out = MCMC_par(out_check_model_2$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) }
  

  # return results
  attributes(out)$PPBstats.object = "mean_comparisons_model_2"

 return(out)
}