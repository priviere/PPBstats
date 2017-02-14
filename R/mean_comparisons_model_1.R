#' Get mean comparisons from check_model_model_1
#'
#' @description
#' \code{mean_comparisons_model_1} performs mean comparisons from object coming from \code{\link{check_model model_1}}
#' See \code{\link{mean_comparisons}} for more information.
#' 
#' @param out_check_model_1
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
mean_comparisons_model_1 = function(
  out_check_model_1,
  parameter,
  alpha = 0.05,
  type = 1,
  get.at.least.X.groups = 2,
  precision = 0.0005,
  threshold = 1,
  p.adj = "soft.bonf"
){
  # 1. Error message
  if( attributes(out_check_model_1)$PPBstats.object != "check_model_model_1" ) { stop("data must come from check_model and model_1") }
  
  if(!is.element(parameter, c("mu", "beta"))) { stop("With outputs from model 1, the parameters must be mu or beta.") }
  
  # 2. Get square matrice with pvalue or vector with pvalue ----------
  MCMC_par = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
    a = colnames(MCMC)[grep(paste("^", parameter, "\\[", sep = ""), colnames(MCMC))]
    vec_env = sub("\\]", "", unique(sapply(a, function(x){unlist(strsplit(x, ","))[2]})))
    vec_MCMC_par = lapply(vec_env, function(env, MCMC){ MCMC[grep(paste(",", env, "]", sep = ""), colnames(MCMC))] }, MCMC)
    out = lapply(vec_MCMC_par, get_mean_comparisons_and_Mpvalue, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) 
    
    fun = function(out, para){
      data = out$mean.comparisons
      data$entry = sub(paste(para, "\\[", sep=""), "", sapply(data$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
      data$environment =  sub("\\]", "", sapply(data$parameter, function(x){unlist(strsplit(as.character(x), ","))[2]}))
      data$location = sapply(data$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
      data$year = sapply(data$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
      out$mean.comparisons = data
      return(out)
    }
    out = lapply(out, fun, parameter)
    names(out) = vec_env
    return(out)
  }
  
  if(parameter == "mu") { data_mean_comparisons = MCMC_par(out_check_model_1$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) }
  
  if(parameter == "beta") { data_mean_comparisons = MCMC_par(out_check_model_1$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) }
  
  attributes(data_mean_comparisons)$PPBstats.object = "data_mean_comparisons"
  
  # 3. Format data_env_with_no_controls and data_env_whose_param_did_not_converge
  fun_format_data = function(x){
    vec_env = unique(x$environment)
    out = lapply(vec_env, function(env, x){ list("mean.comparisons" = filter(x, environment == env)) }, x)
    names(out) = vec_env
    return(out)
  }
  
  if( length(out_check_model_1$data_env_with_no_controls) > 0 ) { 
    data_env_with_no_controls = fun_format_data(out_check_model_1$data_env_with_no_controls)
    attributes(data_env_with_no_controls)$PPBstats.object = "data_env_with_no_controls"
  } else { 
    data_env_with_no_controls = out_check_model_1$data_env_with_no_controls
    }
  
  if( length(out_check_model_1$data_env_whose_param_did_not_converge) > 0 ) { 
    data_env_whose_param_did_not_converge = fun_format_data(out_check_model_1$data_env_whose_param_did_not_converge) 
    attributes(data_env_whose_param_did_not_converge)$PPBstats.object = "data_env_whose_param_did_not_converge"
  } else { 
      data_env_whose_param_did_not_converge = out_check_model_1$data_env_whose_param_did_not_converge 
      }
  
  # 4. Return results
  out = list(
    "data_mean_comparisons" = data_mean_comparisons,
    "data_env_with_no_controls" = data_env_with_no_controls,
    "data_env_whose_param_did_not_converge" = data_env_whose_param_did_not_converge
  )

  attributes(out)$PPBstats.object = "mean_comparisons_model_1"
  
  return(out)
}
