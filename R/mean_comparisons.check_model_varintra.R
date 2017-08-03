mean_comparisons.check_model_variance_intra = function(
  x,
  parameter,
  alpha = 0.05,
  type = 1,
  get.at.least.X.groups = 2,
  precision = 0.0005,
  threshold = 1,
  p.adj = "soft.bonf"
){
  
  library(qdapRegex)
  # 1. Error message

  if(!is.element(parameter, c("mu", "sigma"))) { stop("With outputs from model 1, the parameters must be mu or sigma") }
  
  MCMC =x$MCMC
  
  # 2. Get square matrice with pvalue or vector with pvalue ----------
  MCMC_par = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
    a = colnames(MCMC)[grep(paste("^", parameter, "\\[", sep = ""), colnames(MCMC))]
    vec_env = unique(unlist(lapply(a,function(x){ex_between(x, right = "]", left=",")})))
    vec_MCMC_par = lapply(vec_env, function(env, MCMC){ MCMC[grep(paste(",", env, "]", sep = ""), colnames(MCMC))] }, MCMC)
    out = lapply(vec_MCMC_par, get_mean_comparisons_and_Mpvalue, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) 
    
    fun = function(out, para){
      x = out$mean.comparisons
      x$entry = sub(paste(para, "\\[", sep=""), "", sapply(x$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
      x$environment =  sub("\\]", "", sapply(x$parameter, function(x){unlist(strsplit(as.character(x), ","))[2]}))
      x$location = sapply(x$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
      x$year = sapply(x$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
      out$mean.comparisons = x
      return(out)
    }
    out = lapply(out, fun, parameter)
    names(out) = vec_env
    return(out)
  }
  
  data_mean_comparisons = MCMC_par(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups)
  attributes(data_mean_comparisons)$PPBstats.object = "data_mean_comparisons"
  
  # 3. Format data_env_with_no_controls and data_env_whose_param_did_not_converge
  # to do !!!
  data_env_with_no_controls = NULL
  data_env_whose_param_did_not_converge = NULL
  
  # 4. Return results
  out <- list(
    "data_mean_comparisons" = data_mean_comparisons,
    "data_env_with_no_controls" = data_env_with_no_controls,
    "data_env_whose_param_did_not_converge" = data_env_whose_param_did_not_converge
  )
  
  class(out) <- c("PPBstats", "mean_comparisons_model_variance_intra")
  
  return(out)
}
