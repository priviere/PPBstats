mean_comparisons_model_1 = function(
  out_check_model_1,
  parameter,
  alpha = 0.05,
  type = 1,
  threshold = 1,
  p.adj = "soft.bonf",
  get.at.least.X.groups = 2,
  precision = 0.0005
){
  # 1. Error message
  if( attributes(out_check_model_1)$PPBstats.object != "check_model_model_1" ) { stop("data must come from check_model and model_1") }
  
  if(!is.element(parameter, c("mu", "beta"))) { stop("With outputs from model 1, the parameters must be mu or beta.") }
  
  # 2. Get square matrice with pvalue or vector with pvalue ----------
  MCMC_par = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
    a = colnames(MCMC)[grep(parameter, colnames(MCMC))]
    vec_env = sub("\\]", "", unique(sapply(a, function(x){unlist(strsplit(x, ","))[2]})))
    vec_MCMC_par = lapply(vec_env, function(env, MCMC){ MCMC[grep(paste(",", env, "]", sep = ""), colnames(MCMC))] }, MCMC)
    out = lapply(vec_MCMC_par, get_mean_comparisons_and_Mpvalue, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) 
    names(out) = vec_env
    return(out)
  }
  
  if(parameter == "mu") { mean_comparisons = MCMC_par(out_check_model_1$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) }
  
  if(parameter == "beta") { mean_comparisons = MCMC_par(out_check_model_1$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) }
  
  # return results
  out = list(
    "data_mean_comparisons" = mean_comparisons,
    "data_env_with_no_controls" = out_check_model_1$data_env_with_no_controls,
    "data_env_whose_param_did_not_converge" = out_check_model_1$data_env_whose_param_did_not_converge
  )

  attributes(out)$PPBstats.object = "mean_comparisons_model_1"
  
  return(out)
}
