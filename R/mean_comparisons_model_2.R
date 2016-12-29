mean_comparisons_model_2 = function(
  out_check_model_2, 
  parameter,
  alpha = 0.05,
  type = 1,
  threshold = 1,
  p.adj = "soft.bonf",
  get.at.least.X.groups = 2,
  precision = 0.0005
  ){
  # 1. Error message
  if(!is.element(parameter, c("alpha", "beta", "theta"))) { stop("With outputs from model 2, the parameters must be alpha, beta or theta.") }
  
  # 2. Get square matrice with pvalue or vector with pvalue ----------
  MCMC_par = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
    MCMC_par = MCMC[,grep(parameter, colnames(MCMC))]
    out = get_mean_comparisons_and_Mpvalue(MCMC_par, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) 
    return(out)
  }
  
  if(parameter == "alpha") { out = list(MCMC_par(out_check_model_2$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups)) }
  
  if(parameter == "beta") { out = list(MCMC_par(out_check_model_2$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups)) }

  if(parameter == "theta") { out = list(MCMC_par(out_check_model_2$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups)) }
  

  # return results
  attributes(out)$PPBstats.object = "mean_comparisons_model_2"

 return(out)
}