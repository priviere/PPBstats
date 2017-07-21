mean_comparisons.check_model_varintra = function(
  data,
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
  if( attributes(data)$PPBstats.object != "model_varintra" ) { stop("data must come from check_model and model_1") }
  
  if(!is.element(parameter, c("mu", "sigma"))) { stop("With outputs from model 1, the parameters must be mu or sigma") }
  
  MCMC = rbind.data.frame(as.data.frame(data$MCMC[[1]],as.data.frame(data$MCMC[[2]])))
  
  # 2. Get square matrice with pvalue or vector with pvalue ----------
  MCMC_par = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
    a = colnames(MCMC)[grep(paste("^", parameter, "\\[", sep = ""), colnames(MCMC))]
    vec_env = unique(unlist(lapply(a,function(x){ex_between(x, right = "]", left=",")})))
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
  
  data_mean_comparisons = MCMC_par(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups)

  attributes(data_mean_comparisons)$PPBstats.object = "data_mean_comparisons"
  
  return(data_mean_comparisons)
}
