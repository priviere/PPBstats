parameter_groups_model_2 = function(
  out_check_model_model_2,
  parameter,
  nb.clust = -1
  ){
  
  # 2. Create the obj matrix ----------
  MCMC = obj.rownames = NULL
  
  for(m in 1:length(out_check_model_model_2)) {
    mcmc = out_check_model_model_2[[m]]$MCMC
    mcmc = mcmc[,grep(paste(parameter, "\\[", sep=""), colnames(mcmc))]
    obj.rownames = c(obj.rownames, colnames(mcmc))
    MCMC = c(MCMC, list(mcmc))
  }
  
  obj.rownames = unique(obj.rownames)
  obj = matrix(NA, ncol = length(out_check_model_model_2), nrow = length(obj.rownames))
  rownames(obj) = obj.rownames
  colnames(obj) = names(out_check_model_model_2)
  
  # 3. fill the obj matrix  ----------
  for(m in 1:length(out_check_model_model_2)) {
    mcmc = out_check_model_model_2[[m]]$MCMC
    mcmc = mcmc[,grep(paste(parameter, "\\[", sep=""), colnames(mcmc))]
    obj[colnames(mcmc), names(out_check_model_model_2)[m]] = apply(mcmc, 2, median)
  }
  
  rownames(obj) = sapply(rownames(obj), function(x){ sub("\\]", "", sub(paste(parameter, "\\[", sep=""), "", x ) ) } )
  
  return(out)
}

