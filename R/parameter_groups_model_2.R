parameter_groups_model_2 = function(
  list_out_check_model_model_2,
  parameter,
  nb.clust = -1
  ){
  
  fun_get_effect_for_all_variables = function(list_data, parameter){
    # 1. Create the obj matrix ----------
    MCMC = obj.rownames = NULL
    
    for(m in 1:length(list_data)) {
      mcmc = list_data[[m]]$MCMC
      mcmc = mcmc[,grep(paste("^", parameter, "\\[", sep=""), colnames(mcmc))]
      obj.rownames = c(obj.rownames, colnames(mcmc))
      MCMC = c(MCMC, list(mcmc))
    }
    
    obj.rownames = unique(obj.rownames)
    obj = matrix(NA, ncol = length(list_data), nrow = length(obj.rownames))
    rownames(obj) = obj.rownames
    colnames(obj) = names(list_data)
    
    # 3. fill the obj matrix  ----------
    for(m in 1:length(list_data)) {
      mcmc = list_data[[m]]$MCMC
      mcmc = mcmc[,grep(paste("^", parameter, "\\[", sep=""), colnames(mcmc))]
      obj[colnames(mcmc), names(list_data)[m]] = apply(mcmc, 2, median)
    }
    
    rownames(obj) = sapply(rownames(obj), function(x){ sub("\\]", "", sub(paste(parameter, "\\[", sep=""), "", x ) ) } )
    
  }
  
  out = fun_get_effect_for_all_variables(list_out_check_model_model_2, parameter)
    
  
  return(out)
}

