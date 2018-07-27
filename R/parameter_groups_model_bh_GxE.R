#' Get matrix with variables in column and effect in row from \code{\link{check_model.fit_model_bh_GxE}}
#'
#' @description
#' \code{parameter_groups_model_bh_GxE} gets matrix with variables in column and effect in row
#'
#' @param list_out_check_model_model_bh_GxE A list whose elements are output from \code{\link{check_model.fit_model_bh_GxE}}
#'  
#' @param parameter parameter on which to get the matrix
#' 
#' @return 
#' The function returns a data frame with variables in column and effect of paramters in row
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{parameter_groups}}
#'
#' @export
#'
parameter_groups_model_bh_GxE = function(
  list_out_check_model_model_bh_GxE,
  parameter
  ){
  
  # 1. Error message
  for(m in 1:length(list_out_check_model_model_bh_GxE)) {
    mcmc = list_out_check_model_model_bh_GxE[[m]]$MCMC
    if( length(grep(paste("^", parameter, "\\[", sep=""), colnames(mcmc))) == 0 ) { stop(parameter," is not in MCMC in list_out_check_model_model_bh_GxE") } 
  }
  
  
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
    
    # 2. fill the obj matrix  ----------
    for(m in 1:length(list_data)) {
      mcmc = list_data[[m]]$MCMC
      mcmc = mcmc[,grep(paste("^", parameter, "\\[", sep=""), colnames(mcmc))]
      obj[colnames(mcmc), names(list_data)[m]] = apply(mcmc, 2, median)
    }
    
    rownames(obj) = sapply(rownames(obj), function(x){ sub("\\]", "", sub(paste(parameter, "\\[", sep=""), "", x ) ) } )
    
    return(obj)
  }
  
  out = fun_get_effect_for_all_variables(list_out_check_model_model_bh_GxE, parameter)
    
  attributes(out)$PPBstats.object = "parameter_groups_model_bh_GxE"
  return(out)
}

