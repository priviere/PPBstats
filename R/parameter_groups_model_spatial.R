#' Get matrix with variables in column and effects in row from \code{\link{check_model.fit_model_spatial}}
#'
#' @description
#' \code{parameter_groups_model_spatial} gets matrix with variables in column and effects in row
#'
#' @param list_out_check_model_spatial A list whose elements are output from \code{\link{check_model.fit_model_spatial}}
#'  
#' @param parameter parameter on which to get the matrix: germplasm 
#' 
#' @return 
#' The function returns a data frame with variables in column and effect of paramters in row
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{parameter_groups}}
#'
#'
#' @import SpATS
#' 
#' @export
#'
parameter_groups_model_spatial = function(
  list_out_check_model_spatial,
  parameter
){
  n_G = NULL
  for(i in 1:length(list_out_check_model_spatial)){
    pre = SpATS::predict.SpATS(list_out_check_model_spatial[[i]]$spatial$model$model, which = "germplasm")
    n_G = c(n_G, as.character(pre$germplasm))
  }
  n_G = unique(n_G)
  
  df_G = matrix(NA, ncol = length(list_out_check_model_spatial), nrow = length(n_G))
  colnames(df_G) = names(list_out_check_model_spatial)
  rownames(df_G) = n_G
  
  for(i in 1:length(list_out_check_model_spatial)){
    pre = SpATS::predict.SpATS(list_out_check_model[[i]]$spatial$model$model, which = "germplasm")
    df_G[as.character(pre$germplasm), i] = pre$predicted.values
  }
  
  if(parameter == "germplasm") { out = df_G }

  return(out)
}
