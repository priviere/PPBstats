#' Get matrix with variables in column and effects in row from \code{\link{check_model.fit_model_anova}}
#'
#' @description
#' \code{parameter_groups_model_anova} gets matrix with variables in column and effects in row
#'
#' @param list_out_check_model_anova A list whose elements are output from \code{\link{check_model.fit_model_anova}}
#'  
#' @param parameter parameter on which to get the matrix: germplasm, location or var_intra_germplasm
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
parameter_groups_model_anova = function(
  list_out_check_model_anova,
  parameter
){
  match.arg(parameter, c("germplasm", "location", "var_intra_germplasm"), several.ok = FALSE)
  
  # 1. Prepare data set ----------
  n_G = n_E = n_varG = NULL
  for(i in 1:length(list_out_check_model_anova)){
    n_G = c(n_G, names(list_out_check_model_anova[[i]]$model_anova$ANOVA$germplasm_effects$effects))
    n_varG = c(n_varG, names(list_out_check_model_anova[[i]]$model_anova$ANOVA$germplasm_effects$intra_variance))
  }
  n_G = unique(n_G)
  n_varG = unique(n_varG)
  
  df_G = matrix(NA, ncol = length(list_out_check_model_anova), nrow = length(n_G))
  colnames(df_G) = names(list_out_check_model_anova)
  rownames(df_G) = n_G
  
  df_varG = matrix(NA, ncol = length(list_out_check_model_anova), nrow = length(n_varG))
  colnames(df_varG) = names(list_out_check_model_anova)
  rownames(df_varG) = n_varG
  
  for(i in 1:length(list_out_check_model_anova)){
    g = list_out_check_model_anova[[i]]$model_anova$ANOVA$germplasm_effects$effects
    df_G[names(g),i] = g
    vg = list_out_check_model_anova[[i]]$model_anova$ANOVA$germplasm_effects$intra_variance
    df_varG[names(vg),i] = vg
  }
  
  if(parameter == "germplasm") { out = df_G }
  if(parameter == "var_intra_germplasm") { out = df_varG }
  
  # 3. Return results
  return(out)
}
