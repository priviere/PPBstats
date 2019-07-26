#' Get dataframe for factors' variation for each variable from a list whose elements are output from \code{\link{check_model_anova}}
#' 
#' @description 
#' \code{post_hoc_variation_model_anova} returns a dataframe for factors' variation for each variable from a list whose elements are output from \code{\link{check_model_anova}}
#'
#' @param list_out_check_model A list whose elements are output from \code{\link{check_model}}
#' 
#' @return 
#' A dataframe with the following columns: variable, factor and percentage sum of square coming from the model
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{post_hoc_variation}}
#' 
#' @export
#'
post_hoc_variation_model_anova = function(list_out_check_model){
  
  d = data.frame()
  for(i in 1:length(list_out_check_model)){
    factor = list_out_check_model[[i]]$data_ggplot$data_ggplot_variability_repartition_pie$factor
    percentage_Sum_sq = list_out_check_model[[i]]$data_ggplot$data_ggplot_variability_repartition_pie$percentage_Sum_sq
    variable = list_out_check_model[[i]]$model_anova$info$variable
    dtmp = data.frame(variable, factor, percentage_Sum_sq)
    d = rbind.data.frame(d, dtmp)  
  }
  
  return(d)
}
