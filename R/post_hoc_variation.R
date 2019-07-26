#' Plot a barplot with factors' variation for each variable
#' 
#' @description 
#' \code{post_hoc_variation} plots a barplot with factors' variation for each variable
#'
#' @param list_out_check_model A list whose elements are output from \code{\link{check_model}}
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{check_model}}
#'  \item \code{\link{parameter_groups_model_GxE}}
#' }
#' 
#' @export
#'
#'
post_hoc_variation = function(list_out_check_model){
  
  # 1. Error message ----------
  all_by_model = check_list_out_check_model(valid_models = c("check_model_GxE", "check_model_anova", "check_model_spatial"), list_out_check_model)
  
  # 2. Get dataframe
  ## function look-up (in the order of valid_models)
  get_df =  c(post_hoc_variation_model_GxE, post_hoc_variation_model_anova, post_hoc_variation_model_spatial)[[which(all_by_model)]]
  df = get_df(list_out_check_model)
  
  # 3. Get ggplot
  p = ggplot(df, aes(x = variable, y = percentage_Sum_sq, fill = factor)) + geom_bar(stat = "identity") 
  
  return(p)
}
