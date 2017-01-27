#' Get ggplot objects from mean_comparisons_predict_the_past_model_2
#'
#' @description
#' \code{ggplot_mean_comparisons_predict_the_past_model_2} returns ggplot objects from mean_comparisons_predict_the_past_model_2
#' 
#' @param out_mean_comparisons_predict_the_past_model_2 outputs from mean_comparisons_predict_the_past_model_2 function
#' 
#' @details See get_ggplot
#' 
#' @return See get_ggplot
#' 
#' @seealso \code{\link{get_ggplot}}, \code{\link{mean_comparisons_predict_the_past_model_2}}
#'
ggplot_mean_comparisons_predict_the_past_model_2 = function(
  out_mean_comparisons_predict_the_past_model_2,
  data_version = NULL,
  ggplot.type = "interaction",
  nb_parameters_per_plot = 10
){
  if( attributes(out_mean_comparisons_predict_the_past_model_2)$PPBstats.object != "mean_comparisons_predict_the_past_model_2" ) { stop("data must come from mean_comparisons_predict_the_past_model_2") }
  
  out_mean_comparisons_predict_the_past_model_2 = list(
    "data_mean_comparisons" = out_mean_comparisons_predict_the_past_model_2[[1]],
    "data_env_with_no_controls" = NULL,
    "data_env_whose_param_did_not_converge" = NULL
  )
  
  attributes(out_mean_comparisons_predict_the_past_model_2)$PPBstats.object = "mean_comparisons_model_1"
  
  out = ggplot_mean_comparisons_model_1(out_mean_comparisons_predict_the_past_model_2, data_version, ggplot.type, nb_parameters_per_plot)
  out = out[1]
  
  return(out)
}
  