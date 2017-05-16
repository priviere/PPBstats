#' Get ggplot from mean_comparisons_predict_the_past_model_2
#'
#' @description
#' \code{ggplot_mean_comparisons_predict_the_past_model_2} returns ggplot from \code{\link{mean_comparisons_predict_the_past_model_2}}
#' 
#' @param out_mean_comparisons_predict_the_past_model_2 outputs from \code{\link{mean_comparisons_predict_the_past_model_2}}
#' 
#' @details See \code{\link{get_ggplot}}
#' 
#' @return See \code{\link{get_ggplot}}
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{get_ggplot}},
#' \item\code{\link{mean_comparisons_predict_the_past_model_2}}
#' }
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
  
  if(ggplot.type == "score" | ggplot.type == "interaction") {
    ylab = paste(
      out_mean_comparisons_predict_the_past_model_2$data_mean_comparisons[[1]]$mean.comparisons$entry,
      " (",
      out_mean_comparisons_predict_the_past_model_2$data_mean_comparisons[[1]]$mean.comparisons$parameter_statuts,
      ")", sep = ""
    )
    out_mean_comparisons_predict_the_past_model_2$data_mean_comparisons[[1]]$mean.comparisons$entry = ylab
  }
  
  out = ggplot_mean_comparisons_model_1(out_mean_comparisons_predict_the_past_model_2, data_version, ggplot.type, nb_parameters_per_plot)
  
  if(ggplot.type == "barplot") {
    p1 = lapply(out$data_mean_comparisons[[1]], function(x){ x + geom_bar(aes(fill = parameter_statuts), stat = "identity") } )
    out$data_mean_comparisons[[1]] = p1
  }
  
  out = out[1]
  
  return(out)
}
  