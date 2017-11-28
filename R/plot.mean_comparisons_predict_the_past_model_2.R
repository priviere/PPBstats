plot.mean_comparisons_predict_the_past_model_2 <- function(
  x,
  data_version = NULL,
  plot_type = "interaction",
  nb_parameters_per_plot = 8
){

  x = list(
    "data_mean_comparisons" = x[[1]],
    "data_env_with_no_controls" = NULL,
    "data_env_whose_param_did_not_converge" = NULL
  )
  
  attributes(x)$PPBstats.object = "mean_comparisons_model_1"
  
  if( !is.null(data_version) ) { stop("data_version must be NULL with data plot from predict_the_past()") }
  
  if(plot_type == "score" | plot_type == "interaction") {
    ylab = paste(
      x$data_mean_comparisons[[1]]$mean.comparisons$entry,
      " (",
      x$data_mean_comparisons[[1]]$mean.comparisons$parameter_statuts,
      ")", sep = ""
    )
    x$data_mean_comparisons[[1]]$mean.comparisons$entry = ylab
  }
  
  out = plot.mean_comparisons_model_1(x, data_version, plot_type, nb_parameters_per_plot)
  
  if(plot_type == "barplot") {
    p1 = lapply(out$data_mean_comparisons[[1]], function(x){ x + geom_bar(aes(fill = parameter_statuts), stat = "identity") } )
    out$data_mean_comparisons[[1]] = p1
  }
  
  out = out[1]
  
  return(out)
}
  