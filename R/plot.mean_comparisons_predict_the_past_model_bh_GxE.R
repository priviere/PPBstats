#' Get ggplot to visualize output from \code{\link{mean_comparisons.predict_the_past_model_bh_GxE}}
#'
#' @description
#' \code{plot.mean_comparisons_predict_the_past_model_bh_GxE} returns ggplot to visualize outputs from \code{\link{mean_comparisons.predict_the_past_model_bh_GxE}}
#'
#' @param x Output from \code{\link{mean_comparisons.predict_the_past_model_bh_GxE}} 
#' 
#' @param data_version Output from \code{\link{format_data_PPBstats.data_agro_version}} 
#' 
#' @param plot_type "interaction", "barplot" or "score"
#' 
#' @param nb_parameters_per_plot number of parameter per plot to display
#'
#' @param ... further arguments passed to or from other methods
#' 
#' @details
#' S3 method.
#' See example in the book: https://priviere.github.io/PPBstats_book/family-2.html#model-2
#' 
#' @return 
#' It is the same outputs than for \code{\link{plot.mean_comparisons_model_bh_intra_location}}
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{mean_comparisons.predict_the_past_model_bh_GxE}}
#' 
#' @export
#' 
plot.mean_comparisons_predict_the_past_model_bh_GxE <- function(
  x,
  data_version = NULL,
  plot_type = "interaction",
  nb_parameters_per_plot = 8, ...
){
  parameter_statuts = NULL # to avoid no visible binding for global variable
  
  x = list(
    "data_mean_comparisons" = x[[1]],
    "data_env_with_no_controls" = NULL,
    "data_env_whose_param_did_not_converge" = NULL
  )
  
  attributes(x)$PPBstats.object = "mean_comparisons_model_bh_intra_location"
  
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
  
  out = plot.mean_comparisons_model_bh_intra_location(x, data_version, plot_type, nb_parameters_per_plot)
  
  if(plot_type == "barplot") {
    p1 = lapply(out$data_mean_comparisons[[1]], function(x){ x + geom_bar(aes(fill = parameter_statuts), stat = "identity") } )
    out$data_mean_comparisons[[1]] = p1
  }
  
  out = out[1]
  
  return(out)
}
  