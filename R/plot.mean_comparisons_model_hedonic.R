#' Get ggplot to visualize output from \code{\link{mean_comparisons.check_model_GxE}}
#'
#' @description
#' \code{plot.mean_comparisons_model_hedonic} returns ggplot to visualize outputs from \code{\link{mean_comparisons.check_model_hedonic}}
#'
#' @param x Output from \code{\link{mean_comparisons.check_model_hedonic}} 
#' 
#' @param nb_parameters_per_plot number of parameter per plot to display
#'
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' See example in the book: https://priviere.github.io/PPBstats_book/hedonic.html#get-and-visualize-mean-comparisons-6
#' 
#' @return 
#' A list with barplot.
#' For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#' Letters are displayed on each bar. Parameters that do not share the same letters are different regarding type I error (alpha) and alpha correction. 
#' The error I (alpha) and the alpha correction are displayed in the title. 
#' \itemize{
#'  \item germplasm : mean comparison for germplasm
#'  }
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{mean_comparisons.check_model_hedonic}}
#' 
#' @export
#' 
plot.mean_comparisons_model_hedonic <- function(
  x,
  nb_parameters_per_plot = 8, ...
){
  out = plot_mean_comparisons_freq_anova(x, variable = "note", nb_parameters_per_plot)
  out = list(germplasm = out$germplasm)
  return(out)
}

