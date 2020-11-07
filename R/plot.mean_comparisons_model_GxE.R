#' Get ggplot to visualize output from \code{\link{mean_comparisons.check_model_GxE}}
#'
#' @description
#' \code{plot.mean_comparisons_model_GxE} returns ggplot to visualize outputs from \code{\link{mean_comparisons.check_model_GxE}}
#'
#' @param x Output from \code{\link{mean_comparisons.check_model_GxE}} 
#' 
#' @param nb_parameters_per_plot number of parameter per plot to display
#' 
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' See example in the book regarding 
#' \href{https://priviere.github.io/PPBstats_book/family-2.html#ammi}{AMMI} and 
#' \href{https://priviere.github.io/PPBstats_book/family-2.html#gge}{GGE}.
#' 
#' @return 
#' A list with barplot.
#' For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#' Letters are displayed on each bar. Parameters that do not share the same letters are different regarding type I error (alpha) and alpha correction. 
#' The error I (alpha) and the alpha correction are displayed in the title. 
#' \itemize{
#'  \item germplasm : mean comparison for germplasm
#'  \item location : mean comparison for location
#'  \item year : mean comparison for year
#'  }
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{mean_comparisons.check_model_GxE}}
#' 
#' @export
#' 
plot.mean_comparisons_model_GxE <- function(
  x,
  nb_parameters_per_plot = 8, ...
  ){
  out = plot_mean_comparisons_freq_anova(x, variable = x$info$variable, nb_parameters_per_plot)
  return(out)
}

