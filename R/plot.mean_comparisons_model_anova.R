#' Get ggplot to visualize output from \code{\link{mean_comparisons.check_model_anova}}
#'
#' @description
#' \code{plot.mean_comparisons_model_anova} returns ggplot to visualize outputs from \code{\link{mean_comparisons.check_model_anova}}
#'
#' @param x Output from \code{\link{mean_comparisons.check_model_anova}} 
#' 
#' @param nb_parameters_per_plot number of parameter per plot to display
#' 
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
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
#' @seealso \code{\link{mean_comparisons.check_model_anova}}
#' 
#' @export
#' 
plot.mean_comparisons_model_anova <- function(
  x,
  nb_parameters_per_plot = 8, ...
  ){
  out = plot_mean_comparisons_freq_anova(x, variable = x$info$variable, nb_parameters_per_plot)
  return(out)
}

