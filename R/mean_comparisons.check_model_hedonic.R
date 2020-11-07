#' Get mean comparisons from \code{\link{check_model.fit_model_hedonic}} object
#'
#' @description
#' \code{mean_comparisons} performs mean comparisons from object coming from \code{\link{check_model.fit_model_hedonic}}
#'
#' @param x outputs from \code{\link{check_model.fit_model_hedonic}}
#' 
#' @param alpha level of type one error. 0.05 (5\%) by default
#' 
#' @param p.adj For all except type = 2. 
#' NULL for no adjustement of the type one error. 
#' p.adj can "holm", "hochberg", "bonferroni", "BH", "BY" or "fdr"
#' p-adj = "none" is t-student.
#' See p.adjust() for more details.
#' 
#' @param ... further arguments passed to or from other methods#' 
#' 
#' @details
#' S3 method.
#' See in the book for more details : https://priviere.github.io/PPBstats_book/intro-agro.html#section-freq
#' 
#' @return 
#'  A list of two elements : 
#'   \itemize{
#'    \item info : a list with variable and data
#'    \item data_ggplot_LSDbarplot_germplasm
#'   }
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{plot.PPBstats}}
#'  \item \code{\link{plot.mean_comparisons_model_hedonic}}
#' }
#' 
#' @export
#' 
mean_comparisons.check_model_hedonic <- function(
  x, 
  alpha = 0.05,
  p.adj = "none",
  ...
){
  out = mean_comparisons_freq_anova(model = x$hedonic$model, variable = "note", alpha, p.adj)
  out = list(data_ggplot_LSDbarplot_germplasm = out$data_ggplot_LSDbarplot_germplasm)
  class(out) <- c("PPBstats", "mean_comparisons_model_GxE")
  return(out)
}