#' Get mean comparisons from \code{\link{check_model.fit_model_GxE}} object
#'
#' @description
#' \code{mean_comparisons} performs mean comparisons from object coming from \code{\link{check_model.fit_model_GxE}}
#'
#' @param x outputs from \code{\link{check_model.fit_model_GxE}}
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
#'  A list of four elements : 
#'   \itemize{
#'    \item info : a list with variable and gxe_analysis
#'    \item data_ggplot_LSDbarplot_germplasm
#'    \item data_ggplot_LSDbarplot_location
#'    \item data_ggplot_LSDbarplot_year
#'   }
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{plot.PPBstats}}
#'  \item \code{\link{plot.mean_comparisons_model_GxE}}
#' }
#' 
#' @export
#' 
mean_comparisons.check_model_GxE <- function(
  x, 
  alpha = 0.05,
  p.adj = "none",
  ...
){
  out = mean_comparisons_freq_anova(model = x$GxE$ANOVA$model, variable = x$GxE$info$variable, 
                                    alpha, p.adj, info = x$info)
  class(out) <- c("PPBstats", "mean_comparisons_model_GxE")
  return(out)
}