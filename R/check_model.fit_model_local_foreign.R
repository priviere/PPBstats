#' Check if the local foreign model went well
#'
#' @description
#' \code{check_model.fit_model_local_foreign} computes tests to assess if the model went well.
#' It is important to run this step before going ahead with the analysis otherwise you may make mistakes in the interpretation of the results.
#'
#' @param x outputs from \code{\link{model_local_foreign}}
#'
#' @details
#' S3 method.
#' The different test apply to the model are explained in the book \href{https://priviere.github.io/PPBstats_book/intro-agro.html#section-freq}{here}.
#'
#' @return It returns a list with the following elements:
#'
#' \itemize{
#'  \item model_local_foreign the output from the model
#'  \item data_ggplot a list containing information for ggplot:
#'  \itemize{
#'   \item data_ggplot_residuals a list containing :
#'    \itemize{
#'     \item data_ggplot_normality
#'     \item data_ggplot_skewness_test
#'     \item data_ggplot_kurtosis_test
#'     \item data_ggplot_qqplot
#'     }
#'   \item data_ggplot_variability_repartition_pie
#'   \item data_ggplot_var_intra
#'  }
#' }
#'
#' @author Pierre Riviere and Baptiste Rouger
#'
#' @seealso
#' \itemize{
#' \item \code{\link{check_model}}
#' \item \code{\link{plot.check_model_local_foreign}}
#' \item \code{\link{mean_comparisons}}
#' \item \code{\link{mean_comparisons.check_model_local_foreign}}
#' }
#'
#' @export
#'
check_model.fit_model_local_foreign <- function(x){
  model <- x$ANOVA$model

  out <- c(list("model_local_foreign" = x), "data_ggplot" = list(check_freq_anova(model)))

  class(out) <- c("PPBstats", "check_model_local_foreign")

  return(out)
}
