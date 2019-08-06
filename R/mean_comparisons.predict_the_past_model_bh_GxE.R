#' Get mean comparisons from \code{\link{predict_the_past_model_bh_GxE}} object
#'
#' @description
#' \code{mean_comparisons} performs mean comparisons from object coming from \code{\link{predict_the_past_model_bh_GxE}}
#'
#' @param x outputs from \code{\link{predict_the_past_model_bh_GxE}}
#' 
#' @param alpha level of type one error. 0.05 (5\%) by default
#' 
#' @param type type of comparisons
#' \itemize{
#'  \item type = 1 for comparison two by two
#'  \item type = 2 for comparison to a specific threshold
#' }
#' 
#' @param get.at.least.X.groups For type = 1. 
#' If there are only one group with alpha, the minimum number of groups wanted with a higher type one error (i.e. lower confidence). 
#' If NULL, nothing is done.
#' 
#' @param precision For type = 1. The precision of the alpha with the correspondong groups from get.at.least.X.groups. The smaller the better, but the smaller the more time consuming due to computing matters
#' 
#' @param threshold For type = 2. The threshold to which a parameter is different
#' 
#' @param p.adj For all except type = 2. 
#' NULL for no adjustement of the type one error. 
#' p.adj can be "soft.bonf". 
#' 
#' p.adj = "soft.bonf" for a soft bonferonni correction to take into account multiple comparisons (alpha / nb of parameters)..
#' The comparisons is based on the probability of having a common distribution for each pair of parameter.
#' When there is only one group with the value of alpha, the function (via \code{get.at.least.X.groups argument}) returns at least X groups with a new value of alpha.
#' 
#' @param ... further arguments passed to or from other methods#' 
#' 
#' @details 
#' S3 method.
#' For more details, see in the book : https://priviere.github.io/PPBstats_book/intro-agro.html#section-bayes
#' 
#' @return 
#'  A list of one element : data_mean_comparisons, composed of a list of one element for the given environment choose, being a list of two elements: 
#'    \itemize{
#'     \item mean.comparisons: a dataframe with the following columns : parameter, median, groups, number of groups, alpha (type one error), alpha.correction (correction used), entry, environment, location and year.
#'     \item Mpvalue : a square matrix with pvalue computed for each pair of parameter.
#'    }
#' 
#' parameter is set to "mu" by default
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{plot.PPBstats}}
#'  \item \code{\link{plot.mean_comparisons_predict_the_past_model_bh_GxE}}
#' }
#' 
#' @export
#' 
mean_comparisons.predict_the_past_model_bh_GxE <- function(
  x,
  alpha = 0.05,
  type = 1,
  get.at.least.X.groups = 2,
  precision = 0.0005,
  threshold = 1,
  p.adj = "soft.bonf",
  ...
){
  match.arg(p.adj, "soft.bonf", several.ok = FALSE)
  
  out <- mean_comparisons.check_model_bh_intra_location(
    x, parameter = "mu", alpha = alpha, type = type, threshold = threshold,
    p.adj = p.adj, get.at.least.X.groups = get.at.least.X.groups,
    precision = precision)
  
  out <- out[1]
  
  d <- out$data_mean_comparisons[[1]]$mean.comparisons
  d$parameter_statuts <- x$parameter_statuts[d$parameter]
  
  out$data_mean_comparisons[[1]]$mean.comparisons <- d
  
  class(out) <- c("PPBstats", "mean_comparisons_predict_the_past_model_bh_GxE")
  
  return(out)
}

