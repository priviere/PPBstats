#' Get mean comparisons from \code{\link{check_model.fit_model_bh_intra_location}} object
#' 
#' @description
#' \code{mean_comparisons} performs mean comparisons from object coming from \code{\link{check_model.fit_model_bh_intra_location}}
#'
#' @param x outputs from \code{\link{check_model.fit_model_bh_intra_location}}
#'  
#' @param parameter parameter on which the mean comparison is done. 
#' The possible values are "mu" and "beta"
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
#'A list of three elements : 
#'   \itemize{
#'    \item data_mean_comparisons a list with as many elements as environment.
#'    Each element of the list is composed of two elements:
#'    \itemize{
#'     \item mean.comparisons: a dataframe with the following columns : parameter, median, groups, number of groups, alpha (type one error), alpha.correction (correction used), entry, environment, location and year.
#'     \item Mpvalue : a square matrix with pvalue computed for each pair of parameter.
#'    }
#'    
#'    \item data_env_with_no_controls a list with as many elements as environment.
#'    In each list it is mean.comparisons : a dataframe with the following columns : parameter, median, groups, number of groups, alpha (type one error), alpha.correction (correction used), entry, environment, location and year.
#'    
#'    \item data_env_whose_param_did_not_converge  a list with as many elements as environment.
#'     In each list it is mean.comparisons : a dataframe with the following columns : entry, germplasm, environment, block, X, Y, ID, median, parameter.
#'   }
#'  
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{plot.PPBstats}}
#'  \item \code{\link{plot.mean_comparisons_model_bh_intra_location}}
#' }
#' 
#' @export
#' 
mean_comparisons.check_model_bh_intra_location <- function(
  x,
  parameter,
  alpha = 0.05,
  type = 1,
  get.at.least.X.groups = 2,
  precision = 0.0005,
  threshold = 1,
  p.adj = "soft.bonf",
  ...
){
  # 1. Error message
  match.arg(parameter, c("mu", "beta"), several.ok = FALSE)
  match.arg(p.adj, "soft.bonf", several.ok = FALSE)
  
  # 2. Get square matrice with pvalue or vector with pvalue ----------
  MCMC_par = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
    a = colnames(MCMC)[grep(paste("^", parameter, "\\[", sep = ""), colnames(MCMC))]
    vec_env = sub("\\]", "", unique(sapply(a, function(x){unlist(strsplit(x, ","))[2]})))
    vec_MCMC_par = lapply(vec_env, function(env, MCMC){ MCMC[grep(paste(",", env, "]", sep = ""), colnames(MCMC))] }, MCMC)
    out = lapply(vec_MCMC_par, get_mean_comparisons_and_Mpvalue, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) 
    
    fun = function(out, para){
      data = out$mean.comparisons
      data$entry = sub(paste(para, "\\[", sep=""), "", sapply(data$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
      data$environment =  sub("\\]", "", sapply(data$parameter, function(x){unlist(strsplit(as.character(x), ","))[2]}))
      data$location = sapply(data$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
      data$year = sapply(data$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
      out$mean.comparisons = data
      return(out)
    }
    out = lapply(out, fun, parameter)
    names(out) = vec_env
    return(out)
  }
  
  data_mean_comparisons = MCMC_par(x$MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups)
  
  attributes(data_mean_comparisons)$PPBstats.object = "data_mean_comparisons"
  
  # 3. Format data_env_with_no_controls and data_env_whose_param_did_not_converge
  fun_format_data = function(x){
    vec_env = unique(x$environment)
    out = lapply(vec_env, function(env, x){ list("mean.comparisons" = filter(x, environment == env)) }, x)
    names(out) = vec_env
    return(out)
  }
  
  if( length(x$data_env_with_no_controls) > 0 ) { 
    data_env_with_no_controls = fun_format_data(x$data_env_with_no_controls)
    attributes(data_env_with_no_controls)$PPBstats.object = "data_env_with_no_controls"
  } else { 
    data_env_with_no_controls = x$data_env_with_no_controls
    }
  
  if( length(x$data_env_whose_param_did_not_converge) > 0 ) { 
    data_env_whose_param_did_not_converge = fun_format_data(x$data_env_whose_param_did_not_converge) 
    attributes(data_env_whose_param_did_not_converge)$PPBstats.object = "data_env_whose_param_did_not_converge"
  } else { 
      data_env_whose_param_did_not_converge = x$data_env_whose_param_did_not_converge 
      }
  
  # 4. Return results
  out <- list(
    "data_mean_comparisons" = data_mean_comparisons,
    "data_env_with_no_controls" = data_env_with_no_controls,
    "data_env_whose_param_did_not_converge" = data_env_whose_param_did_not_converge
  )

  class(out) <- c("PPBstats", "mean_comparisons_model_bh_intra_location")
  
  return(out)
}
