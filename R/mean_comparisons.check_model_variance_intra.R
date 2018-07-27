#' Get mean comparisons from \code{\link{check_model.fit_model_bh_variance_intra}} object
#' 
#' @description
#' \code{mean_comparisons} performs mean comparisons from object coming from \code{\link{check_model.fit_model_bh_variance_intra}}
#'
#' @param x outputs from \code{\link{check_model.fit_model_bh_variance_intra}}
#'  
#' @param parameter parameter on which the mean comparison is done. 
#' The possible values are "mu" and "sigma"
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
#'  A list of one elements : 
#'   \itemize{
#'    \item data_mean_comparisons a list with as many elements as environment.
#'    Each element of the list is composed of two elements:
#'    \itemize{
#'     \item mean.comparisons: a dataframe with the following columns : parameter, median, groups, number of groups, alpha (type one error), alpha.correction (correction used), entry, environment, location and year.
#'     \item Mpvalue : a square matrix with pvalue computed for each pair of parameter.
#'    }
#'   }
#'  
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{plot.PPBstats}}
#' }
#' 
#' @export
#' 
mean_comparisons.check_model_bh_variance_intra = function(
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

  if(!is.element(parameter, c("mu", "sigma"))) { stop("With outputs from bayesian hierarchical variance intra model, the parameters must be mu or sigma") }
  
  MCMC =x$MCMC
  
  # 2. Get square matrice with pvalue or vector with pvalue ----------
  MCMC_par = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
    a = colnames(MCMC)[grep(paste("^", parameter, "\\[", sep = ""), colnames(MCMC))]
    vec_env = unique(unlist(lapply(a,function(x){b = strsplit(x,",")[[1]][2]; b=strsplit(b,":")[[1]][1]})))
    vec_MCMC_par = lapply(vec_env, function(env, MCMC){ MCMC[grep(env, colnames(MCMC))] }, MCMC)
    out = lapply(vec_MCMC_par, get_mean_comparisons_and_Mpvalue, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups) 
    
    fun = function(out, para){
      x = out$mean.comparisons
      x$entry = sub(paste(para, "\\[", sep=""), "", sapply(x$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
      x$environment =  sub("\\]", "", sapply(x$parameter, function(x){unlist(strsplit(as.character(x), ","))[2]}))
      x$location = sapply(x$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
      x$year = sapply(x$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
      out$mean.comparisons = x
      return(out)
    }
    out = lapply(out, fun, parameter)
    names(out) = vec_env
    return(out)
  }
  
  data_mean_comparisons = MCMC_par(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups)
  
  # 4. Return results
  out <- list(
    "data_mean_comparisons" = data_mean_comparisons
  )
  
  class(out) <- c("PPBstats", "mean_comparisons_model_bh_variance_intra")
  
  return(out)
}
