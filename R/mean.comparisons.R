# 0. help ----------
#' Get mean comparisons for a given parameter two by two or to a given threshold based on MCMC outputs
#'
#' @description
#' \code{get.mean.comparisons} performs mean comparisons two by two or to a given threshold based on MCMC outputs
#'
#' @param MCMC MCMC outputs from \code{analysis.outputs}.
#'  
#' @param parameter The parameter on which the mean comparison is done 
#' 
#' @param alpha The level of type one error. 0.05 (5\%) by default
#' 
#' @param type The type of comparisons. 1 for comparison two by two. 2 for comparison to a specific threshold.
#' 
#' @param threshold For type = 2. The threshold to which a parameter is different
#' 
#' @param p.adj For type = 1. NULL for no adjustement of the type one error. "soft.bonf" for a soft bonferonni correction to take into account multiple comparisons (alpha / nb of parameters).
#' 
#' @param get.at.least.X.groups For type = 1. If there are only one group with alpha, the minimum number of groups wanted with a higher type one error (i.e. lower confidence). If NULL, nothing is done.
#' 
#' @param precision For type = 1. The precision of the alpha with the correspondong groups from get.at.least.X.groups. The smaller the better, but the smaller the more time consuming due to computing matters
#' 
#' 
#' @details
#' The comparisons is based on the probability of having a common distribution for each pair of parameters. 
#' When there is only one group with the value of alpha, the function (via \code{get.at.least.X.groups argument}) returns at least X groups with a new value of alpha.
#' More details in the vignette (type vignette ("PPBstats")).
#' 
#' @return The function returns a data frame with the following columns: parameter, median, groups, number of groups, type one error and correction used
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{analyse.outputs}}, \code{\link{comp.parameters}}, \code{\link{get.significant.groups}}, \code{\link{get.at.least.X.groups}}, \code{\link{get.ggplot}}
#' 
mean_comparisons = function(
  out_check_model,
  parameter,
  alpha = 0.05,
  type = 1,
  threshold = 1,
  p.adj = "soft.bonf",
  get.at.least.X.groups = 2,
  precision = 0.0005
)
  # let's go !!! ----------
{
  # 1. Error message and update arguments ----------
  mess = "out_check_model must come from check_model."
  if( is.null(attributes(out_check_model)$PPBstats.object) ) { stop(mess) } 
  if( !is.element(attributes(out_check_model)$PPBstats.object, c("check_model_model_1", "check_model_model_2", "check_model_GxE")) ) { stop(mess) } 
  
  if( attributes(out_check_model)$PPBstats.object == "check_model_GxE" ) { 
    out = mean_comparisons_GxE(out_check_model, p.adj) 
  }
  
  if( attributes(out_check_model)$PPBstats.object == "check_model_model_1" ) { 
    out = mean_comparisons_model_1(
      out_check_model, 
      parameter,
      alpha,
      type,
      threshold,
      p.adj,
      get.at.least.X.groups,
      precision
    )
  }
  
  if( attributes(out_check_model)$PPBstats.object == "check_model_model_2" ) { 
    out = mean_comparisons_model_2(
      out_check_model, 
      parameter,
      alpha,
      type,
      threshold,
      p.adj,
      get.at.least.X.groups,
      precision
    )
    }

  if( attributes(out_check_model)$PPBstats.object == "predict_the_past_model_2" ) { 
    out_check_model = out_predict_the_past_model_2
    mean_comparisons_predict_the_past_model_2(
      out_check_model,
      alpha = 0.05,
      type = 1,
      threshold = 1,
      p.adj = "soft.bonf",
      get.at.least.X.groups = 2,
      precision = 0.0005
    )
  }
  
  return(out)
}
