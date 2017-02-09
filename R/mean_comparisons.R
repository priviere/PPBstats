#' Get mean comparisons from object coming from check_model or predict_the_past_model_2
#'
#' @description
#' \code{mean_comparisons} performs mean comparisons from object coming from check_model or predict_the_past_model_2
#'
#' @param out_check_model outputs from \code{check_model} or \code{predict_the_past_model_2}
#'  
#' @param parameter parameter on which the mean comparison is done. Used only for \code{check_model} of model_1 and model_2.
#' The possible values are:
#' \itemize{
#'  \item for \code{check_model} of model_1 : "mu", "beta"
#'  \item for \code{check_model} of model_2 : 'alpha", "beta", "theta"
#' }
#' 
#' @param alpha level of type one error. 0.05 (5\%) by default
#' 
#' @param type type of comparisons. 1 for comparison two by two. 2 for comparison to a specific threshold.
#' Only for \code{check_model} of model_1 and model_2 and \code{predict_the_past_model_2}
#' 
#' @param get.at.least.X.groups For type = 1. If there are only one group with alpha, the minimum number of groups wanted with a higher type one error (i.e. lower confidence). If NULL, nothing is done.
#' 
#' @param precision For type = 1. The precision of the alpha with the correspondong groups from get.at.least.X.groups. The smaller the better, but the smaller the more time consuming due to computing matters
#' 
#' @param threshold For type = 2. The threshold to which a parameter is different
#' 
#' @param p.adj For all excpet type = 2. NULL for no adjustement of the type one error. 
#' For \code{check_model} of model_1, model_2 and predict_the_past_model_2, p.adj can be NULL or "soft.bonf".
#' For \code{check_model} of GxE, p.adj can be NULL, "holm", "hochberg", "bonferroni", "BH", "BY" or "fdr". see p.adjust() p-adj = "none" is t-student.
#' 
#' @details
#' 
#' parameter is set to "mu" by default for code{check_model} of predict_the_past_model_2
#' 
#' p.adj = "soft.bonf" for a soft bonferonni correction to take into account multiple comparisons (alpha / nb of parameters)..
#' It is the default for model_1, model_2 and predict_the_past_model_2
#' 
#' The comparisons is based on the probability of having a common distribution for each pair of parameter.
#' 
#' When there is only one group with the value of alpha, the function (via \code{get.at.least.X.groups argument}) returns at least X groups with a new value of alpha.
#' 
#' @return 
#' \itemize{
#'  \item From check_model and GxE, list of four elements : 
#'   \itemize{
#'    \item info : a list with variable and gxe_analysis
#'    \item data_ggplot_LSDbarplot_germplasm
#'    \item data_ggplot_LSDbarplot_location
#'    \item data_ggplot_LSDbarplot_year
#'   }
#'  
#'  \item From check_model and model_1, list of three elements : 
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
#'  \item From check_model and model_2 : A list of two elements:
#'    \itemize{
#'     \item mean.comparisons: a dataframe with the following columns : parameter, median, groups, number of groups, alpha (type one error), alpha.correction (correction used), entry, environment, location and year.
#'     \item Mpvalue : a square matrix with pvalue computed for each pair of parameter.
#'    }
#'    
#'  \item From predict_the_past_model_2 a list of one element : data_mean_comparisons, composed of a list of one element for the given environment choose, being a list of two elements: 
#'    \itemize{
#'     \item mean.comparisons: a dataframe with the following columns : parameter, median, groups, number of groups, alpha (type one error), alpha.correction (correction used), entry, environment, location and year.
#'     \item Mpvalue : a square matrix with pvalue computed for each pair of parameter.
#'    }
#' }
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{check_model}},
#'  \item \code{\link{check_model_GxE}},
#'  \item \code{\link{check_model_model_1}},
#'  \item \code{\link{check_model_model_2}},
#'  \code{\link{predict_the_past_model_2}},
#'  \code{\link{get_ggplot}}
#' }
#' 
mean_comparisons = function(
  out_check_model,
  parameter,
  alpha = 0.05,
  type = 1,
  get.at.least.X.groups = 2,
  precision = 0.0005,
  threshold = 1,
  p.adj = "soft.bonf"
)
{
  # 1. Error message and update arguments ----------
  mess = "out_check_model must come from check_model or predict_the_past_model_2."
  if( is.null(attributes(out_check_model)$PPBstats.object) ) { stop(mess) } 
  if( !is.element(attributes(out_check_model)$PPBstats.object, c("check_model_model_1", "check_model_model_2", "check_model_GxE", "predict_the_past_model_2")) ) { stop(mess) } 
  
  if( attributes(out_check_model)$PPBstats.object == "check_model_GxE" ) { 
    out = mean_comparisons_GxE(
      out_check_model, 
      alpha, 
      p.adj
      ) 
  }
  
  if( attributes(out_check_model)$PPBstats.object == "check_model_model_1" ) { 
    out = mean_comparisons_model_1(
      out_check_model, 
      parameter,
      alpha,
      type,
      get.at.least.X.groups,
      precision,
      threshold,
      p.adj
    )
  }
  
  if( attributes(out_check_model)$PPBstats.object == "check_model_model_2" ) { 
    out = mean_comparisons_model_2(
      out_check_model, 
      parameter,
      alpha,
      type,
      get.at.least.X.groups,
      precision,
      threshold,
      p.adj
    )
    }

  if( attributes(out_check_model)$PPBstats.object == "predict_the_past_model_2" ) { 
    out = mean_comparisons_predict_the_past_model_2(
      out_predict_the_past_model_2 = out_check_model,
      alpha = alpha,
      type = type,
      get.at.least.X.groups = get.at.least.X.groups,
      precision = precision,
      threshold = threshold,
      p.adj = p.adj
    )
  }
  
  return(out)
}
