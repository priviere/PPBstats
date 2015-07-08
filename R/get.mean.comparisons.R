# 0. help ----------
#' Get mean comparisons for a given parameter two by two or to a given threshold based on MCMC outputs
#'
#' @description
#' \code{get.mean.comparisons} performs mean comparisons two by two or to a given threshold based on MCMC outputs
#'
#' @param MCMC MCMC outputs from \code{analysis.outputs}.
#'  
#' @param parameter The parameter on which gets mean comparisons 
#' 
#' @param alpha The level of type one error. 0.05 (5\%) by default
#' 
#' @param type The type of comparisons. 1 for comparison two by two. 2 for comparison to a specific threshold.
#' 
#' @param threshold For type = 2. The threshold to which a parameter is different
#' 
#' @param p.adj For type = 1. NULL for no adjustement of the type one error. "soft.bonf" for a soft bonferonni correction to take into account multiple comparisons (alpha / nb of parameters).
#' 
#' @param get.at.least.X.groups For type = 1. If there are only one group with alpha, the minimum number of groups wanted with a higher type one error (i.e. lower confidence)
#' 
#' @param precision For type = 1. The precision of the alpha with the correspondong groups from get.at.least.X.groups. The smaller the better, but the smaller the more time consuming due to computing matters
#' 
#' 
#' @details
#' The comparisons is based on the probability to have a common distribution for each pair of parameters. 
#' When there is only one group with the value of alpha, the function (via \code{get.at.least.X.groups argument}) returns at least X groups with a new value of alpha.
#' More details in the vignette (type vignette ("PPBstats")).
#' 
#' @return The function returns a data frame with the following columns: parameter, median, groups, number of groups, type one error and correction used
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{analyse.outputs}}, \code{\link{comp.parameters}}, \code{\link{get.significant.groups}}, \code{\link{get.at.least.X.groups}}, \code{\link{get.ggplot}}
#' 
#'  
get.mean.comparisons = function(
  MCMC,
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
  if( is.null(attributes(MCMC)$model) ) { stop("The MCMC object should come from model 1 (PPBstats::MC$MCMC) or model 2 (PPBstats::FWH$MCMC) follow by PPBstats::analyse.outputs.") } 

  if(attributes(MCMC)$model == "model1" & !is.element(parameter, c("mu", "beta"))) { stop("With outputs from model 1, the parameters must be mu or beta.") }

  if(attributes(MCMC)$model == "model2" & !is.element(parameter, c("alpha", "beta", "theta"))) { stop("With outputs from model 2, the parameters must be alpha, beta or theta.") }
  
  # 2. Get square matrice with pvalue or vector with pvalue ----------
  
  if(attributes(MCMC)$model == "model1" & parameter == "mu") { 
    a = colnames(MCMC)[grep(parameter, colnames(MCMC))]
    vec_env = sub("\\]", "", unique(sapply(a, function(x){unlist(strsplit(x, ","))[2]})))
    vec_element = vec_env
  }

  if(attributes(MCMC)$model == "model1" & parameter == "beta") { 
    a = colnames(MCMC)[grep(parameter, colnames(MCMC))]
    vec_env = sub("beta\\[", "", unique(sapply(a, function(x){unlist(strsplit(x, ","))[1]})))
    vec_element = vec_env
  }
  
  if(attributes(MCMC)$model == "model2" & parameter == "alpha") { vec_element = "alpha\\[" }
  if(attributes(MCMC)$model == "model2" & parameter == "beta") { vec_element = "beta\\[" }
  if(attributes(MCMC)$model == "model2" & parameter == "theta") { vec_element = "theta\\[" }
  
  OUT = NULL
  for (e in 1:length(vec_element)) {
    
    element = vec_element[e]
    
    a = colnames(MCMC)[grep(parameter, colnames(MCMC))]
    toget = a[grepl(element, a)]
    MCMC_element = MCMC[, toget]
    
    Mpvalue = comp.parameters(MCMC = MCMC_element, parameter = parameter, type = type, threshold = threshold)
        
    if(type == 1) {
      Comparison = get.significant.groups(Mpvalue = Mpvalue, MCMC = MCMC_element, alpha = alpha, p.adj = p.adj)
      
      # number of groups
      a = unlist(strsplit(paste(Comparison[, "groups"], collapse = ""), ""))
      nb_group = length(unique(a))
      
      # get at least X groups
      if(nb_group == 1) {
        message(paste("Get at least X groups for ", sub("\\\\\\[", "", element),". It may take some time ...", sep = "")) # The sub is useful for model2
        ALPHA = get.at.least.X.groups(Mpvalue, MCMC_element, p.adj = p.adj, precision = precision)  
        alp = ALPHA[paste(get.at.least.X.groups, "_groups", sep = "")]  
        if(is.numeric(alp)){ alp = round(alp, 3) }
        message(paste("Get at least X groups for ", sub("\\\\\\[", "", element),"is done."))
      } else { alp = alpha }
            
      TAB = cbind.data.frame("parameter" = Comparison$parameter,
                             "median" = Comparison$median, 
                             "groups" = Comparison$groups, 
                             "nb_group" = rep(nb_group, nrow(Comparison)), 
                             "alpha" = rep(alp, nrow(Comparison)),
                             "alpha.correction" = rep(p.adj, nrow(Comparison))
                             )

    } 
    
    if(type == 2) { TAB = cbind.data.frame("proba" = Mpvalue) }
    
    OUT = rbind.data.frame(OUT, TAB)
    
  }
  if( attributes(MCMC)$model == "model1") { attributes(OUT)$PPBstats.object = "mean.comparisons.model1" }
  if( attributes(MCMC)$model == "model2") { attributes(OUT)$PPBstats.object = "mean.comparisons.model2" }

  return(OUT)
}
