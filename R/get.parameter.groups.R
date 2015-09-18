# 0. help ----------
#' Get groups of parameters based on multivariate analysis
#'
#' @description
#' \code{get.parameter.groups}
#'
#' @param analyse.outputs.list A list whose elements are output from \code{analyse.outputs}
#'  
#' @param parameter The parameter on which to get the groups
#' 
#' @param nb.clust Number of cluster to get. -1 to get automatic clustering. See details.
#' 
#' @details
#' The function run a PCA on a matrix containing the value of each parameter for each variable.
#' 
#' Cluster are done with the \code{FactoMineR::HCPC} function.
#' You can tone the number of clusters with the argument \code{nb.clust}, which is the same than in \code{HCPC}
#' Type ?\code{HCPC} for more informations.
#' 
#' More details in the vignette (type vignette ("PPBstats")).
#' 
#' @return 
#' The function returns a data frame with for each parameter, the median for each variable and the cluster to which it belongs
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{FWH}}, \code{\link{analyse.outputs}}, \code{\link{get.ggplot}}
#' 
#' 
get.parameter.groups = function(
  analyse.outputs.list,
  parameter,
  nb.clust = -1
)
  # let's go !!! ----------
{
  # 1. Error message ----------
  if( length(analyse.outputs.list) <= 1 ) { stop("analyse.outputs.list must have at least two elements (i.e. two variables).") }
  if( is.null(names(analyse.outputs.list)) ){ stop("Each element of analyse.outputs.list must have a name") }
  if( is.element(TRUE, is.element(names(analyse.outputs.list), "")) ){ stop("Each element of analyse.outputs.list must have a name") }
  
  for(m in 1:length(analyse.outputs.list)) {
    mcmc = analyse.outputs.list[[m]]$MCMC
    
    if( is.null(attributes(mcmc)$model) ) { stop("The MCMC object should come from model 1 (PPBstats::MC$MCMC) or model 2 (PPBstats::FWH$MCMC) follow by PPBstats::analyse.outputs.") } 
    
    if( length(grep(paste(parameter, "\\[", sep=""), colnames(mcmc))) == 0 ) { stop(parameter," is not in MCMC in analyse.outputs") } 
  }
  
  
  # 2. Create the obj matrix ----------
  MCMC = obj.rownames = NULL
  
  for(m in 1:length(analyse.outputs.list)) {
    mcmc = analyse.outputs.list[[m]]$MCMC
    mcmc = mcmc[,grep(paste(parameter, "\\[", sep=""), colnames(mcmc))]
    obj.rownames = c(obj.rownames, colnames(mcmc))
    MCMC = c(MCMC, list(mcmc))
  }
  
  obj.rownames = unique(obj.rownames)
  obj = matrix(NA, ncol = length(analyse.outputs.list), nrow = length(obj.rownames))
  rownames(obj) = obj.rownames
  colnames(obj) = names(analyse.outputs.list)
  
  # 3. fill the obj matrix  ----------
  for(m in 1:length(analyse.outputs.list)) {
    mcmc = analyse.outputs.list[[m]]$MCMC
    mcmc = mcmc[,grep(paste(parameter, "\\[", sep=""), colnames(mcmc))]
    obj[colnames(mcmc), names(analyse.outputs.list)[m]] = apply(mcmc, 2, median)
  }
  
  rownames(obj) = sapply(rownames(obj), function(x){ sub("\\]", "", sub(paste(parameter, "\\[", sep=""), "", x ) ) } )
  
  # 4. Run the PCA ----------
  obj.pca = PCA(obj, scale.unit=TRUE, ncp=2, graph = FALSE) # We keep only two dimension inorder to do the HCPC (ncp=2), we assumed it is noise after
  
  # 5. Get the clusters ----------  
  hcpc = HCPC(obj.pca, nb.clust = nb.clust, consol = 0, min = 2, max = 5, graph = FALSE) # Be careful, if we put 2, it often returns 2! The package propose to put 3
  clust = hcpc$data.clust
  clust$clust = paste("cluster", clust$clust)
  
  # 6. Return the outputs ----------
  OUT = list("obj.pca" = obj.pca, "clust" = clust)
  attributes(OUT)$PPBstats.object = "parameter.groups.model2"
  
  return(OUT)
}
