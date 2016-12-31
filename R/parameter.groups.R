# 0. help ----------
#' Get groups of parameters based on multivariate analysis
#'
#' @description
#' \code{get.parameter.groups}
#'
#' @param list_out_check_model A list whose elements are output from \code{analyse.outputs}
#'  
#' @param parameter The parameter on which to get the groups
#' 
#' @param nb.clust Number of cluster to get. -1 to get automatic clustering. See details.
#' 
#' @details
#' The function run a PCA on a matrix containing the value of each parameter for each variable.
#' 
#' Clusters are done with the \code{FactoMineR::HCPC} function.
#' You can tone the number of clusters with the argument \code{nb.clust}, which is the same as in \code{HCPC}
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
  list_out_check_model,
  parameter,
  nb.clust = -1
)
  # let's go !!! ----------
{
  # 1. Error message ----------
  if( length(list_out_check_model) <= 1 ) { stop("list_out_check_model must have at least two elements (i.e. two variables).") }
  if( is.null(names(list_out_check_model)) ){ stop("Each element of list_out_check_model must have a name") }
  if( is.element(TRUE, is.element(names(list_out_check_model), "")) ){ stop("Each element of list_out_check_model must have a name") }
  
  for(i in 1:length(list_out_check_model)) {
    l = list_out_check_model[[i]]
    if( !is.element(attributes(l$PPBstats.object), c("check_model_GxE", "check_model_GxE")) ) {
      stop("All the element of list_out_check_model must come from check_model with model_2 or GxE. This is not the case with the ", i, " element.")
    }
  }
  
  # 2. Get matrix
  if( attributes(list_out_check_model[[1]])$PPBstats.object == "check_model_GxE" ) { parameter_groups_GxE(list_out_check_model, parameter) }

  if( attributes(list_out_check_model[[1]])$PPBstats.object == "check_model_model_2" ) { parameter_groups_model_2(list_out_check_model, parameter) }
  
  
  # 3. Run the PCA ----------
  obj.pca = PCA(obj, scale.unit=TRUE, ncp=2, graph = FALSE) # We keep only two dimension inorder to do the HCPC (ncp=2), we assumed it is noise after
  
  # 4. Get the clusters ----------  
  hcpc = HCPC(obj.pca, nb.clust = nb.clust, consol = 0, min = 2, max = 5, graph = FALSE) # Be careful, if we put 2, it often returns 2! The package propose to put 3
  clust = hcpc$data.clust
  clust$clust = paste("cluster", clust$clust)
  
  # 5. Return the outputs ----------
  OUT = list("obj.pca" = obj.pca, "clust" = clust)
  attributes(OUT)$PPBstats.object = "parameter_groups"
  
  return(OUT)
}
