#' Get groups of parameters based on multivariate analysis
#'
#' @description
#' \code{parameter_groups} gets groups of parameters based on multivariate analysis
#'
#' @param list_out_check_model A list whose elements are output from \code{\link{check_model}}
#'  
#' @param parameter The parameter on which to get the groups
#' 
#' @details
#' The function run a PCA on a matrix containing the value of each parameter for each variable.
#' 
#' Clusters are done based on Average silhouette method for k-means clustering as explained here \url{http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning#r-codes-1}
#' 
#' The hcluster are computed with the function hclust(dist())
#' 
#' The kmeans are computed with the function kmeans()
#' 
#' @return 
#' The function returns a list of two elements:
#' \itemize{
#'  \item obj.pca : the PCA object from FactoMineR::PCA
#'  \item clust, a list of four elements:
#'   \itemize{
#'    \item data : the matrix with variables in column and effect in row, from wich have been computed the PCA
#'    \item nb.k : number of cluster
#'    \item hc : hcluster
#'    \item km : kmeans
#'   }
#' }
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{check_model}},
#'  \item \code{\link{check_model_model_2}},
#'  \item \code{\link{check_model_GxE}},
#'  \item \code{\link{parameter_groups_GxE}},
#'  \item \code{\link{parameter_groups_model_2}},
#'  \item \code{\link{get_ggplot}}
#' }
#'
parameter_groups = function(
  list_out_check_model,
  parameter
)
{
  # 1. Error message ----------
  if( length(list_out_check_model) <= 1 ) { stop("list_out_check_model must have at least two elements (i.e. two variables).") }
  if( is.null(names(list_out_check_model)) ){ stop("Each element of list_out_check_model must have a name") }
  if( is.element(TRUE, is.element(names(list_out_check_model), "")) ){ stop("Each element of list_out_check_model must have a name") }
  
  for(i in 1:length(list_out_check_model)) {
    l = list_out_check_model[[i]]
    if( !is.element(attributes(l)$PPBstats.object, c("check_model_model_2", "check_model_GxE")) ) {
      stop("All the element of list_out_check_model must come from check_model with model_2 or GxE. This is not the case with the ", i, " element.")
    }
  }
  
  # 2. Get matrix
  if( attributes(list_out_check_model[[1]])$PPBstats.object == "check_model_GxE" ) { 
    out = parameter_groups_GxE(list_out_check_model, parameter) 
    }

  if( attributes(list_out_check_model[[1]])$PPBstats.object == "check_model_model_2" ) { 
    out = parameter_groups_model_2(list_out_check_model, parameter) 
    }
  
  
  # 3. Run the PCA ----------
  obj.pca = PCA(out, scale.unit=TRUE, ncp=2, graph = FALSE) # We keep only two dimension inorder to do the HCPC (ncp=2), we assumed it is noise after
  
  # 4. Get the clusters ----------  
  # see method here: http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning
  res.hcpc = HCPC(obj.pca, nb.clust = -1, consol = 0, min = 2, max = 5, graph = FALSE) # Be careful, if we put 2, it often returns 2! The package propose to put 3
  clust =  res.hcpc$data.clust
  clust$clust = paste("cluster", clust$clust)
  
  # 6. Return the outputs ----------
  out = list("obj.pca" = obj.pca, "clust" = list("res.hcpc"= res.hcpc, "clust"=clust))
  attributes(out)$PPBstats.object = "parameter_groups"
  
  return(out)

}
