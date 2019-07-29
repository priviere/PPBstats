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
#' Clusters are done based on HCPC method with FactoMineR package as explained here \url{http://www.sthda.com/english/wiki/hcpc-hierarchical-clustering-on-principal-components-hybrid-approach-2-2-unsupervised-machine-learning}
#' 
#' @return 
#' The function returns a list of two elements:
#' \itemize{
#' \item obj.pca : the PCA object from FactoMineR::PCA
#' \item clust, a list of two elements:
#'  \itemize{
#'  \item res.hcpc : the HCPC object from FactoMineR::HCPC
#'  \item clust : the dataframe with cluster assigned to each individual
#'  }
#' }
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{check_model}}
#'  \item \code{\link{parameter_groups_model_GxE}}
#'  \item \code{\link{parameter_groups_model_bh_GxE}}
#'  \item \code{\link{parameter_groups_model_anova}}
#'  \item \code{\link{parameter_groups_model_spatial}}
#'  \item \code{\link{plot.parameter_groups}}
#' }
#'
#' @import FactoMineR
#' 
#' @export
#'
parameter_groups = function(
  list_out_check_model,
  parameter
)
{
  # 1. Error message ----------
  all_by_model = check_list_out_check_model(valid_models = c("check_model_bh_GxE", "check_model_GxE", "check_model_anova", "check_model_spatial"), list_out_check_model)

  # 2. Get matrix
  ## function look-up (in the order of valid_models)
  get_matrix <- 
    c(parameter_groups_model_bh_GxE, 
      parameter_groups_model_GxE, 
      parameter_groups_model_anova, 
      parameter_groups_model_spatial)[[which(all_by_model)]]
  mat <- get_matrix(list_out_check_model, parameter)
  
  # 3. Run the PCA ----------
  obj.pca = FactoMineR::PCA(mat, scale.unit=TRUE, ncp=2, graph = FALSE) # We keep only two dimension inorder to do the HCPC (ncp=2), we assumed it is noise after
  
  # 4. Get the clusters ----------  
  # see method here: http://www.sthda.com/english/wiki/hcpc-hierarchical-clustering-on-principal-components-hybrid-approach-2-2-unsupervised-machine-learning
  res.hcpc = FactoMineR::HCPC(obj.pca, nb.clust = -1, consol = 0, min = 2, max = 5, graph = FALSE) # Be careful, if we put 2, it often returns 2! The package propose to put 3
  clust =  res.hcpc$data.clust
  clust$clust = paste("cluster", clust$clust)
  
  # 6. Return the outputs ----------
  out = list("obj.pca" = obj.pca, "clust" = list("res.hcpc"= res.hcpc, "clust"=clust))
  class(out) <- c("PPBstats", "parameter_groups")
  
  return(out)

}
