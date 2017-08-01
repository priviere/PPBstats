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
#' \item\texttt{obj.pca} : the PCA object from \texttt{FactoMineR::PCA}
#' \item \texttt{clust}, a list of two elements:
#'  \itemize{
#'  \item \texttt{res.hcpc} : the HCPC object from \texttt{FactoMineR::HCPC}
#'  \item \texttt{clust} : the dataframe with cluster assigned to each individual
#'  }
#' }
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{check_model}},
#'  \item \code{\link{parameter_groups_GxE}},
#'  \item \code{\link{parameter_groups_model_2}},
#'  \item \code{\link{plot.PPBstats}}
#' }
#'
parameter_groups = function(
  list_out_check_model,
  parameter
)
{
  valid_models <- c("check_model_2", "check_model_GxE")
  # 1. Error message ----------
  if( length(list_out_check_model) <= 1 ) { stop("list_out_check_model must have at least two elements (i.e. two variables).") }
  if( is.null(names(list_out_check_model)) ){ stop("Each element of list_out_check_model must have a name") }
  if( is.element(TRUE, is.element(names(list_out_check_model), "")) ){ stop("Each element of list_out_check_model must have a name") }
  
  if (!all(
    idx <- vapply(list_out_check_model, inherits, TRUE, valid_models)
  )) {
    ## some (idx) elements of the list are either not a check_model or not
    ## a model_2 nor GxE. Pinpoint all of them.
    mess <- paste(
      "Element(s)",
      paste(names(list_out_check_model)[which(idx)], collapse = ", "),
      "in", substitute(list_out_check_model), "must come from check_model()",
      "from etiher a model_2() of GxE()."
    )
    stop(mess)
  }
  
  ## Check that all elements are consistently from the same model
  ## Matrix where each column (element in the list) contains the idx of the
  ## element's class in the position matching valid_models
  model_classes.idx <- vapply(
    list_out_check_model,
    inherits,
    rep(1,length(valid_models)),
    valid_models,
    which = TRUE)
  rownames(model_classes.idx) <- valid_models
  all_by_model <- apply(model_classes.idx > 0, 1, all)
  if (sum(all_by_model) != 1) {
    ## not all elements are from the same model
    model.idx <- apply(model_classes.idx > 0, 2, function(x) valid_models[which(x)])
    print(model.idx)
    stop("All elements in", substitute(list_out_check_model),
         "need to come from the same model")
  }
  
  

  # 2. Get matrix
  ## function look-up (in the order of valid_models)
  get_matrix <- 
    c(parameter_groups_model_2, parameter_groups_GxE)[[which(all_by_model)]]
  mat <- get_matrix(list_out_check_model, parameter)

  # 3. Run the PCA ----------
  obj.pca = PCA(mat, scale.unit=TRUE, ncp=2, graph = FALSE) # We keep only two dimension inorder to do the HCPC (ncp=2), we assumed it is noise after
  
  # 4. Get the clusters ----------  
  # see method here: http://www.sthda.com/english/wiki/hcpc-hierarchical-clustering-on-principal-components-hybrid-approach-2-2-unsupervised-machine-learning
  res.hcpc = HCPC(obj.pca, nb.clust = -1, consol = 0, min = 2, max = 5, graph = FALSE) # Be careful, if we put 2, it often returns 2! The package propose to put 3
  clust =  res.hcpc$data.clust
  clust$clust = paste("cluster", clust$clust)
  
  # 6. Return the outputs ----------
  out = list("obj.pca" = obj.pca, "clust" = list("res.hcpc"= res.hcpc, "clust"=clust))
  class(out) <- c("PPBstats", "parameter_groups")
  
  return(out)

}
