#' Get ggplot from parameter_groups
#'
#' @description
#' \code{ggplot_parameter_groups} returns ggplot from \code{\link{parameter_groups}}
#' 
#' @param x outputs from \code{\link{parameter_groups}} function
#' 
#' @details See \code{\link{get_ggplot}}
#' 
#' @return See \code{\link{get_ggplot}}
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{get_ggplot}}, 
#' \item \code{\link{parameter_groups}}
#' }
#' 
plot.parameter_groups <- function(x){

  pca = x$obj.pca
  res.hcpc = x$clust$res.hcpc
  
  out = list(
    "pca" = list(
      "composante_variance" = fviz_eig(pca),
      "ind" = fviz_pca_ind(pca),
      "var" = fviz_pca_var(pca)
    ),
    "clust" = fviz_cluster(res.hcpc)
  )
  
  return(out)
}
