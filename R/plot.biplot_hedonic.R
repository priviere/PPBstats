#' Get ggplot to visualize output from \code{\link{biplot_data.check_model_hedonic}}
#' 
#' @description
#' \code{plot.biplot_hedonic} returns ggplot to visualize outputs from \code{\link{biplot_data.check_model_hedonic}}
#' 
#' @param x Output from \code{\link{biplot_data.check_model_hedonic}}
#' 
#' @param ... further arguments passed to or from other methods
#' 
#' @details
#' S3 method.
#' The plot are done with the factoextra package
#' 
#' @return 
#' It returns a list with two elements:
#'   \itemize{
#'    \item ca_biplot biplot regarding CA analysis
#'    \item hcpc biplot biplot regarding PCA and HCPC analysis which is a list of two elements
#'    \itemize{
#'     \item variable of the PCA and supplementary variables
#'     \item clusters of juges plot on the PCA
#'    }
#'   }
#'   
#' @author Pierre Riviere
#' 
#' @seealso
#' \itemize{
#'  \item \code{biplot_data}
#' }
#' 
#' @export
#' 
#' @import factoextra
#' 
plot.biplot_hedonic = function(x, ...){
  # see http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_ca.html
  # CA ----------
  out_CA = x$CA
  p = fviz_ca_biplot(out_CA)
  p$data$sample = out_CA$call$Xtot$sample
  ca_biplot = p + geom_point(aes(color = sample))
  
  # PCA ----------
  out_HCPC = x$HCPC
  
  p_var = fviz_pca_var(out_HCPC$res.pca, repel = TRUE)
  p_var = fviz_add(p_var, out_HCPC$res.pca$quali.sup$coord, color = "red")
  
  hcpc_biplot = list(
    "var" = p_var,
    "cluster" = fviz_cluster(out_HCPC$res.hcpc, repel = TRUE) 
  )
  
  out = list("ca_biplot" = ca_biplot, "hcpc_biplot" = hcpc_biplot)
  return(out)
}