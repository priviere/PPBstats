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
