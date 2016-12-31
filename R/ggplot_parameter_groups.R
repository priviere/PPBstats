ggplot_parameter_groups = function(parameter_groups){

  pca = parameter_groups$obj.pca
  clust = parameter_groups$clust
  
  nb.k = clust$nb.k
  res.hc = clust$res.hc
  km.res = clust$km.res
  
  
  out = list(
    "pca" = list(
      "variation_dim" = fviz_eig(pca),
      "ind" = fviz_pca_ind(pca),
      "var" = fviz_pca_var(pca)
    ),
    "clust" = list(
      "nb_k" = fviz_nbclust(out, kmeans, method = "silhouette"),0
      "dendrogramm" = fviz_dend(res.hc, cex = 0.5, k = nb.k, type = "triangle"),
      "pca" = fviz_cluster(km.res, out, frame.type = "norm")
    )
  )
  
  return(out)
}
