ggplot_parameter_groups = function(out_parameter_groups){

  pca = out_parameter_groups$obj.pca
  
  clust = out_parameter_groups$clust
  data = clust$data
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
      "nb_k" = fviz_nbclust(data, kmeans, method = "silhouette"),
      #"dendrogramm" = fviz_dend(res.hc, cex = 0.5, k = nb.k, type = "triangle"),
      "pca" = fviz_cluster(km.res, data, frame.type = "norm")
    )
  )
  
  return(out)
}
