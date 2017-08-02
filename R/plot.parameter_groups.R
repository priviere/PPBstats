plot.parameter_groups <- function(x){

  pca = x$obj.pca
  res.hcpc = x$clust$res.hcpc
  
  # Get one plot per cluster with the rigth legend ----------
  nb_clust = nlevels(res.hcpc$call$X$clust)
  p_all = fviz_cluster(res.hcpc, repel = TRUE)
  list_p_clust = list(p_all)
  for(c in 1:nb_clust){
    d = res.hcpc
    dX = filter(d$call$X, clust == c)
    rownames(dX) = rownames(d$call$X)[which(d$call$X$clust == c)]
    d$call$X = dX
    levels(d$call$X$clust) = c(1:nb_clust)
    p1 = fviz_cluster(d, repel = TRUE)
    xlim = range(p1$data$x)
    ylim = range(p1$data$y)
    levels(d$call$X$clust) = c(1:nb_clust)
    for(cc in 1:nb_clust){ d$call$X = rbind.data.frame(d$call$X, c(NA, NA, cc)) }
    rownames(d$call$X)[(nrow(d$call$X) - nb_clust + 1): nrow(d$call$X)] = paste("cluster", c(1:nb_clust))
    p = fviz_cluster(d, repel = TRUE, ellipse = FALSE) + coord_cartesian(xlim = xlim, ylim = ylim)
    list_p_clust = c(list_p_clust, list(p))
  }
  names(list_p_clust) = paste("cluster", c("all", c(1:nb_clust)), sep = "_")
  
  # Return results ----------
  out = list(
    "pca" = list(
      "composante_variance" = fviz_eig(pca),
      "ind" = fviz_pca_ind(pca, repel = TRUE),
      "var" = fviz_pca_var(pca, repel = TRUE)
    ),
    "clust" = list_p_clust
  )
  
  return(out)
}
