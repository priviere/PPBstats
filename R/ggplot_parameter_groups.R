ggplot_parameter_groups = function(parameter_groups){

  # 2.4. PCA ----------
  if(ggplot.type == "PCA"){  
    
    obj = data$obj.pca
    clust = data$clust$clust; names(clust) = rownames(data$clust)
    
    # Based on the code of Ben Marwick
    # https://gist.github.com/benmarwick/2139672
    
    # Individuals
    PC1 <- obj$ind$coord[,1]
    PC2 <- obj$ind$coord[,2]
    PCs <- cbind.data.frame(PC1,PC2)
    PCs$labs = rownames(obj$ind$coord)
    PCs$cluster = clust[PCs$labs]
    
    pind = ggplot(data = PCs, aes(PC1, PC2, label = labs, colour = cluster)) + geom_text()
    pind = pind + xlab(paste("PC1 (", round(obj$eig[1,2], 2), "%)", sep="")) + ylab(paste("PC2 (", round(obj$eig[2,2], 2), "%)", sep="") )
    
    # Variables
    vPC1 <- obj$var$coord[,1]
    vPC2 <- obj$var$coord[,2]
    vlabs <- rownames(obj$var$coord)
    vPCs <- data.frame(cbind(vPC1,vPC2))
    vPCs$labs = vlabs
    
    # put a faint circle there, as is customary
    angle <- seq(-pi, pi, length = 50)
    df <- data.frame(x = sin(angle), y = cos(angle))
    pvar <- ggplot() + geom_path(aes(x, y), data = df, colour="grey70")
    
    # add on arrows and variable labels
    pvar <- pvar + geom_text(data=vPCs, aes(x=vPC1,y=vPC2,label=labs), size=4) + xlab(paste("PC1 (", round(obj$eig[1,2], 2), "%)", sep="")) + ylab(paste("PC2 (", round(obj$eig[2,2], 2), "%)", sep="") )
    pvar <- pvar + geom_segment(data=vPCs, aes(x = 0, y = 0, xend = vPC1*0.9, yend = vPC2*0.9), arrow = arrow(length = unit(0.1, "cm")), color = "grey30")
    
    OUT = list("ind" = pind, "var" = pvar)
    return(OUT)
  }
  
  PCA_G = parameter_groups_GxE$PCA_G
  PCA_intraG = parameter_groups_GxE$PCA_intraG
  PCA_E = parameter_groups_GxE$PCA_E
  
  PCA_G = list(
    "variation_dim" = fviz_eig(PCA_G),
    "ind" = fviz_pca_ind(PCA_G),
    "var" = fviz_pca_var(PCA_G)
  )
  
  PCA_intraG = list(
    "variation_dim" = fviz_eig(PCA_intraG),
    "ind" = fviz_pca_ind(PCA_intraG),
    "var" = fviz_pca_var(PCA_intraG)
  )
  
  PCA_E = list(
    "variation_dim" = fviz_eig(PCA_E),
    "ind" = fviz_pca_ind(PCA_E),
    "var" = fviz_pca_var(PCA_E)
  )
  
  out = list(
    "PCA_G_effect" = PCA_G,
    "PCA_intraG_effect" = PCA_intraG,
    "PCA_E_effect" = PCA_E
  )
  
  return(out)
  
}
