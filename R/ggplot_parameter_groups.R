ggplot_parameter_groups = function(parameter_groups_GxE){

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
