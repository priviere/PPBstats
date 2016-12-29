parameter_groups_GxE = function(){
  
}

# 2. write function post ammi analysis regrouping all ammi analysis from different variables ----------
fun_post_gxe = function(out_ammi)
  # go! ----------
{
  # 2.1. barplot_variation_repartition ---------- 
  dtmp = data.frame()
  for(i in 1:length(out_ammi)){
    d = out_ammi[[i]]$ANOVA$variability_repartition$data
    d = cbind.data.frame(names(out_ammi)[i], d)
    dtmp = rbind.data.frame(dtmp, d)
  }
  colnames(dtmp)[1] = "var"
  p = ggplot(dtmp, aes(x = var, y = percentage_Sum_sq)) + geom_bar(stat = "identity", aes(fill = factor))
  p = p + ylab("pourcentage de variation") + theme(axis.text.x = element_text(angle = 90))
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  barplot_variation_repartition = p + scale_fill_manual(values = cbbPalette)
  
  # 2.2. PCA on effects -----------
  
  # 2.2.1. Get data sets ----------
  n_G = n_E = n_varG = NULL
  for(i in 1:length(out_ammi)){
    n_G = c(n_G, names(out_ammi[[i]]$ANOVA$germplasm_effects$effects))
    n_varG = c(n_varG, names(out_ammi[[i]]$ANOVA$germplasm_effects$intra_variance))
    n_E = c(n_E, names(out_ammi[[i]]$ANOVA$location_effects$effects))
  }
  n_G = unique(n_G)
  n_E = unique(n_E)
  n_varG = unique(n_varG)
  
  df_G = matrix(NA, ncol = length(out_ammi), nrow = length(n_G))
  colnames(df_G) = names(out_ammi)
  rownames(df_G) = n_G
  
  df_E = matrix(NA, ncol = length(out_ammi), nrow = length(n_E))
  colnames(df_E) = names(out_ammi)
  rownames(df_E) = n_E
  
  df_varG = matrix(NA, ncol = length(out_ammi), nrow = length(n_varG))
  colnames(df_varG) = names(out_ammi)
  rownames(df_varG) = n_varG
  
  for(i in 1:length(out_ammi)){
    g = out_ammi[[i]]$ANOVA$germplasm_effects$effects
    df_G[names(g),i] = g
    vg = out_ammi[[i]]$ANOVA$germplasm_effects$intra_variance
    df_varG[names(vg),i] = vg
    e = out_ammi[[i]]$ANOVA$location_effects$effects
    df_E[names(e),i] = e
  }
  
  # 2.2.2. Run PCA ----------
  PCA_G = PCA(df_G, scale.unit = TRUE, graph = FALSE)
  PCA_intraG = PCA(df_varG, scale.unit = TRUE, graph = FALSE)
  PCA_E = PCA(df_E, scale.unit = TRUE, graph = FALSE)
  
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
    "barplot_variation_repartition" = barplot_variation_repartition,
    "PCA_G_effect" = PCA_G,
    "PCA_intraG_effect" = PCA_intraG,
    "PCA_E_effect" = PCA_E
  )
}


# 3. Apply analysis and Post analysis to vec_variables ----------
message("I. Run ", gxe_analysis, " model on each variable")
out_gxe = lapply(vec_variables, fun_gxe, data, gxe_analysis)
names(out_gxe) = vec_variables

message("\nII. Post ", gxe_analysis," analysis on all outputs")
#out_post_gxe = fun_post_gxe(out_gxe)
out_post_gxe = fun_post_gxe(out_gxe)

OUT = list(out_gxe, out_post_gxe)
names(OUT) = c(gxe_analysis, paste("Post_", gxe_analysis, sep = ""))

return(OUT)
