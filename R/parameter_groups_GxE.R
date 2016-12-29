parameter_groups_GxE = function(check_model_list){

  # 1. Prepare data set ----------
  n_G = n_E = n_varG = NULL
  for(i in 1:length(check_model_list)){
    n_G = c(n_G, names(check_model_list[[i]]$GxE$ANOVA$germplasm_effects$effects))
    n_varG = c(n_varG, names(check_model_list[[i]]$GxE$ANOVA$germplasm_effects$intra_variance))
    n_E = c(n_E, names(check_model_list[[i]]$GxE$ANOVA$location_effects$effects))
  }
  n_G = unique(n_G)
  n_E = unique(n_E)
  n_varG = unique(n_varG)
  
  df_G = matrix(NA, ncol = length(check_model_list), nrow = length(n_G))
  colnames(df_G) = names(check_model_list)
  rownames(df_G) = n_G
  
  df_E = matrix(NA, ncol = length(check_model_list), nrow = length(n_E))
  colnames(df_E) = names(check_model_list)
  rownames(df_E) = n_E
  
  df_varG = matrix(NA, ncol = length(check_model_list), nrow = length(n_varG))
  colnames(df_varG) = names(check_model_list)
  rownames(df_varG) = n_varG
  
  for(i in 1:length(check_model_list)){
    g = check_model_list[[i]]$GxE$ANOVA$germplasm_effects$effects
    df_G[names(g),i] = g
    vg = check_model_list[[i]]$GxE$ANOVA$germplasm_effects$intra_variance
    df_varG[names(vg),i] = vg
    e = check_model_list[[i]]$GxE$ANOVA$location_effects$effects
    df_E[names(e),i] = e
  }
  
  # 2. Run PCA ----------
  PCA_G = PCA(df_G, scale.unit = TRUE, graph = FALSE)
  PCA_intraG = PCA(df_varG, scale.unit = TRUE, graph = FALSE)
  PCA_E = PCA(df_E, scale.unit = TRUE, graph = FALSE)
  
  # 3. Return results
  out = list(
    "PCA_G" = PCA_G,
    "PCA_intraG" = PCA_intraG,
    "PCA_E" = PCA_E
  )
    
  return(out)
}

#   # 2.1. barplot_variation_repartition ---------- 
#   dtmp = data.frame()
#   for(i in 1:length(out_ammi)){
#     d = out_ammi[[i]]$ANOVA$variability_repartition$data
#     d = cbind.data.frame(names(out_ammi)[i], d)
#     dtmp = rbind.data.frame(dtmp, d)
#   }
#   colnames(dtmp)[1] = "var"
#   p = ggplot(dtmp, aes(x = var, y = percentage_Sum_sq)) + geom_bar(stat = "identity", aes(fill = factor))
#   p = p + ylab("pourcentage de variation") + theme(axis.text.x = element_text(angle = 90))
#   cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#   barplot_variation_repartition = p + scale_fill_manual(values = cbbPalette)
