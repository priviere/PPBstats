parameter_groups_GxE = function(
  list_list_out_check_model_GxE,
  parameter
  ){

  # 1. Prepare data set ----------
  n_G = n_E = n_varG = NULL
  for(i in 1:length(list_out_check_model_GxE)){
    n_G = c(n_G, names(list_out_check_model_GxE[[i]]$GxE$ANOVA$germplasm_effects$effects))
    n_varG = c(n_varG, names(list_out_check_model_GxE[[i]]$GxE$ANOVA$germplasm_effects$intra_variance))
    n_E = c(n_E, names(list_out_check_model_GxE[[i]]$GxE$ANOVA$location_effects$effects))
  }
  n_G = unique(n_G)
  n_E = unique(n_E)
  n_varG = unique(n_varG)
  
  df_G = matrix(NA, ncol = length(list_out_check_model_GxE), nrow = length(n_G))
  colnames(df_G) = names(list_out_check_model_GxE)
  rownames(df_G) = n_G
  
  df_E = matrix(NA, ncol = length(list_out_check_model_GxE), nrow = length(n_E))
  colnames(df_E) = names(list_out_check_model_GxE)
  rownames(df_E) = n_E
  
  df_varG = matrix(NA, ncol = length(list_out_check_model_GxE), nrow = length(n_varG))
  colnames(df_varG) = names(list_out_check_model_GxE)
  rownames(df_varG) = n_varG
  
  for(i in 1:length(list_out_check_model_GxE)){
    g = list_out_check_model_GxE[[i]]$GxE$ANOVA$germplasm_effects$effects
    df_G[names(g),i] = g
    vg = list_out_check_model_GxE[[i]]$GxE$ANOVA$germplasm_effects$intra_variance
    df_varG[names(vg),i] = vg
    e = list_out_check_model_GxE[[i]]$GxE$ANOVA$location_effects$effects
    df_E[names(e),i] = e
  }
  
  if(parameter == "germplasm") { out = df_G }
  if(parameter == "var_intra_germplasm") { out = df_varG }
  if(parameter == "location") { out = df_E }
  
  # 3. Return results
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
