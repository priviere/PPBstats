GxE_biplot = function(data){
  
  # 1. Error message
  if( attributes(data)$PPBstats.object != "check_model_GxE" ) { stop("data must come from check_model and GxE") }
  
  data_interaction = data$GxE$ANOVA$interaction_matrix
  
  # 2. Ecovalence ----------
  m_eco = data_interaction^2
  
  data_ecovalence = data.frame(
    germplasm = rep(rownames(m_eco), times = ncol(m_eco)), 
    location = rep(colnames(m_eco), each = nrow(m_eco)),
    variable = as.vector(m_eco)
  )
  
  out = list(
    "variable" = variable,
    "data_ecovalence" = data_ecovalence,
    "pca" = data$PCA
  )
  
  attributes(out)$PPBstats.object = "biplot_GxE"
  
  return(out)
}



