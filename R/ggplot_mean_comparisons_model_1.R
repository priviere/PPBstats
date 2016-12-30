ggplot_mean_comparisons_model_1 = function(
  mean_comparisons_model_1,
  nb_parameters_per_plot = 10
  ){
  
  # 1. Error message
  if( attributes(mean_comparisons_model_1)$PPBstats.object != "mean_comparisons_model_1" ) { stop("data must come from mean_comparisons and model_1") }
  
  data_Mpvalue = mean_comparisons_model_1$Mpvalue
  data = mean_comparisons_model_1$mean.comparisons
  
  test.mu.m1 = length(grep("mu\\[", data$parameter)) > 0
  test.beta.m1 = length(grep("beta\\[", data$parameter)) > 0  
  
  
  

  # return results
  out = list(
    
  )
  
  return(out)
}

