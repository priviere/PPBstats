ggplot_mean_comparisons_model_2 = function(
  mean_comparisons_model_2,
  nb_parameters_per_plot = 10
){
  
  # 1. Error message
  if( attributes(mean_comparisons_model_1)$PPBstats.object != "mean_comparisons_model_2" ) { stop("data must come from mean_comparisons and model_2") }
  
  if( is.element(ggplot.type, c("score", "interaction")) ) { stop("ggplot.type must be barplot with output from model_2") }
  
  
  data_Mpvalue = mean_comparisons_model_2$Mpvalue
  data = mean_comparisons_model_2$mean.comparisons
  attributes(data)$PPBstats.object = "mean.comparisons.model2"
  
  test.alpha.m2 = length(grep("alpha\\[", data$parameter)) > 0
  test.beta.m2 = length(grep("beta\\[", data$parameter)) > 0
  test.theta.m2 = length(grep("theta\\[", data$parameter)) > 0  
  
  
  # return results
  out = list(
    
  )
  
  return(out)
}

