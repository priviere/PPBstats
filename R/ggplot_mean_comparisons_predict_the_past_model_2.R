ggplot_mean_comparisons_predict_the_past_model_2 = function(
  out_mean_comparisons_predict_the_past_model_2,
  data_version = NULL,
  ggplot.type = "interaction",
  nb_parameters_per_plot = 10
){
  if( attributes(out_mean_comparisons_predict_the_past_model_2)$PPBstats.object != "mean_comparisons_predict_the_past_model_2" ) { stop("data must come from mean_comparisons_predict_the_past_model_2") }
  
  attributes(out_mean_comparisons_predict_the_past_model_2)$PPBstats.object = "mean_comparisons_model_1"
  
  out = ggplot_mean_comparisons_model_1(out_mean_comparisons_predict_the_past_model_2, data_version, ggplot.type, nb_parameters_per_plot)
  
  return(out)
}
  