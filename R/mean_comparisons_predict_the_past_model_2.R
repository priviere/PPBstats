mean_comparisons_predict_the_past_model_2 = function(
  out_predict_the_past_model_2,
  alpha = 0.05,
  type = 1,
  threshold = 1,
  p.adj = "soft.bonf",
  get.at.least.X.groups = 2,
  precision = 0.0005
){
  # 1. Error message
  if( attributes(out_check_model_1)$PPBstats.object != "predict_the_past_model_2" ) { stop("data must come from predict_the_past_model_2") }
  
  attributes(out_predict_the_past_model_2)$PPBstats.object == "check_model_model_1"
  out = mean_comparisons_model_1(out_predict_the_past_model_2, parameter = "mu", alpha = 0.05, type = 1, threshold = 1, p.adj = "soft.bonf", get.at.least.X.groups, precision)
  
  attributes(out)$PPBstats.object == "mean_comparisons_predict_the_past_model_2"
return(out)
}

