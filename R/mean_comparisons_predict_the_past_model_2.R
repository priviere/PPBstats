mean_comparisons_predict_the_past_model_2 = function(
  out_predict_the_past_model_2,
  alpha = 0.05,
  type = 1,
  get.at.least.X.groups = 2,
  precision = 0.0005,
  threshold = 1,
  p.adj = "soft.bonf"
){
  # 1. Error message
  if( attributes(out_predict_the_past_model_2)$PPBstats.object != "predict_the_past_model_2" ) { stop("data must come from predict_the_past_model_2") }
  
  attributes(out_predict_the_past_model_2)$PPBstats.object = "check_model_model_1"
  
  out = mean_comparisons_model_1(out_predict_the_past_model_2, parameter = "mu", alpha = alpha, type = type, threshold = threshold, p.adj = p.adj, get.at.least.X.groups = get.at.least.X.groups, precision = precision)
  
  out = out[1]
  attributes(out)$PPBstats.object = "mean_comparisons_predict_the_past_model_2"
return(out)
}

