mean_comparisons_model_2 = function(
  out_check_model, 
  p.adj = "none"
  ){
  # 1. Error message
  if( attributes(out_check_model)$PPBstats.object != "check_model_model_2" ) { stop("data must come from check_model and model_2") }
  

  # return results
  out = list(
    
  )
  
  attributes(out)$PPBstats.object = "mean_comparisons_model_2"
  
 return(out)
}