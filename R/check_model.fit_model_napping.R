check_model.fit_model_napping = function(x){
  out = x
  class(out) <- c("PPBstats", "check_model_napping", "MFA")
  return(out) 
}