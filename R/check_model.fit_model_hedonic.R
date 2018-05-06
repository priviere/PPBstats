check_model.fit_model_hedonic = function(x){
  model = x$model
  out = c(list("hedonic" = x), "data_ggplot" = list(check_freq_anova(model)))
  class(out) <- c("PPBstats", "check_model_hedonic")
  return(out) 
}
