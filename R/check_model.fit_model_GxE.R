check_model.fit_model_GxE <- function(
  x
){
  model = x$ANOVA$model
  
  out = c(list("GxE" = x), "data_ggplot" = list(check_freq_anova(model)))
  
  class(out) <- c("PPBstats", "check_model_GxE")
  
  return(out)
}

