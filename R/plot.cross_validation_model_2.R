plot.cross_validation_model_bh_GxE <- function(
  x
){
  
  # Get the regression ----------
  p = ggplot(x, aes(x = observed.value, y = estimated.value)) 
  p = p + stat_smooth(method = "lm") + geom_point()
  model = lm(observed.value ~ estimated.value, data = x)
  
  # Get the confidence in the estimation ----------
  bias = x$observed.value - x$estimated.value
  test = t.test(bias, mu = 0)
  proba_mean_equal_zero = round(test$p.value * 100, 1)
  p = p + ggtitle("Cross validation", paste("Probability mean = 0 : ", proba_mean_equal_zero))
  
  out = list("plot" = p, "regression" = model)
  
  return(out)
}
