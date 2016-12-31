ggplot_cross_validation_model_2 = function(
  out_cross_validation_model_2
){
  
  # Get the regression ----------
  p = ggplot(out_cross_validation_model_2, aes(x = real.value, y = estimated.value)) 
  p = p + stat_smooth(method = "lm") + geom_point() + xlab("observed value")
  model = lm(real.value ~ estimated.value, data = out_cross_validation_model_2)
  
  # Get the confidence in the estimation ----------
  bias = out_cross_validation_model_2$real.value - out_cross_validation_model_2$estimated.value
  test = t.test(bias, mu = 0)
  proba_mean_equal_zero = round(test$p.value * 100, 1)
  p = p + ggtitle("Cross validation", paste("Probability mean = 0: ", proba_mean_equal_zero))
  
  out = list("plot" = p, "anova" = model)
  
  return(out)
}
