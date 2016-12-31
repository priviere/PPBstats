ggplot_cross_validation_model_2 = function(
  out_cross_validation_model_2
){
  
  # 5. Get the regression ----------
  p = ggplot(d_out, aes(x = real.value, y = estimated.value)) 
  p = p + stat_smooth(method = "lm") + geom_point() + xlab("observed value")
  model = lm(real.value ~ estimated.value)
  out_r = list("plot" = p, "anova" = model)
  
  # 6. Get the confidence in the estimation ----------
  bias = real.value - estimated.value
  test = t.test(bias, mu = 0)
  percentage.of.confidence = round(test$p.value * 100, 1) # probability than the mean is equal to zero  
  
  return(out)
}
