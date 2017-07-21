ggplot_mean_vs_stability = function(res.pca){
  
  p = get_biplot(res.pca)
  
  var = filter(p$data, color == "darkgreen")
  xymean = data.frame(x1 = 0, y1 = 0, x2 = mean(var$x), y2 = mean(var$y))
  p = p + geom_point(data = xymean, aes(x = x2, y = y2), shape = 21, size = 5, color = "red", fill = "white", stroke = 2, alpha = 0.7, inherit.aes = FALSE) # add mean location
  p = p + geom_segment(data = xymean, aes(x = x1, y = y1, xend = x2, yend = y2), arrow=arrow(length=unit(0.4,"cm")), color = "red", inherit.aes = FALSE)
  
  p = p + geom_abline(intercept = 0, slope = xymean$y2 / xymean$x2, color = "red") # add line that passes through the biplot origin and the average location
  p = p + geom_abline(intercept = 0, slope = - 1 / (xymean$y2 / xymean$x2), color = "red") # add line that is perpendicular to previous line and passes through 0
  
  ind = filter(p$data, color == "black")
  per_line = data.frame()
  for(i in 1:nrow(ind)) {
    x1 = 0
    x2 = xymean$x2
    y1 = 0
    y2 = xymean$y2
    x3 = ind$x[i]
    y3 = ind$y[i]
    
    obj = get_perpendicular_segment(x1, y1, x2, y2, x3, y3)

    per_line = rbind.data.frame(per_line, obj)
  }
  colnames(per_line) = c("x1", "y1", "x2", "y2")
  
  p_common = p
  
  # Graph for means performance
  per_line$mean_score = round(sqrt(per_line$x2*per_line$x2 + per_line$y2*per_line$y2), 2)
  
  # the arrow point the greatest value (i.e. greater score)
  slope = -1 / (xymean$y2 / xymean$x2)
  
  for(i in 1:nrow(per_line)) {
    x = per_line[i, "x1"]
    y = per_line[i, "y1"]
    y1 = 1000
    x1 = y1 / slope
    y2 = -1000
    x2 = y2 / slope
    x3 = 0
    y3 = 1000

    test = is.inside.sector(x, y, x1, y1, x2, y2, x3, y3)
    if(!test){
      per_line[i, "mean_score"] = per_line[i, "mean_score"] * -1
    }
  }
  
  colnames(ind)[2:3] = c("x1", "y1")
  a = join(ind, per_line, by = "x1")[c("label", "mean_score")]
  a = arrange(a, -mean_score) 
  
  vec_rank = as.character(paste("Ranking of entries: \n", paste(a$label, collapse = " > "), sep = ""))
  
  p = p_common + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = mean_score), data = per_line, linetype = 2, size = 1, inherit.aes = FALSE)
  p = p + scale_colour_gradient(low = "green", high = "red")
  p_mean = p + ggtitle("Mean performance", vec_rank)
  
  # Graph for stability performance
  per_line$stability_score = round((per_line$x1 - per_line$x2)^2 + (per_line$y1 - per_line$y2)^2, 2)

  colnames(ind)[2:3] = c("x1", "y1")
  a = join(ind, per_line, by = "x1")[c("label", "stability_score")]
  a = arrange(a, -stability_score)
  
  vec_rank = as.character(paste("Ranking of entries: \n", paste(a$label, collapse = " > "), sep = ""))
  
  p = p_common + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = stability_score), data = per_line, linetype = 2, size = 1, inherit.aes = FALSE)
  p = p + scale_colour_gradient(low = "green", high = "red")
  p_stability = p + ggtitle("Stability performance", vec_rank)
  
  p = list("mean_performance" = p_mean, "stability_performance" = p_stability)
  return(p)
}
