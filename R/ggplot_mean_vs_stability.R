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

  # Graph for means
  per_line$mean_score = round(sqrt(per_line$x1*per_line$x1 + per_line$y1*per_line$y1), 2)
  # the arrow point the greater value (i.e. greater score)
  slope = xymean$y2 / xymean$x2
  if( slope > 0 ){ 
    per_line$mean_score[which(per_line$x1 < 0)] = per_line$mean_score[which(per_line$x1 < 0)] * -1
    per_line = arrange(per_line, -mean_score) 
  } else { 
    per_line$mean_score[which(per_line$x1 > 0)] = per_line$mean_score[which(per_line$x1 < 0)] * -1
    per_line = arrange(per_line, mean_score) 
  }
  
  colnames(ind)[2:3] = c("x1", "y1")
  a = join(ind, per_line, by = "x1")[c("label", "mean_score")]
  if( slope > 0 ){ a = arrange(a, -mean_score) } else { a = a(per_line, mean_score) }
  
  vec_rank = as.character(paste("Ranking of entries: \n", paste(a$label, collapse = " > "), sep = ""))
  
  p = p_common + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = mean_score), data = per_line, linetype = 2, size = 1, inherit.aes = FALSE)
  p = p + scale_colour_gradient(low = "green", high = "red")
  p_mean = p + ggtitle("Mean", vec_rank)
  
  # Graph for stability
  per_line$stability_score = round((per_line$x1 - per_line$x2)^2 + (per_line$y1 - per_line$y2)^2, 2)

  colnames(ind)[2:3] = c("x1", "y1")
  a = join(ind, per_line, by = "x1")[c("label", "stability_score")]
  if( slope > 0 ){ a = arrange(a, -stability_score) } else { a = a(per_line, stability_score) }
  
  vec_rank = as.character(paste("Ranking of entries: \n", paste(a$label, collapse = " > "), sep = ""))
  
  p = p_common + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = stability_score), data = per_line, linetype = 2, size = 1, inherit.aes = FALSE)
  p = p + scale_colour_gradient(low = "green", high = "red")
  p_stability = p + ggtitle("Stability", vec_rank)
  
  p = list("mean" = p_mean, "stability" = p_stability)
  return(p)
}
