mean_vs_stability = function(res.pca){
  
  p = get_biplot(res.pca)
  
  var = filter(p$data, color == "darkgreen")
  xymean = data.frame(x1 = 0, y1 = 0, x2 = mean(var$x), y2 = mean(var$y))
  p = p + geom_point(data = xymean, aes(x = x2, y = y2), shape = 21, size = 5, color = "red", fill = "white", stroke = 2, alpha = 0.7, inherit.aes = FALSE) # add mean location
  p = p + geom_segment(data = xymean, aes(x = x1, y = y1, xend = x2, yend = y2), arrow=arrow(length=unit(0.4,"cm")), color = "red", inherit.aes = FALSE)
  
  p = p + geom_abline(intercept = 0, slope = xymean$y2 / xymean$x2, color = "red") # add line that passes through the biplot origin and the average location
  p = p + geom_abline(intercept = 0, slope = - 1 / (xymean$y2 / xymean$x2), color = "red") # add line that is perpendicular to previous line and passes through 0
  
  per_line = data.frame()
  for(i in 1:nrow(var)) {
    x1 = 0
    x2 = xymean$x2
    y1 = 0
    y2 = xymean$y2
    x3 = var$x[i]
    y3 = var$y[i]
    
    # following formulas thanks to jdbertron cf http://stackoverflow.com/questions/10301001/perpendicular-on-a-line-segment-from-a-given-point
    px = x2-x1
    py = y2-y1
    dAB = px*px + py*py
    u = ((x3 - x1) * px + (y3 - y1) * py) / dAB
    x4 = x1 + u * px
    y4 = y1 + u * py
    per_line = rbind.data.frame(per_line, c(x1 = x3, y1 = y3, x2 = x4, y2 = y4))
  }
  colnames(per_line) = c("x1", "y1", "x2", "y2")
  
  per_line$score = sqrt(per_line$x1*per_line$x1 + per_line$y1*per_line$y1)
  # the arrow point the greater value (i.e. greater score)
  slope = xymean$y2 / xymean$x2
  if( slope > 0 ){ per_line = arrange(per_line, -score) } else { per_line = arrange(per_line, score) }
  per_line$rank = c(1:nrow(per_line))

  colnames(var)[2:3] = c("x1", "y1")
  a = join(var, per_line, by = "x1")[c("label", "score")]
  if( slope > 0 ){ a = arrange(a, -score) } else { a = a(per_line, score) }
  
  # To continue
  # env and germ inversés !!!
  print(as.character(paste("Ranking of germplasms:", paste(a$label, collapse = " > "))))
  
  p = p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = rank), data = per_line, linetype = 2, size = 1, inherit.aes = FALSE)
  p = p + scale_colour_gradient(low = "green", high = "red")
  p = p + ggtitle("Mean vs stability")
  
  return(p)
}
