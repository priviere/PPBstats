ggplot_discrimitiveness_vs_representativeness = function(res.pca){

  p = get_biplot(res.pca)
  
  var = filter(p$data, color == "darkgreen")
  xymean = data.frame(x1 = 0, y1 = 0, x2 = mean(var$x), y2 = mean(var$y))
  p = p + geom_point(data = xymean, aes(x = x2, y = y2), shape = 21, size = 5, color = "red", fill = "white", stroke = 2, alpha = 0.7, inherit.aes = FALSE) # add mean location
  p = p + geom_segment(data = xymean, aes(x = x1, y = y1, xend = x2, yend = y2), arrow=arrow(length=unit(0.4,"cm")), color = "red", inherit.aes = FALSE)
  
  d = data.frame(x1 = 0, y1 = 0, x2 = var$x, y2 = var$y)
  
  # distance of each location from the plot origin
  d$discrimitiveness = NA
  for(i in 1:nrow(d)){
    x1 = d[i,"x1"]
    y1 = d[i,"y1"]
    x2 = d[i,"x2"]
    y2 = d[i,"y2"]
    px = x2-x1
    py = y2-y1
    d[i,"discrimitiveness"] = px*px + py*py
  }
  
  colnames(var)[2:3] = c("x2", "y2")
  a = join(var, d, by = "x2")[c("label", "discrimitiveness")]
  a = arrange(a, -discrimitiveness)
  vec_disc = as.character(paste("Ranking of discrimitiveness: \n", paste(a$label, collapse = " > "), sep = ""))
  
  p = p + geom_segment(data = d, aes(x = x1, y = y1, xend = x2, yend = y2, color = discrimitiveness), linetype = 2, inherit.aes = FALSE)
  p = p + scale_colour_gradient(low = "green", high = "red")
  p = p + ggtitle("Discrimitiveness", vec_disc)
  
  return(p)
}