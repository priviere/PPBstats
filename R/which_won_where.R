which_won_where = function(res.pca, p){
  
  chull_obj = as.data.frame(res.pca$ind$coord)
  chull_obj = chull_obj[chull(x = chull_obj$Dim.1, y = chull_obj$Dim.2),]
  chull_obj$x2 = c(chull_obj$Dim.1[nrow(chull_obj)], chull_obj$Dim.1[1:(nrow(chull_obj)-1)])
  chull_obj$y2 = c(chull_obj$Dim.2[nrow(chull_obj)], chull_obj$Dim.2[1:(nrow(chull_obj)-1)])
  p = p +  geom_segment(aes(x = Dim.1, y = Dim.2, xend = x2, yend = y2), data = chull_obj)
  
  per_line = data.frame()
  for(i in 1:nrow(chull_obj)) {
    x1 = chull_obj$Dim.1[i]
    x2 = chull_obj$x2[i]
    y1 = chull_obj$Dim.2[i]
    y2 = chull_obj$y2[i]
    
    # following formulas thanks to jdbertron cf http://stackoverflow.com/questions/10301001/perpendicular-on-a-line-segment-from-a-given-point
    px = x2-x1
    py = y2-y1
    dAB = px*px + py*py
    x3 = y3 = 0
    u = ((x3 - x1) * px + (y3 - y1) * py) / dAB
    x4 = x1 + u * px
    y4 = y1 + u * py
    
    # Intersection of segments in order to fit the polygon from chull()
    # https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
    # A creuser 
    #test = sp::point.in.polygon(x4, y4, chull_obj$Dim.1, chull_obj$Dim.2)
    #print(test)
    #if( i > 1 & test == 1){
    #  x1 = chull_obj$Dim.1[i-1]
    #  x2 = chull_obj$x2[i-1]
    #  y1 = chull_obj$Dim.2[i-1]
    #  y2 = chull_obj$y2[i-1]
    #  x3 = y3 = 0
    #  x4 = ((x1*y2 - y1*x2)*(x3-x4) - (x1-x2)*(x3*y4 - y3*x4)) / ((x1-x2)*(y3-y4) - (y1-y2)*(x3-x4))
    #  y4 = ((x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x3*y4 - y3*x4)) / ((x1-x2)*(y3-y4) - (y1-y2)*(x3-x4))
    #}
    
    per_line = rbind.data.frame(per_line, c(x1 = 0, y1 = 0, x2 = x4, y2 = y4))
  }
  colnames(per_line) = c("x1", "y1", "x2", "y2")
  
  p = p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color = "red", data = per_line)
  
  # Get entries for each sector that has the largest values (the winner) among all entries
  
  # ind ad var per sector
  per_line$x3 = c(per_line$x2[nrow(per_line)], per_line$x2[1:(nrow(per_line)-1)])
  per_line$y3 = c(per_line$y2[nrow(per_line)], per_line$y2[1:(nrow(per_line)-1)])
  
  var = res.pca$var$coord
  var$sector = NA
  
  
  ind = as.data.frame(res.pca$ind$coord)
  ind$sector = NA
  
  for(j in 1:nrow(ind)){
    x = ind[j, "Dim.1"]
    y = ind[j, "Dim.2"]
    
    for(i in 1:nrow(per_line)){
      x1 = per_line$x1[i]
      y1 = per_line$y1[i]
      x2 = per_line$x2[i]
      y2 = per_line$y2[i]
      x3 = per_line$x3[i]
      y3 = per_line$y3[i]
      
      # to be sure to get all points, make the segment longer
      y2 = y2/x2 * x2*2
      x2 = x2*2
      y3 = y3/x3 * x3*2
      x3 = x3*2
      
      is.inside.sector = function(x, y, x1, y1, x2, y2, x3, y3){
        # resolve it with barycentric coordinates
        # thanks to andreasdr, cf http://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
        
        p0y = y1
        p0x = x1
        p1y = y2
        p1x = x2
        p2y = y3
        p2x = x3
        py = y
        px = x
        
        Area = 0.5 *(-p1y*p2x + p0y*(-p1x + p2x) + p0x*(p1y - p2y) + p1x*p2y);
        s = 1/(2*Area)*(p0y*p2x - p0x*p2y + (p2y - p0y)*px + (p0x - p2x)*py)
        t = 1/(2*Area)*(p0x*p1y - p0y*p1x + (p0y - p1y)*px + (p1x - p0x)*py);
        
        test = s > 0 & t > 0 & (1-s-t) > 0
        return(test)
      }
      
      if( is.inside.sector(x, y, x1, y1, x2, y2, x3, y3) ) { ind[j, "sector"] = i }
    }
    
  }
  
  # entry with the highest value in each sector, i.e. biggest segment from 0, i.e. biggest hypothenus
  ind$id = c(1:nrow(ind))
  ind$hypo = sqrt(abs(ind$Dim.1)^2 + abs(ind$Dim.2)^2)
  winner = data.frame()
  vec_s = sort(unique(as.character(ind$sector)))
  
  for(s in vec_s){
    sec = droplevels(filter(ind, sector == s))
    id_win = sec[which(sec$hypo == max(sec$hypo)), "id"]
    xwin = ind[id_win, "Dim.1"]
    ywin = ind[id_win, "Dim.2"]
    info = as.numeric(as.character(c(xwin, ywin, s)))
    winner = rbind.data.frame(winner, info)
  }
  colnames(winner) = c("x", "y", "sector")
  winner$sector = as.factor(winner$sector)

  p = p + geom_point(data = winner, aes(x = x, y = y, color = sector))
  p = p + ggtitle("which won where")
    
  return(p)
}

