library(ggnetwork)
library(intergraph)

plot.data_network = function(
  net, 
  in_col = NULL, 
  labels_on = FALSE, 
  labels_size = 4, 
  organize_sl = TRUE
  ){
  n = ggnetwork(net, arrow.gap = 0.005)
  
  if( is_bipartite(net) ) { 
    n = ggnetwork(net, arrow.gap = 0)
    ntmp = n
    
    vt1 = which(n$type == "vertex_type_1")
    vt2 = which(n$type == "vertex_type_2")
    # changer vertex type avec location et germplasm : dans fonciotn en amont
    
    y_vt1 = rep(0, length(vt1))
    names(y_vt1) = n$y[vt1]
    y_vt1 = y_vt1[!duplicated(names(y_vt1))]
    
    y_vt2 = rep(1, length(vt2))
    names(y_vt2) = n$y[vt2]
    y_vt2 = y_vt2[!duplicated(names(y_vt2))]
    
    x_vt1 = seq(1, max(length(vt1), length(vt2)), length.out = length(vt1))
    names(x_vt1) = n$x[vt1]
    x_vt1 = x_vt1[!duplicated(names(x_vt1))]
    
    x_vt2 = seq(1, max(length(vt1), length(vt2)), length.out = length(vt2))
    names(x_vt2) = n$x[vt2]
    x_vt2 = x_vt2[!duplicated(names(x_vt2))]
    
    # reformat length knowing names are ok
    x_vt1_ = seq(1, max(length(x_vt1), length(x_vt2)), length.out = length(x_vt1))
    names(x_vt1_) = names(x_vt1)
    x_vt2_ = seq(1, max(length(x_vt1), length(x_vt2)), length.out = length(x_vt2))
    names(x_vt2_) = names(x_vt2)
    
    x_vt1 = x_vt1_
    x_vt2 = x_vt2_
    
    for(i in 1:length(y_vt1)) { ntmp$y[which(n$y == names(y_vt1)[i])] = y_vt1[i] }
    for(i in 1:length(y_vt2)) { ntmp$y[which(n$y == names(y_vt2)[i])] = y_vt2[i] }
    
    for(i in 1:length(x_vt1)) { ntmp$x[which(n$x == names(x_vt1)[i])] = x_vt1[i] }
    for(i in 1:length(x_vt2)) { ntmp$x[which(n$x == names(x_vt2)[i])] = x_vt2[i] }
    
    
    for(i in 1:length(y_vt1)) { ntmp$yend[which(n$yend == names(y_vt1)[i])] = y_vt1[i] }
    for(i in 1:length(y_vt2)) { ntmp$yend[which(n$yend == names(y_vt2)[i])] = y_vt2[i] }
    
    for(i in 1:length(x_vt1)) { ntmp$xend[which(n$xend == names(x_vt1)[i])] = x_vt1[i] }
    for(i in 1:length(x_vt2)) { ntmp$xend[which(n$xend == names(x_vt2)[i])] = x_vt2[i] }
    
    p = ggplot(ntmp, aes(x = x, y = y, xend = xend, yend = yend))
    p = p + geom_nodes(aes(color = type))
    p = p + geom_edges(arrow = arrow(length = unit(0, "pt"), type = "closed"))
    p = p + theme_blank()
    
    
  } else {
    
    if( organize_sl){
      n = ggnetwork(net, arrow.gap = 0)
      
    }
    
    if( is.null(in_col) ) { in_col = "location" }
    colnames(n)[which(colnames(n) == in_col)] = "in_col" 
    
    p = ggplot(n, aes(x = x, y = y, xend = xend, yend = yend))
    p = p + geom_nodes(aes(color = in_col))
    p = p + geom_edges(aes(linetype = relation_type), arrow = arrow(length = unit(4, "pt"), type = "closed"))
    p = p + theme_blank()
    p$labels$colour = in_col
    
    if( labels_on ){ 
      p = p + geom_nodelabel_repel(
        aes(label = vertex.names), 
        size = labels_size,
        segment.color = "black"
      ) 
    }
    
  }
  
  
  return(p)
}
