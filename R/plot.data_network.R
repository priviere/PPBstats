plot.data_network = function(
  net, 
  in_col = NULL, 
  labels_on = FALSE, 
  labels_size = 4, 
  organize_sl = TRUE
  ){
  
  # functions used afterward
  plot_network_bipart = function(net, labels_on, labels_size){
    n = ggnetwork(net, arrow.gap = 0)
    ntmp = n
    
    vt1 = which(n$type == "germplasm")
    vt2 = which(n$type == "location")
    
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
    
    if( labels_on ){ 
      p = p + geom_nodelabel_repel(aes(label = vertex.names), size = labels_size, 
                                   segment.color = "black") 
    }
    
    return(p)
  }
  plot_barplot_bipart = function(net){
    s = sapply(V(net)$name, function(x) length(E(net)[from(V(net)[x])]))
    s = s[which(vertex.attributes(net)$type == "germplasm")]
    d = data.frame(germplasm = names(s), nb_location = s)
    pg = ggplot(d, aes(x = reorder(germplasm, -nb_location), y = nb_location)) + geom_bar(stat="identity")
    pg = pg + xlab("germplasm") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    s = sapply(V(net)$name, function(x) length(E(net)[to(V(net)[x])]))
    s = s[which(vertex.attributes(net)$type == "location")]
    d = data.frame(germplasm = names(s), nb_location = s)
    pl = ggplot(d, aes(x = reorder(germplasm, -nb_location), y = nb_location)) + geom_bar(stat="identity")
    pl = pl + xlab("germplasm") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    out = list("germplasm" = pg, "location" = pl)
    return(out)
  }
  organize_sl_unipart = function(net){
    n = ggnetwork(net, arrow.gap = 0)
    
    a = n
    a$names = n$vertex.names
    
    lapply(a$names, function(x){
      if(length(unlist(strsplit(as.character(x), "_")))!=4){
        stop("Seed_lots must be under the following format : GERMPLASM_LOCATION_YEAR_DIGIT")
      }
    }
    )
    
    a$g = sapply(a$names, function(x){ unlist(strsplit(as.character(x), "_"))[1] })
    a$p = sapply(a$names, function(x){ unlist(strsplit(as.character(x), "_"))[2] })
    a$ye = sapply(a$names, function(x){ unlist(strsplit(as.character(x), "_"))[3] })
    a$d = sapply(a$names, function(x){ unlist(strsplit(as.character(x), "_"))[4] })
    a$gd = paste(a$g, a$d, sep = "_")
    
    # Create a grid where the seed_lots will be put
    # X are the xaxis of the new grid, it is the year
    # Y are the y axis of the new grid
    
    # Y according to location and germplasm for a given location
    pgd = with(a, table(p, gd))
    vec_p = rownames(pgd)
    
    Y = person = germplasm_digit = person_limit = NULL
    for(per in vec_p){
      d = droplevels(a[which(a$p == per),])
      ygd = with(d, table(ye, gd))
      y = NULL
      for(j in 1:ncol(ygd)){ y = c(y, max(ygd[,j])) }
      y = c(y, 4) # make a gap between each location 
      person_limit = c(person_limit, length(y) ) # and store the information for the plot after (in order to draw horizontal line)
      Y = c(Y, y)
      person = c(person, rep(per, length(y)))
      germplasm_digit = c(germplasm_digit, colnames(ygd), "limit")
    }
    person_limit = cumsum(person_limit)
    Y = cumsum(Y)
    person_limit = Y[person_limit]; names(person_limit) = vec_p
    dY = data.frame(person, germplasm_digit, Y)
    
    # Place seed_lots on the grid
    # new coordinates
    a$x_new = a$y_new = a$xend_new = a$yend_new = NA
    
    for(i in 1:nrow(a)) {
      germ_digit = a[i, "gd"]
      pers = a[i, "p"]
      year = a[i, "ye"]
      
      x = a[i, "x"]
      y = a[i, "y"]
      xend = a[i, "xend"]
      yend = a[i, "yend"]
      
      a[i, "x_new"] = year
      a[i, "y_new"] = dY[which(dY$person == pers & dY$germplasm_digit == germ_digit), "Y"][1]
    }
    
    x = a$x_new
    names(x) = a$x
    x = x[!duplicated(names(x))]
    for(i in 1:length(x)) { a$xend_new[which(a$xend == names(x)[i])] = x[i] }
    
    y = a$y_new
    names(y) = a$y
    y = y[!duplicated(names(y))]
    for(i in 1:length(y)) { a$yend_new[which(a$yend == names(y)[i])] = y[i] }
    
    # update n with new coordinates
    n$x = a$x_new
    n$y = a$y_new
    n$xend = a$xend_new
    n$yend = a$yend_new
    
    out = list("person_limit" = person_limit, "n" = n)
    
    return(out)
  }
  plot_network_unipart = function(n){
    nr = n[which(n$relation_type != "diffusion"),]
    nd = n[which(n$relation_type == "diffusion"),]
    
    p = ggplot(n, aes(x = x, y = y, xend = xend, yend = yend))
    p = p + geom_nodes(aes(color = in_col))
    p = p + geom_edges(data = nr, aes(linetype = relation_type), arrow = arrow(length = unit(4, "pt"), type = "closed"))
    p = p + geom_edges(data = nd, aes(linetype = relation_type), arrow = arrow(length = unit(4, "pt"), type = "closed"), curvature = 0.2)
    p$labels$colour = in_col
    
    scale_ex = c("solid", "dotted", "longdash", "dashed", "twodash", "dotdash")
    p = p + scale_linetype_manual(values = scale_ex[1:length(na.omit(unique(n$relation_type)))] )
    return(p)
  }
  plot_network_organize_sl_unipart = function(n, person_limit){
    p = plot_network_unipart(n)
    r = range(as.numeric(as.character(n$x)))
    m = max(as.numeric(as.character(n$x)))
    m = m + (r[2]-r[1]) /length(r[1]:r[2])
    
    d_lab = data.frame(x = as.character(rep(m, length(person_limit))),
                       y = tapply(n$y, n$location, function(x){mean(range(x))}),
                       location = c(names(person_limit))
    )
    
    p = p + geom_hline(yintercept = c(0, person_limit)) 
    p = p + geom_label(data = d_lab, aes(x = x, y = y, label = location), inherit.aes = FALSE)
    p = p + theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.background = element_blank()
    )
    
    p = p + xlab("year") + scale_x_discrete(labels = c(r[1]:r[2], ""))
    return(p)
  }
  
  
  if( is_bipartite(net) ) { 
    out = list(
      "network" = plot_network_bipart(net, labels_on, labels_size), 
      "barplot" = plot_barplot_bipart(net)
      )
  } else {
    
    if( organize_sl){ 
      out = organize_sl_unipart(net) 
      person_limit = out$person_limit
      n = out$n
    } else { 
        n = ggnetwork(net, arrow.gap = 0.005) 
        }
    
    if( is.null(in_col) ) { in_col = "location" }
    if( organize_sl){ in_col = "germplasm"} 
    colnames(n)[which(colnames(n) == in_col)] = "in_col" 
    
    if( organize_sl){ 
      p = plot_network_organize_sl_unipart(n, person_limit) 
    } else { 
        p = plot_network_unipart(n) + theme_blank() 
        }
    
    if( labels_on ){ 
      p = p + geom_nodelabel_repel(aes(label = vertex.names), size = labels_size, 
                                   segment.color = "black") 
    }
    
    out = list("network" = p)
  }
  
  return(out)
}
