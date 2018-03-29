#' Plot network object from format_data_PPBstats()
#' 
#' @description
#' \code{plot.data_network} returns ggplot to visualize outputs from format_data_PPBstats()
#' 
#' @param net output from format_data_PPBstats with data_type = "network"
#' 
#' @param data_to_pie output from format_data_PPBstats with data_type = "agro"
#' 
#' @param variable when data_to_pie is not NULL, variable to plot in a pie
#'
#' @param pie_size when data_to_pie is not NULL, size of the pie 
#'   
#' @param plot_type "network", "barplot" or "map
#' 
#' @param in_col factor in color that fill the barplot or the vertex of the network.
#' It can be germplasm, location or year
#' 
#' @param labels_on for plot_type = "network", labels to display on network for each vertex or locaiton for map
#' 
#' @param labels_size for plot_type = "network", size of the labels
#' 
#' @param organize_sl for plot_type = "network", if TRUE, organize the network for unipart seed lots format with year 
#' in the chronological order on the x axis, location separated on the y axis
#' 
#' @param x_axis for plot_type = "barplot" and unipart seed lots network, factor on the x axis
#' It can be germplasm, location or year
#' 
#' @param nb_parameters_per_plot_x_axis for plot_type = "barplot" and unipart seed lots network, number of parameter on the x_axis
#' 
#' @param nb_parameters_per_plot_in_col for plot_type = "barplot" and unipart seed lots network, number of paramter by color that fill
#' 
#' @details
#' For network diffusion are represented by a curve.
#' For organize_sl, The representation is possible if the seed_lots are under the following format : 
#' GERMPLASM_LOCATION_YEAR_DIGIT.
#' 
#' The id column from data_to_pie must refer to the id of the network.
#' 
#' @return 
#' A list with ggplot object.
#' For plot_type = "network", a list with as many elements as net with the network representation in ggplot format
#' For plot_type = "barplot" and bipart network, it represents the number of edges per vertex for each germplasm 
#' and each location.
#' 
#' @author Pierre Riviere
#' 
#' @seealso
#' \code{format_data_PPBstats}
#' 
plot.data_network = function(
  net,
  data_to_pie = NULL,
  variable = NULL,
  pie_size = 0.2,
  plot_type = "network",
  in_col = "location",
  labels_on = FALSE,
  labels_size = 4,
  organize_sl = FALSE,
  x_axis = NULL,
  nb_parameters_per_plot_x_axis = 5,
  nb_parameters_per_plot_in_col = 5
  ){
  
  # check arguments
  match.arg(plot_type,  c("network", "barplot", "map"), several.ok = FALSE)
  match.arg(in_col,  c("germplasm", "location", "year"), several.ok = FALSE)
  match.arg(x_axis,  c("germplasm", "location", "year"), several.ok = FALSE)
  
  # error and warning messages
  if( !is.list(net[1]) ){ 
    stop("net must be a list. I.e. with net coming from format_data_PPBstats, use net[1] is ok and not net$`blabla`") 
    }
  
  if( !is.null(data_to_pie) ){
    if( !is.element(plot_type, c("network", "map")) ) { stop("data_to_pie can be used only with plot_type = network or map") }
    if( is.null(variable) ) { stop("with data_to_pie, variable must not be NULL") }
    if( !is.element(variable, colnames(data_to_pie) ) ) { stop(variable, " is not present in data_to_pie") }
  } else {
    if( !is.null(variable) ) { stop(variable, " can be used only is data_to_pie is not NULL") }
  }
  
  format = ggnetwork(net[[1]])[1, "format"]
  
  if( format == "bipart" & plot_type == "network" & !is.null(data_to_pie) ) { 
    stop("With bipart network, pies on network are not possible.") 
  }
  if( format == "unipart_location" & plot_type == "network" & !is.null(data_to_pie) ) { 
    stop("With unipart network on location, pies on network are not possible.") 
  }
  
  if( format == "unipart_location" & organize_sl ) { 
    stop("With unipart network on location, organize_sl can not be used.") 
  }
  if( format == "bipart" & organize_sl ) { 
    stop("With bipart network, organize_sl can not be used.") 
  }
  
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
    p = p + geom_edges()
    p = p + theme_blank()
    
    if( labels_on ){ 
      p = p + geom_nodelabel_repel(aes(label = vertex.names), size = labels_size) 
    }
    
    return(p)
  }
  
  plot_barplot_bipart = function(net){
    s = sapply(V(net)$name, function(x) length(E(net)[from(V(net)[x])]))
    s = s[which(vertex.attributes(net)$type == "germplasm")]
    d = data.frame(germplasm = names(s), nb_location = s)
    pg = ggplot(d, aes(x = reorder(germplasm, -nb_location), y = nb_location)) + geom_bar(stat="identity")
    pg = pg + xlab("germplasm") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    pg = pg + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    s = sapply(V(net)$name, function(x) length(E(net)[to(V(net)[x])]))
    s = s[which(vertex.attributes(net)$type == "location")]
    d = data.frame(location = names(s), nb_germplasm = s)
    pl = ggplot(d, aes(x = reorder(location, -nb_germplasm), y = nb_germplasm)) + geom_bar(stat="identity")
    pl = pl + xlab("location") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    pl = pl + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    out = list("germplasm" = pg, "location" = pl)
    return(out)
  }
  
  organize_sl_unipart = function(net){
    n = ggnetwork(net, arrow.gap = 0)
    
    a = n
    a$names = n$vertex.names
    
    lapply(a$names, function(x){
      if(length(unlist(strsplit(as.character(x), "_")))!=4){
        stop("Id of vertex must be seed_lots under the following format : GERMPLASM_LOCATION_YEAR_DIGIT")
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
  
  plot_network_unipart = function(n, in_col){
    
    colnames(n)[which(colnames(n) == in_col)] = "in_col"
    nr = n[which(n$relation_type != "diffusion"),]
    nd = n[which(n$relation_type == "diffusion"),]
    
    p = ggplot(n, aes(x = x, y = y, xend = xend, yend = yend))
    if(is.element("nb_diff", colnames(n))){
      p = p + geom_nodes()
      p = p + geom_edges(data = nd, aes(linetype = relation_type, colour = nb_diff), 
                         arrow = arrow(length = unit(4, "pt"), type = "closed"), 
                         curvature = 0.2)
      p = p + scale_colour_gradient(low = "blue", high = "red")
      p = p + coord_cartesian(xlim = range(c(n$x, n$xend)*1.1), ylim = range(c(n$y, n$yend)*1.1))
    } else { 
      p = p + geom_nodes(aes(color = in_col)) 
      p = p + geom_edges(data = nr, aes(linetype = relation_type), 
                         arrow = arrow(length = unit(4, "pt"), type = "closed"))
      p = p + geom_edges(data = nd, aes(linetype = relation_type), 
                         arrow = arrow(length = unit(4, "pt"), type = "closed"), curvature = 0.2)
    }    
    p$labels$colour = in_col
    
    scale_ex = c("solid", "dotted", "longdash", "dashed", "twodash", "dotdash")
    p = p + scale_linetype_manual(values = scale_ex[1:length(na.omit(unique(n$relation_type)))] )
    return(p)
  }
  
  plot_network_organize_sl_unipart = function(n, person_limit, in_col){
    colnames(n)[which(colnames(n) == in_col)] = "in_col"
    p = plot_network_unipart(n, in_col)
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
  
  plot_barplot_unipart = function(net, x_axis, in_col, 
                                  nb_parameters_per_plot_x_axis, nb_parameters_per_plot_in_col){
    n = ggnetwork(net, arrow.gap = 0)
    n$count = 1
    dall = reshape_data_split_x_axis_in_col(n, 
                                            vec_variables = "count", 
                                            labels_on = NULL,
                                            x_axis = x_axis, 
                                            nb_parameters_per_plot_x_axis = nb_parameters_per_plot_x_axis, 
                                            in_col = in_col, 
                                            nb_parameters_per_plot_in_col = nb_parameters_per_plot_in_col
                                            )
    
    fun_bar = function(d, in_col){
      if(!is.null(in_col)) {	
        p = ggplot(d, aes(x = x_axis, fill = in_col)) + geom_bar()
        p = p + xlab(x_axis) + ylab("") + theme(legend.title=element_blank())
        p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      } else {
        p = ggplot(d, aes(x = x_axis)) + geom_bar() + xlab(x_axis) + ylab("")
        p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    }
    
    out = lapply(dall, fun_bar, in_col)

    return(out)
  }
  
  pmap = function(net, format, labels_on, labels_size){
    # As it is not possible to use annotation_custom with polar coordinates (i.e. output from ggmap) in order to add pies on map,
    # I decided to transfer ggmap output to a png that is inserted in a background of a plot with cartesian coordinates
    # Note there is a change in the look of the map because of coordinates change ...
    n = ggnetwork(net, arrow.gap = 0)
    
    if( format == "bipart" ) {
      n = n[which(n$type == "location"), c("lat", "long", "vertex.names")]
      colnames(n)[ncol(n)] = "location"
    } 
    if( format == "unipart_location" ){
      n = n[c("lat", "long", "vertex.names")]
      colnames(n)[ncol(n)] = "location"
    }
    
    n = unique(n[, c("lat", "long", "location")]) 
    n$lat = as.numeric(as.character(n$lat))
    n$long = as.numeric(as.character(n$long))
    n = na.omit(n)
    center_location = c(mean(n$long), mean(n$lat))
    map = get_map(location = center_location, source = "google", zoom = 6)
    m = ggmap(map, extent = "device")
    ggsave("tmp_map.png", m, width = 1, height = 1) # get a perfect square
    p = ggplot(mtcars, aes(wt, mpg)) + geom_point(size = -10) # support for the map background
    p = p + coord_cartesian(xlim = range(m$data$lon), ylim = range(m$data$lat), expand = FALSE)
    img = readPNG("tmp_map.png")
    pmap = p + annotation_custom(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc")), 
                                 -Inf, Inf, -Inf, Inf) # change in the look of the map because of coordinates changes
    pmap = pmap + xlab("long") + ylab("lat")
    file.remove("tmp_map.png")
    if( labels_on ){ 
      pmap = pmap + geom_nodelabel_repel(data = n, aes(x = long, y = lat, label = location), size = labels_size, inherit.aes = FALSE) 
    }
    return(pmap)
  }
  
  add_pies = function(p, n, format, ptype, data_to_pie, variable, pie_size){
    # script adapted from 
    # Pies On A Map, Demonstration script, By QDR : 
    #   https://qdrsite.wordpress.com/2016/06/26/pies-on-a-map/
    # Guangchuang YU code :
    #   https://cran.r-project.org/web/packages/ggimage/vignettes/ggimage.html#geom_subview
    #   https://github.com/GuangchuangYu/ggimage/blob/master/R/geom_subview.R
    
    # p : network or map
    # n : network object from igraph
    # other arg : cf Rd on the top
    
    # add a invisible point with variable value to get the legend of pies + set the legend
    colnames(data_to_pie)[which(colnames(data_to_pie) == variable)] = "variable"
    
    col_low = "red" # "#132B43"
    col_high = "green" # "#56B1F7"
    
    if( is.numeric(data_to_pie$variable) ) {  
      p = p + geom_point(data = data_to_pie, x = 0, y = 0, size = -10, aes(fill = variable), inherit.aes = FALSE)
      p = p + scale_fill_continuous(low = col_low, high = col_high)
      scale_ok = scales::seq_gradient_pal(low = col_low, high = col_high)(seq(0, 1, length.out = nrow(data_to_pie)))
      s = seq(min(data_to_pie$variable, na.rm = TRUE), max(data_to_pie$variable, na.rm = TRUE), length.out = nrow(data_to_pie))
      data_to_pie$scale_col = sapply(data_to_pie$variable, function(x){scale_ok[which(s >= x)[1]]})
    }
    
    if( is.factor(data_to_pie$variable) ) {  
      p = p + geom_point(data = data_to_pie, x = -10, y = 10, aes(shape = variable, fill = variable), inherit.aes = FALSE) 
      p = p + scale_shape_manual(values = rep(22, nlevels(data_to_pie$variable)))
      scale_ok = scales::seq_gradient_pal(low = col_low, high = col_high)(seq(0, 1, length.out = nlevels(data_to_pie$variable)))
      p = p + scale_fill_manual(values = scale_ok)
      s = seq(1, nlevels(data_to_pie$variable))
      data_to_pie$scale_col = sapply(as.numeric(data_to_pie$variable), function(x){scale_ok[which(s >= x)[1]]})
    }
    
    
    # Set colnames for next step according to plot type and get range for x and y
    if( ptype == "map" ) { 
      colnames(data_to_pie)[which(colnames(data_to_pie) == "location")] = "id_ok" 
      xmin = min(p$coordinates$limits$x); xmax = max(p$coordinates$limits$x)
      ymin = min(p$coordinates$limits$y); ymax = max(p$coordinates$limits$y)
    }
    
    if( ptype == "network" ){ 
        colnames(data_to_pie)[which(colnames(data_to_pie) == "id")] = "id_ok" 
        xmin = min(p$data$x); xmax = max(p$data$x)
        ymin = min(p$data$y); ymax = max(p$data$y)
    }
    
    # Create a list of ggplot objects. Each one is the pie chart for each site with all labels removed.
    pies <- dlply(data_to_pie, .(id_ok), function(z){
      z = arrange(z, variable)
      s_col = z$scale_col; names(s_col) = z$variable
      s_col = s_col[unique(names(s_col))]
      ggplot(z, aes(x = factor(1), fill = factor(variable))) +
        geom_bar(width = 1) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = s_col) +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank()) 
    }
    )
    
    # Get coordinates of each pie and select pies
    d = ggnetwork(n, arrow.gap = 0)
    v_id = c(unique(as.character(data_to_pie$id_ok)))
    
    if( plot_type == "map" & format == "bipart" ){
      d = droplevels(d[which(d$type == "location"),])
      v_ok = v_id[which(is.element(v_id, as.character(d$vertex.names) ))]
      v_not_ok = v_id[which(!is.element(v_id, as.character(d$vertex.names) ))]
    } 
    if( plot_type == "map" & format == "unipart_location" ){
      v_ok = v_id[which(is.element(v_id, as.character(d$vertex.names) ))]
      v_not_ok = v_id[which(!is.element(v_id, as.character(d$vertex.names) ))]
    } 
    if( plot_type == "map" & format == "unipart_sl" ){
      v_ok = v_id[which(is.element(v_id, as.character(d$location) ))]
      v_not_ok = v_id[which(!is.element(v_id, as.character(d$location) ))]
    }    

    if( plot_type == "network" & format == "unipart_sl" ){
      v_ok = v_id[which(is.element(v_id, as.character(d$vertex.names) ))]
      v_not_ok = v_id[which(!is.element(v_id, as.character(d$vertex.names) ))]
    } 
    
    pies = pies[v_ok]
    
    if( length(v_ok) == 0) { 
      warning("In data_to_pie, no id exist in the network object and therefore no pies are displayed") 
    }
    if( length(v_ok) < length(v_id) ){ 
      warning("In data_to_pie, the following id does not exist in the network objet: ", paste(v_not_ok, collapse = ",")) 
    }
    
    if( ptype == "map" ){ 
      if( length(v_ok) > 0 ) {
        d = ggnetwork(n, arrow.gap = 0)
        if( format == "bipart" ) {
          d = d[which(d$type == "location"), c("lat", "long", "vertex.names")]
          colnames(d)[ncol(d)] = "location"
        } 
        if( format == "unipart_location" ){
          d = d[c("lat", "long", "vertex.names")]
          colnames(d)[ncol(d)] = "location"
        }
        d = unique(d[, c("lat", "long", "location")])
        
        piecoords = lapply(names(pies), function(x){
          c(x = as.numeric(as.character(d[which(d$location == x), "long"])), 
            y = as.numeric(as.character(d[which(d$location == x), "lat"]))
          )
          } 
      )
      }
    }
    
    
    if( ptype == "network" ){ 
      if( length(v_ok) > 0 ) {
        piecoords = lapply(names(pies), function(x){
          c(x = unique(p$data[which(p$data$vertex.names == x), "x"]), y = unique(p$data[which(p$data$vertex.names == x), "y"]))
        }
        )
      }
    }
    
    # add pies on plot
    if( length(v_ok) > 0 ) {
      for(i in 1:length(pies)){
        p = p + geom_subview(x = piecoords[[i]]["x"], y = piecoords[[i]]["y"], 
                             subview = pies[[i]], 
                             width = (xmax-xmin)*pie_size, height = (ymax-ymin)*pie_size)
      }
    }

    return(p)
  }
  
  # run functions
  run_fun = function(  net,
                       format,
                       plot_type,
                       in_col,
                       labels_on,
                       labels_size,
                       organize_sl,
                       x_axis,
                       nb_parameters_per_plot_x_axis,
                       nb_parameters_per_plot_in_col
  ){
    if( plot_type == "network" ) {
      if( format == "bipart" ) { 
        out = list("network" = plot_network_bipart(net, labels_on, labels_size))
      } else {
        
        if( organize_sl){ 
          out = organize_sl_unipart(net) 
          person_limit = out$person_limit
          n = out$n
        } else { 
          n = ggnetwork(net, arrow.gap = 0.005) 
        }
        
        if( organize_sl){ 
          warning("with organize_sl = TRUE, in_col is automaticaly set to in_col = \"germplasm\".")
          p = plot_network_organize_sl_unipart(n, person_limit, in_col = "germplasm") 
        } else { 
          p = plot_network_unipart(n, in_col) + theme_blank() 
        }
        
        if( labels_on ){ 
          p = p + geom_nodelabel_repel(aes(label = vertex.names), size = labels_size) 
        }
        
        out = list("network" = p)
      }
    }
    
    if( plot_type == "barplot" ) {
      if( format == "bipart" ) { 
        out = list("barplot" = plot_barplot_bipart(net))
      } else {
        out = list("barplot" = plot_barplot_unipart(net, x_axis, in_col, nb_parameters_per_plot_x_axis, 
                                                    nb_parameters_per_plot_in_col))
      }
    }
    
    if( plot_type == "map" ){
      out = list("map" = pmap(net, format, labels_on, labels_size) )
    }
    
    out_all = list("net" = net, "out" = out)
    
    return(out_all)
  }
  
  if( plot_type == "network" ){
    test = which(unlist(lapply(net, function(x){ length(E(x)) == 1 })))
    if( length(test) > 0 ){ 
      warning("The following element are not taken into account because they have only one edge: ", paste(names(net)[test], collapse = " ,"), ". igraph objet with only one edge are not handle by ggnetwork. See https://github.com/briatte/ggnetwork/pull/18")
      net = net[-test]
    }
  }
  
  out_all = lapply(net, run_fun, format, plot_type, in_col, labels_on, labels_size, organize_sl, x_axis, 
               nb_parameters_per_plot_x_axis, nb_parameters_per_plot_in_col)
  
  # add pies
  if( !is.null(data_to_pie) ) {
    out = lapply(out_all, function(x) { 
      p = list(add_pies(x$out[[1]], x$net, format, plot_type, data_to_pie, variable, pie_size))
      names(p) = paste(plot_type, "_with_pies", sep = "")
      return(p)
      }
    )
  } else { out = lapply(out_all, function(x){x$out}) }
  
  return(out)
}
