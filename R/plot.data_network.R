#' Plot network object from format_data_PPBstats()
#' 
#' @description
#' \code{plot.data_network} returns ggplot to visualize outputs from format_data_PPBstats()
#' 
#' @param x output from \code{\link{format_data_PPBstats.data_network}}
#' 
#' @param data_to_pie output from format_data_PPBstats with data_type = "data_agro"
#' 
#' @param variable when data_to_pie is not NULL, variable to plot in a pie
#'
#' @param pie_size when data_to_pie is not NULL, size of the pie 
#'   
#' @param plot_type "network", "barplot" or "map"
#' 
#' @param in_col factor in color that fill the barplot or the vertex of the network.
#' It can be germplasm, location or year
#' 
#' @param labels_on for plot_type = "network", labels to display on network for each vertex or location for map
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
#' @param vec_variables for plot_type = "barplot" and unipart seed lots network, name of a relation type to display
#' 
#' @param zoom zoom of the map, see ?get_map for more details
#' 
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' For network diffusion are represented by a curve.
#' For organize_sl, The representation is possible if the seed_lots are under the following format : 
#' GERMPLASM_LOCATION_YEAR_DIGIT.
#' 
#' The seed_lot column from data_to_pie must refer to the seed_lot of the network.
#' The results is a list of two elements for each variable:
#' \itemize{
#'  \item nb_received: number of seed lots that end the relation
#'  \item nb_given: number of seed lots that start the relation
#' }
#' 
#' @return 
#' A list with ggplot object.
#' For plot_type = "network", a list with as many elements as net with the network representation in ggplot format
#' For plot_type = "barplot" and bipart network, it represents the number of edges per vertex for each germplasm 
#' and each location.
#' For plot_type = "barplot" and unipart network on seed lots, it represents the number of edges per vertex for each germplasm 
#' and each location.
#' 
#' @author Pierre Riviere
#' 
#' @details 
#' S3 method.
#' See the book for more details : https://priviere.github.io/PPBstats_book/describe-data-network.html
#' 
#' @seealso
#' \code{\link{format_data_PPBstats.data_network}}
#' 
#' @export
#' 
#' @import ggnetwork
#' @import intergraph
#' @import ggplot2
#' @import igraph
#' 
plot.data_network = function(
  x,
  data_to_pie = NULL,
  variable = NULL,
  pie_size = 0.2,
  plot_type = "network",
  in_col = "location",
  labels_on = FALSE,
  labels_size = 4,
  organize_sl = FALSE,
  x_axis = "germplasm",
  nb_parameters_per_plot_x_axis = 5,
  nb_parameters_per_plot_in_col = 5,
  vec_variables = NULL,
  zoom = 6, ...
  ){
  
  net = x
  
  # check arguments
  match.arg(plot_type,  c("network", "barplot", "map"), several.ok = FALSE)
  match.arg(in_col,  c("germplasm", "location", "year"), several.ok = FALSE)
  match.arg(x_axis,  c("germplasm", "location", "year"), several.ok = FALSE)
  
  format = ggnetwork::ggnetwork(net[[1]])[1, "format"]
  
  # error and warning messages
  if( !is.list(net[1]) ){ 
    stop("net must be a list. I.e. with net coming from format_data_PPBstats, use net[1] is ok and not net$`blabla`") 
    }
  
  if( plot_type == "network" | ( plot_type == "map" & format == "unipart_location") ){
    test = which(unlist(lapply(net, function(x){ length(E(x)) == 1 })))
    if( length(test) > 0 ){ 
      warning("The following element are not taken into account because they have only one edge: ", paste(names(net)[test], collapse = " ,"), ". igraph objet with only one edge are not handle by ggnetwork. See https://github.com/briatte/ggnetwork/pull/18")
      net = net[-test]
    }
    if( length(net) == 0 ){ stop("There are no more network to display as there were only one edge per network.") }
  }
  
  pie_on_map = FALSE
  if( !is.null(data_to_pie) ){
    if(!is(data_to_pie, "data_agro")){ stop(substitute(data_to_pie), " must be formated with type = \"data_agro\", see PPBstats::format_data_PPBstats().") }
    if( !is.element(plot_type, c("network", "map")) ) { stop("data_to_pie can be used only with plot_type = network or map") }
    if( is.null(vec_variables[1]) ) { stop("with data_to_pie, variable must not be NULL") }
    for(v in vec_variables){
      if( !is.element(v, colnames(data_to_pie) ) ) { stop(v, " is not present in data_to_pie") }
    }
    pie_on_map = TRUE
  } else {
    if( !is.null(vec_variables[1]) ){
      vec_relation_type = unlist(lapply(net, function(x){ unique(na.omit(ggnetwork::ggnetwork(x)$relation_type)) }))
      mess = paste(variable, " can be used only is data_to_pie is not NULL or 
                 must  be a type of relation used in the network: ", paste(vec_relation_type, collapse = ", "), sep = "")
      # Only if data_to_pie is NULL to avoid conflict if same name in edge and data_to_pie
      for(v in vec_variables){
        if( !is.element(v, vec_relation_type ) ) { stop(mess) }
      }
      if( !is.element(plot_type, c("barplot", "map")) ) { stop("relation of a network can be used only with plot_type = barplot or map") }
      if( plot_type == "map" ) { pie_on_map = TRUE }
    }
  }
  
  if( format == "bipart" & plot_type == "network" & !is.null(data_to_pie) ) { 
    stop("With bipart network, pies on network are not possible.") 
  }
  
  if( format == "unipart_location" & organize_sl ) { 
    stop("With unipart network on location, organize_sl can not be used.") 
  }
  if( format == "bipart" & organize_sl ) { 
    stop("With bipart network, organize_sl can not be used.") 
  }
  
  # functions used afterward
  plot_network_bipart = function(net, labels_on, labels_size){
    x = y = xend = yend = type = vertex.names = NULL  # to avoid no visible binding for global variable
    
    n = ggnetwork::ggnetwork(net, arrow.gap = 0)
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
    from = to = germplasm = nb_location = nb_germplasm = NULL  # to avoid no visible binding for global variable
    s = sapply(igraph::V(net)$name, function(x) length(igraph::E(net)[from(igraph::V(net)[x])]))
    s = s[which(igraph::vertex.attributes(net)$type == "germplasm")]
    d = data.frame(germplasm = names(s), nb_location = s)
    pg = ggplot(d, aes(x = reorder(germplasm, -nb_location), y = nb_location)) + geom_bar(stat="identity")
    pg = pg + xlab("germplasm") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    pg = pg + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    s = sapply(igraph::V(net)$name, function(x) length(igraph::E(net)[to(igraph::V(net)[x])]))
    s = s[which(igraph::vertex.attributes(net)$type == "location")]
    d = data.frame(location = names(s), nb_germplasm = s)
    pl = ggplot(d, aes(x = reorder(location, -nb_germplasm), y = nb_germplasm)) + geom_bar(stat="identity")
    pl = pl + xlab("location") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    pl = pl + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    out = list("germplasm" = pg, "location" = pl)
    return(out)
  }
  
  plot_barplot_unipart_location = function(net){
    from = to = nb_diffusion = NULL # to avoid no visible binding for global variable 
    
    s_r = sapply(igraph::V(net)$name, function(x) length(igraph::E(net)[to(igraph::V(net)[x])])) 
    s_g = sapply(igraph::V(net)$name, function(x) length(igraph::E(net)[from(igraph::V(net)[x])]))
    
    d_r = data.frame(location = names(s_r), nb_diffusion = s_r)
    pr = ggplot(d_r, aes(x = reorder(location, -nb_diffusion), y = nb_diffusion)) + geom_bar(stat="identity")
    pr = pr + xlab("location") + ylab("nb of seed-lots received")
    pr = pr + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    d_g = data.frame(location = names(s_g), nb_diffusion = s_g)
    pg = ggplot(d_g, aes(x = reorder(location, -nb_diffusion), y = nb_diffusion)) + geom_bar(stat="identity")
    pg = pg + xlab("location") + ylab("nb of seed-lots given")
    pg = pg + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    out = list("received" = pr, "given" = pg)
    return(out)
  }
  
  data_unipart_relation_type = function(net){
    from = to = NULL # to avoid no visible binding for global variable 
    
    s_r = sapply(igraph::V(net)$name, function(x) length(igraph::E(net)[to(igraph::V(net)[x])])) 
    s_g = sapply(igraph::V(net)$name, function(x) length(igraph::E(net)[from(igraph::V(net)[x])]))
    
    d = data.frame(
      id = igraph::vertex_attr(net)$name,
      location = igraph::vertex_attr(net)$location, 
      year = igraph::vertex_attr(net)$year,
      germplasm = igraph::vertex_attr(net)$germplasm, 
      block = NA,
      X = NA,
      Y = NA,
      nb_received = s_r,
      nb_given = s_g)
    
    n = unique(igraph::edge_attr(net)$relation_type)
    colnames(d)[8:9] = paste(n, colnames(d)[8:9], sep = "_")
    return(d)
  }
  
  plot_barplot_unipart_relation_type = function(net, x_axis, in_col, nb_parameters_per_plot_x_axis, 
                                                nb_parameters_per_plot_in_col){

    d = data_unipart_relation_type(net)
    vec_variables = colnames(d)[8:9]

    d_all_r = reshape_data_split_x_axis_in_col(d, 
                                            vec_variables = vec_variables[1], 
                                            labels_on = NULL,
                                            x_axis = x_axis, 
                                            nb_parameters_per_plot_x_axis = nb_parameters_per_plot_x_axis, 
                                            in_col = in_col, 
                                            nb_parameters_per_plot_in_col = nb_parameters_per_plot_in_col
    )
    d_all_g = reshape_data_split_x_axis_in_col(d, 
                                             vec_variables = vec_variables[2], 
                                             labels_on = NULL,
                                             x_axis = x_axis, 
                                             nb_parameters_per_plot_x_axis = nb_parameters_per_plot_x_axis, 
                                             in_col = in_col, 
                                             nb_parameters_per_plot_in_col = nb_parameters_per_plot_in_col
    )
    
    fun_bar = function(d, in_col){
      variable =  colnames(d)[4]
      colnames(d)[4] = "variable"
      if(!is.null(in_col)) {
        p = ggplot(d, aes(x = x_axis, y = variable, fill = in_col)) + geom_bar(stat = "identity")
      } else {
        p = ggplot(d, aes(x = x_axis, y = variable)) + geom_bar(stat = "identity") + xlab(x_axis) + ylab("")
      }
      p = p + xlab(x_axis) + ylab(variable) + theme(legend.title=element_blank())
      p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }

    out_r = lapply(d_all_r, fun_bar, in_col)
    out_g = lapply(d_all_g, fun_bar, in_col)
    out = list("nb_received" = out_r, "nb_given" = out_g)
    return(out)
  }
  
  organize_sl_unipart = function(net){
    n = ggnetwork::ggnetwork(net, arrow.gap = 0)
    
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
  
  plot_network_unipart = function(net = NULL, n = NULL, plot_type, in_col, pmap = "NULL"){
    x = y = xend = yend = relation_type = nb_diff = NULL  # to avoid no visible binding for global variable
    
    if( plot_type == "map" ){ # only use fo format = unipart_location
      if(is.null(n)) { n = ggnetwork::ggnetwork(net, arrow.gap = 0) }
      nd = n[which(n$relation_type == "diffusion"),]
      
      vec_x = as.vector(nd[is.na(nd$na.y), "long"])
      names(vec_x) = nd[is.na(nd$na.y), "x"]
      vec_y = as.vector(nd[is.na(nd$na.y), "lat"])
      names(vec_y) = nd[is.na(nd$na.y), "y"]
      
      nd[,"x"] = as.numeric(vec_x[as.vector(as.character(nd$x))])
      nd[,"xend"] = as.numeric(vec_x[as.vector(as.character(nd$xend))])
      nd[,"y"] = as.numeric(vec_y[as.vector(as.character(nd$y))])
      nd[,"yend"] = as.numeric(vec_y[as.vector(as.character(nd$yend))])
      
      p = pmap
      p = p + geom_nodes(data = nd, aes(x = x, y = y))
      p = p + geom_edges(data = nd, aes(x = x, y = y, xend = xend, yend = yend,
                                         linetype = relation_type, colour = nb_diff), 
                         arrow = arrow(length = unit(4, "pt"), type = "closed"), 
                         curvature = 0.2)
      p = p + scale_colour_gradient(low = "blue", high = "red")
    } else {
      if(is.null(n)) { n = ggnetwork::ggnetwork(net, arrow.gap = 0.005) }
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
    }
    p$labels$colour = in_col
    
    scale_ex = c("solid", "dotted", "longdash", "dashed", "twodash", "dotdash")
    p = p + scale_linetype_manual(values = scale_ex[1:length(na.omit(unique(n$relation_type)))] )
    return(p)
  }
  
  plot_network_organize_sl_unipart = function(n, person_limit, in_col){
    x = y = NULL  # to avoid no visible binding for global variable
    
    colnames(n)[which(colnames(n) == in_col)] = "in_col"
    p = plot_network_unipart(net = NULL, n, plot_type = "network", in_col)
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
  
  plot_barplot_unipart_sl = function(net, x_axis, in_col, nb_parameters_per_plot_x_axis, 
                                  nb_parameters_per_plot_in_col){
    n = ggnetwork::ggnetwork(net, arrow.gap = 0)
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
      } else {
        p = ggplot(d, aes(x = x_axis)) + geom_bar() + xlab(x_axis) + ylab("")
      }
      p = p + xlab(x_axis) + ylab("") + theme(legend.title=element_blank())
      p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    out = lapply(dall, fun_bar, in_col)
    
    return(out)
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
                       nb_parameters_per_plot_in_col,
                       vec_variables,
                       data_to_pie
  ){
    vertex.names = NULL  # to avoid no visible binding for global variable
    
    if( plot_type == "network" ) {
      if( format == "bipart" ) { 
        p = plot_network_bipart(net, labels_on, labels_size)
      } 
      
      if( format == "unipart_sl" ) { 
        if( organize_sl ){ 
          out = organize_sl_unipart(net) 
          person_limit = out$person_limit
          n = out$n
          warning("with organize_sl = TRUE, in_col is automaticaly set to in_col = \"germplasm\".")
          p = plot_network_organize_sl_unipart(n, person_limit, in_col = "germplasm") 
        } else {
          p = plot_network_unipart(net, n = NULL, plot_type, in_col) + theme_blank()
        }
      } 

      if( format == "unipart_location" ) { 
        p = plot_network_unipart(net, n = NULL, plot_type, in_col) + theme_blank() 
      } 
      
      if( labels_on == "location" | labels_on == "seed_lots" ){ 
        p = p + geom_nodelabel_repel(aes(label = vertex.names), size = labels_size) 
      }
        
      out = list("network" = p, "data_to_pie" = data_to_pie, "vec_variables" = vec_variables)
    }
    
    if( plot_type == "barplot" ) {
      if( format == "bipart" ) { 
        out = list("barplot" = plot_barplot_bipart(net))
      }
      if( format == "unipart_location" ) { 
        out = list("barplot" = plot_barplot_unipart_location(net))
      }
      if( format == "unipart_sl" & is.null(vec_variables[1]) ) { 
        out = list("barplot" = plot_barplot_unipart_sl(net, x_axis, in_col, nb_parameters_per_plot_x_axis, 
                                                    nb_parameters_per_plot_in_col))
      }
      if( format == "unipart_sl" & !is.null(vec_variables[1]) ) { 
        out = list()
        for(v in vec_variables){
          net_tmp = igraph::delete_edges(net, edges = which(!is.element(edge_attr(net)$relation_type, v)))
          out_p = plot_barplot_unipart_relation_type(net_tmp, x_axis, in_col, nb_parameters_per_plot_x_axis, 
                                                          nb_parameters_per_plot_in_col)
          out = c(out, list(out_p))
        }
        names(out) = vec_variables
      }
    }
    
    if( plot_type == "map" ){
      if( format == "unipart_location" ){
        out_m = pmap(net, format, labels_on, labels_size, zoom)
        out_pm = plot_network_unipart(net, n = NULL, plot_type, in_col, out_m) + theme_blank() 
        out_data_to_pie = data_to_pie
        out = list("map" =  out_m, "data_to_pie" = out_data_to_pie, "vec_variables" = vec_variables)
      }
      if( format == "unipart_sl" | format == "bipart" ){
        out_m = pmap(net, format, labels_on, labels_size, zoom)
        out_data_to_pie = data_to_pie
        if( is.null(data_to_pie) & !is.null(vec_variables[1]) ) {
          net = igraph::delete_edges(net, edges = which(!is.element(edge_attr(net)$relation_type, v)))
          out_data_to_pie = data_unipart_relation_type(net)
          vec_variables = colnames(out_data_to_pie)[8:9]
        }
        out = list("map" =  out_m, "data_to_pie" = out_data_to_pie, "vec_variables" = vec_variables)
      }
    }
    
    out_all = list("net" = net, "out" = out)
    
    return(out_all)
  }
  
  out_all = lapply(net, run_fun, format, plot_type, in_col, labels_on, labels_size, organize_sl, x_axis, 
               nb_parameters_per_plot_x_axis, nb_parameters_per_plot_in_col, vec_variables, data_to_pie)
  
  # add pies
  if( pie_on_map ) {
    fun_pies = function(x, format, plot_type, pie_size){
      out_p = list()
      vec_variables = x$out$vec_variables
      for(v in vec_variables){
        p = list(add_pies(x$out[[1]], x$net, format, plot_type, data_to_pie = x$out$data_to_pie, variable = v, pie_size))
        names(p) = paste(v, "_", plot_type, "_with_pies", sep = "")
        out_p = c(out_p, p)
      }
      return(out_p)
    }
    out = lapply(out_all, fun_pies, format, plot_type, pie_size)
  } else { 
    out = lapply(out_all, function(x){x$out[1]}) 
  }

  return(out)
}
