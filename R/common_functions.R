#' Some functions used in one or several functions of PPBstats
#' 
#' @name common_functions
#' 
#' @description
#' This file group several functions used in several functions of PPBstats
#' 
#' @author Pierre Riviere
#' 

# Function use in describe_data.R, GxE.R, model_1.R, model_2.R ----------
check_data_vec_variables = function(data, vec_variables){
  for(variable in vec_variables) { if(!is.element(variable, colnames(data))) { stop(variable," is not in data") } }
  for(variable in vec_variables) { 
    if(!is.numeric(data[,variable])) { stop(variable," is not numeric") } }
}

# Function use in describe_data.R ----------
split_data_for_ggplot = function(data, factor, nb_param){
  ns = unique(data[,factor])
  s = rep(c(1:length(ns)), each = nb_param)[1:length(ns)]
  names(s) = ns
  data$split_factor = s[data[,factor]]
  data_f =  plyr:::splitter_d(data, .(split_factor))
  return(data_f)
}

# Function use in which_won_where.R, mean_vs_stability.R ----------
get_biplot = function(res.pca){
  
  var = as.data.frame(res.pca$var$coord)
  var = cbind.data.frame(rownames(var), var, color = "darkgreen"); colnames(var)[1:3] = c("label", "x", "y")
  
  ind = as.data.frame(res.pca$ind$coord)
  ind = cbind.data.frame(rownames(ind), ind, color = "black"); colnames(ind)[1:3] = c("label", "x", "y")
  
  r <- min((max(ind[, "x"]) - min(ind[, "x"])/(max(var[, "x"]) - min(var[, "x"]))), (max(ind[, "y"]) - min(ind[, "y"])/(max(var[, "y"]) - min(var[, "y"]))))
  var[, c("x", "y")] <- var[, c("x", "y")] * r * 0.7 # taken from factoextra::fviz_pca_biplot
  
  vi = rbind.data.frame(var, ind)
  vi$size = 4
  vi$size[which(vi$color == "darkgreen")] = 6
  
  dimvar = round(as.data.frame(res.pca$eig)$`percentage of variance`[1:2], 1)
  
  p = ggplot(data = vi, aes(x = x, y = y, label = label)) + geom_text(color = as.character(vi$color), size = vi$size) + geom_point(color = as.character(vi$color))
  p = p + xlab(paste("Dim 1 (", dimvar[1], "%)", sep = "")) + ylab(paste("Dim 2 (", dimvar[2], "%)", sep = ""))
  p = p + ggtitle("Biplot germplasm and locations")
  p = p + geom_vline(xintercept = 0, linetype = "longdash",color="grey") + geom_hline(yintercept = 0, linetype = "longdash",color="grey")
  
  return(p)
}

get_perpendicular_segment = function(x1, y1, x2, y2, x3, y3, longer = FALSE){
  # following formulas thanks to jdbertron cf http://stackoverflow.com/questions/10301001/perpendicular-on-a-line-segment-from-a-given-point
  px = x2-x1
  py = y2-y1
  dAB = px*px + py*py
  u = ((x3 - x1) * px + (y3 - y1) * py) / dAB
  x4 = x1 + u * px
  y4 = y1 + u * py
  
  # to make the segment longer
  if(longer & x4 != 0){
    y4 = y4/x4 * x4*1000000
    x4 = x4*1000000
  }
  
  return(c(x1 = x3, y1 = y3, x2 = x4, y2 = y4))
}


# Function use in check_model_model_1.R, check_model_model_2.R ----------
check_analysis_argument = function(analysis){
  if(!is.null(analysis)) { 
    if( !is.element(analysis, c("experimental_design", "convergence", "posteriors")) ){ stop("analysis must be \"experimental_design\", \"convergence\" or \"posteriors\".") }  
    if( !is.element(analysis, c("convergence")) ){ warning("\"convergence\" is not chosen! You may make mistakes in the interpretation of the results !!!") }  
  } else { analysis = "all" }
  return(analysis)
}

check_convergence = function(out.model, model_name = "model1"){
  MCMC = out.model$MCMC
  MCMC = rbind.data.frame(MCMC[[1]], MCMC[[2]])
  attributes(MCMC)$model = model_name
  
  s = summary(out.model$MCMC)
  sq_MCMC = as.data.frame(s$quantiles)
  sq_MCMC$parameter = as.factor(rownames(sq_MCMC))
  colnames(sq_MCMC) = c("q1", "q2", "q3", "q4", "q5", "parameter")
  
  message("The Gelman-Rubin test is running for each parameter ...")
  test = gelman.diag(out.model$MCMC, multivariate = FALSE)$psrf[,1]
  conv_ok = names(which(test < 1.05))
  conv_not_ok = names(which(test > 1.05))
  
  if( length(conv_not_ok) > 0 ) {
    message("The two MCMC of the following parameters do not converge thanks to the Gelman-Rubin test : ", paste(conv_not_ok, collapse = ", ") ,". Therefore, they are not present in MCMC output.")
    } else { 
    message("The two MCMC for each parameter converge thanks to the Gelman-Rubin test.")
    }
  OUT = list("MCMC" = MCMC, "sq_MCMC" = sq_MCMC, "conv_not_ok" = conv_not_ok)
  return(OUT)
}

# Function use in ggplot_check_model_model_1.R, ggplot_check_model_model_2.R ----------

get.caterpillar.plot = function(x, xmin, xmax){ # cf ggmcmc:ggs_caterpillar
  p = ggplot(x, aes(x = q3, y = reorder(parameter, q3))) 
  p = p + geom_point(size = 3) # median 
  p = p + geom_segment(aes(x = q2, xend = q4, yend = reorder(parameter, q3)), size = 1.5) # 25%-75%
  p = p + geom_segment(aes(x = q1, xend = q5, yend = reorder(parameter, q3)), size = 0.5) # 2.5%-25% and 75%-97.5%
  p = p + ylab("parameter") + xlab("value") + ggtitle(x[1, "environment"])
  p = p + coord_cartesian(xlim = c(xmin, xmax))
  return(p)
}


get_mcmc_traceplot_density = function(MCMC){
  if( is.vector(MCMC) ) { 
    mcmc = as.data.frame(matrix(MCMC, ncol = 1))
    colnames(mcmc) = names(MCMC)[1]
    MCMC = mcmc
    }
  conv_not_ok = colnames(MCMC)
  vec.plot = NULL
  for (para in conv_not_ok) {
    D = cbind.data.frame(Iteration = rep(c(1:(nrow(MCMC)/2)), 2), 
                         Chain = factor(rep(c(1,2), each = (nrow(MCMC)/2))), 
                         Parameter = para, 
                         value = as.vector(MCMC[,para])
    )
    traceplot = ggplot(D, aes(x = Iteration, y = value, color = Chain)) + geom_line() + ggtitle(para) # cf ggmcmc:ggs_traceplot
    density = ggplot(D, aes(x = value, fill = Chain, color = Chain)) + geom_density() + ggtitle(para) # cf ggmcmc:ggs_density
    plot = list(list("traceplot" = traceplot, "density" = density))
    names(plot) = para
    vec.plot = c(vec.plot, plot)
  }
  return(vec.plot)
}

  

# Function used in mean_comparisons_model_1.R and mean_comparisons_model_2.R ----------

get_mean_comparisons_and_Mpvalue = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
  
  if( !is.element(type, c(1,2)) ){ stop("type must be 1 or 2") }

  Mpvalue = comp.parameters(MCMC = MCMC, parameter = parameter, type = type, threshold = threshold)
  
  if(type == 1 & is.null(Mpvalue)) { message("mean comparisons not done for ", sub("\\\\\\[", "", element), " because there are less than two parameters to compare.") }
  
  
  if(type == 1 & !is.null(Mpvalue)) {
    Comparison = get.significant.groups(Mpvalue = Mpvalue, MCMC = MCMC, alpha = alpha, p.adj = p.adj)
    
    # number of groups
    a = unlist(strsplit(paste(Comparison[, "groups"], collapse = ""), ""))
    nb_group = length(unique(a))
    
    # get at least X groups
    if(nb_group == 1 & !is.null(get.at.least.X.groups)) {
      env = sub("\\]", "", unique(sapply(colnames(MCMC), function(x){unlist(strsplit(x, ","))[2]})))
      message(paste("Get at least X groups for ", sub("\\\\\\[", "", env),". It may take some time ...", sep = "")) # The sub is useful for model2
      ALPHA = get.at.least.X.groups(Mpvalue, MCMC, p.adj = p.adj, precision = precision)  
      alp = ALPHA[paste(get.at.least.X.groups, "_groups", sep = "")]  
      if(is.numeric(alp)){ alp = round(alp, 3) }
      message(paste("Get at least X groups for", sub("\\\\\\[", "", env),"is done."))
    } else { alp = alpha }
    
    TAB = cbind.data.frame("parameter" = Comparison$parameter,
                           "median" = Comparison$median, 
                           "groups" = Comparison$groups, 
                           "nb_group" = rep(nb_group, nrow(Comparison)), 
                           "alpha" = rep(alp, nrow(Comparison)),
                           "alpha.correction" = rep(p.adj, nrow(Comparison))
    )
    
  }
  
  if(type == 2) { 
    TAB = cbind.data.frame("proba" = Mpvalue) 
    o = order(TAB$proba)
    tab = as.data.frame(matrix(TAB[o,], ncol = 1)); rownames(tab) = rownames(TAB)[o]
    TAB = tab
  }
  
  out = list(TAB, Mpvalue)
  names(out) = c("mean.comparisons", "Mpvalue")
  return(out)
}

# Function use in check_model_model_1.R, check_model_model_2.R ----------
add_split_col = function(x, each){ rep(c(1:nrow(x)), each = each)[1:nrow(x)] } 

# Function use in ggplot_which_won_where.R, ggplot_mean_vs_stability.R ----------

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
  
  Area = 0.5 *(-p1y*p2x + p0y*(-p1x + p2x) + p0x*(p1y - p2y) + p1x*p2y)
  s = 1/(2*Area)*(p0y*p2x - p0x*p2y + (p2y - p0y)*px + (p0x - p2x)*py)
  t = 1/(2*Area)*(p0x*p1y - p0y*p1x + (p0y - p1y)*px + (p1x - p0x)*py)
  
  test = s > 0 & t > 0 & (1-s-t) > 0
  return(test)
}


# Reshape data in a list based on nb_parameters_per_plot arguments ----------
# used in describe_data.data_agro.R and plot.data_network.R
reshape_data_split_x_axis_in_col = function(
  d, 
  vec_variables, 
  labels_on,
  x_axis, 
  nb_parameters_per_plot_x_axis, 
  in_col, 
  nb_parameters_per_plot_in_col
){
  
  if(!is.null(x_axis)){ d$x_axis = as.factor(as.character(d[,x_axis])) } else { d$x_axis = NA }
  if(!is.null(in_col)){ d$in_col = as.factor(as.character(d[,in_col])) } else { d$in_col = NA }
  if(!is.null(labels_on)){ d$labels_text = d[,labels_on] } else { d$labels_text = NA }
  d_head = d[,c("labels_text", "x_axis", "in_col")]
  
  d_var = as.data.frame(as.matrix(d[,vec_variables], ncol = 1))
  
  # get rid off rows with only NA
  tokeep = apply(d_var, 1, function(x){length(which(is.na(x))) != length(x)})
  t = length(which(!tokeep))
  if( t > 0 ) { warning(t, " rows have been deleted for ", paste(vec_variables, collapse = ", "), " because of only NA on the row for these variables.") }
  d_var = d_var[tokeep,]
  d_var = as.data.frame(as.matrix(d[,vec_variables], ncol = 1))
  colnames(d_var) = vec_variables
  
  d_head = d_head[tokeep,]
  
  d = droplevels(cbind.data.frame(d_head, d_var))
  
  # split for x_axis
  if(!is.null(x_axis)){
    ns = unique(d$x_axis)
    s = rep(c(1:length(ns)), each = nb_parameters_per_plot_x_axis)[1:length(ns)]
    names(s) = ns
    d$split_x_axis = s[d$x_axis]
  } else { d$split_x_axis = NA }
  
  # split for in_col
  if(!is.null(in_col)){
    ns = unique(d$in_col)
    s = rep(c(1:length(ns)), each = nb_parameters_per_plot_in_col)[1:length(ns)]
    names(s) = ns
    d$split_in_col = s[d$in_col]
  } else { d$split_in_col = NA }
  
  # Overall split
  d$split = paste(
    paste(x_axis, d$split_x_axis, sep = "-"), 
    paste(in_col, d$split_in_col, sep = "-"), 
    sep = "|")
  d = dplyr::select(d, - split_x_axis, - split_in_col)
  d = plyr:::splitter_d(d, .(split))
  
  return(d)
}		


# ggradar taken from https://github.com/ricardo-bion/ggradar
ggradar <- function(plot.data,
                    font.radar="Circular Air Light",
                    values.radar = c("0%", "50%", "100%"),                       
                    axis.labels=colnames(plot.data)[-1],                             
                    grid.min=0,  #10,
                    grid.mid=0.5,  #50,
                    grid.max=1,  #100,
                    centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                    plot.extent.x.sf=1,
                    plot.extent.y.sf=1.2,
                    x.centre.range=0.02*(grid.max-centre.y),
                    label.centre.y=FALSE,
                    grid.line.width=0.5,
                    gridline.min.linetype="longdash",
                    gridline.mid.linetype="longdash",
                    gridline.max.linetype="longdash",
                    gridline.min.colour="grey",
                    gridline.mid.colour="#007A87",
                    gridline.max.colour="grey",
                    grid.label.size=7,
                    gridline.label.offset=-0.1*(grid.max-centre.y),
                    label.gridline.min=TRUE,
                    axis.label.offset=1.15,
                    axis.label.size=8,
                    axis.line.colour="grey",
                    group.line.width=1.5,
                    group.point.size=6,
                    group.colours=NULL,
                    background.circle.colour="#D7D6D1",
                    background.circle.transparency=0.2,
                    plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
                    legend.title="",
                    plot.title="",
                    legend.text.size=grid.label.size ) {
  
  plot.data <- as.data.frame(plot.data)
  
  plot.data[,1] <- as.factor(as.character(plot.data[,1]))
  names(plot.data)[1] <- "group"
  
  var.names <- colnames(plot.data)[-1]  #'Short version of variable names 
  #axis.labels [if supplied] is designed to hold 'long version' of variable names
  #with line-breaks indicated using \n
  
  #calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
  
  #Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data)-1) 
    return("Error: 'axis.labels' contains the wrong number of axis labels") 
  if(min(plot.data[,-1])<centre.y)
    return("Error: plot.data' contains value(s) < centre.y")
  if(max(plot.data[,-1])>grid.max)
    return("Error: 'plot.data' contains value(s) > grid.max")
  #Declare required internal functions
  
  CalculateGroupPath <- function(df) {
    #Converts variable values into a set of radial x-y coordinates
    #Code adapted from a solution posted by Tony M to
    #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
    #Args:
    #  df: Col 1 -  group ('unique' cluster / group ID of entity)
    #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
    
    path <- df[,1]
    
    ##find increment
    angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
    ##create graph data frame
    graphData= data.frame(seg="", x=0,y=0)
    graphData=graphData[-1,]
    
    for(i in levels(path)){
      pathData = subset(df, df[,1]==i)
      for(j in c(2:ncol(df))){
        #pathData[,j]= pathData[,j]
        
        
        graphData=rbind(graphData, data.frame(group=i, 
                                              x=pathData[,j]*sin(angles[j-1]),
                                              y=pathData[,j]*cos(angles[j-1])))
      }
      ##complete the path by repeating first pair of coords in the path
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,2]*sin(angles[1]),
                                            y=pathData[,2]*cos(angles[1])))
    }
    #Make sure that name of first column matches that of input data (in case !="group")
    colnames(graphData)[1] <- colnames(df)[1]
    graphData #data frame returned by function
  }
  CaclulateAxisPath = function(var.names,min,max) {
    #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
    #Args:
    #var.names - list of variables to be plotted on radar plot
    #min - MININUM value required for the plotted axes (same value will be applied to all axes)
    #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
    #var.names <- c("v1","v2","v3","v4","v5")
    n.vars <- length(var.names) # number of vars (axes) required
    #Cacluate required number of angles (in radians)
    angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
    #calculate vectors of min and max x+y coords
    min.x <- min*sin(angles)
    min.y <- min*cos(angles)
    max.x <- max*sin(angles)
    max.y <- max*cos(angles)
    #Combine into a set of uniquely numbered paths (one per variable)
    axisData <- NULL
    for (i in 1:n.vars) {
      a <- c(i,min.x[i],min.y[i])
      b <- c(i,max.x[i],max.y[i])
      axisData <- rbind(axisData,a,b)
    }
    #Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) <- c("axis.no","x","y")
    rownames(axisData) <- seq(1:nrow(axisData))
    #Return calculated axis paths
    as.data.frame(axisData)
  }
  funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
    #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data 
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)
  
  #print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)
  # (e) Create Circular grid-lines + labels
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  #print(head(gridline$max$path))
  #gridline labels
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                   text=as.character(grid.min))
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                   text=as.character(grid.max))
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                   text=as.character(grid.mid))
  #print(gridline$min$label)
  #print(gridline$max$label)
  #print(gridline$mid$label)
  ### Start building up the radar plot
  
  # Declare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size=20) + 
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))
  
  if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")
  
  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)], 
  # then centred labels for axis labels almost immediately above/below x= 0 
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly 
  # identify plot extent when plotting first (base) layer]
  
  #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
              aes(x=x,y=y,label=text),size=axis.label.size,hjust=1, family=font.radar) +
    scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) + 
    scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  
  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5, family=font.radar)
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0, family=font.radar)
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                              fill=background.circle.colour,
                              alpha=background.circle.transparency)
  
  # + radial axes
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           colour=axis.line.colour)
  
  
  # ... + group (cluster) 'paths'
  base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                           size=group.line.width)
  
  # ... + group points (cluster data)
  base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)
  
  
  #... + amend Legend title
  if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)
  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                            lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
                            lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                            lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
  # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
  if (label.gridline.min==TRUE) {
    
    base <- base + geom_text(aes(x=x,y=y,label=values.radar[1]),data=gridline$min$label,size=grid.label.size*0.8, hjust=1, family=font.radar) }
  base <- base + geom_text(aes(x=x,y=y,label=values.radar[2]),data=gridline$mid$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  base <- base + geom_text(aes(x=x,y=y,label=values.radar[3]),data=gridline$max$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y==TRUE) {
    centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
    base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,size=grid.label.size, hjust=0.5, family=font.radar) }
  
  if (!is.null(group.colours)){
    colour_values=rep(group.colours,100)
  } else {
    colour_values=rep(c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051", 
                        "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"), 100)
  }
  
  base <- base + theme(legend.key.width=unit(3,"line")) + theme(text = element_text(size = 20,
                                                                                    family = font.radar)) +
    theme(legend.text = element_text(size = legend.text.size), legend.position="left") +
    theme(legend.key.height=unit(2,"line")) +
    scale_colour_manual(values=colour_values) +
    theme(text=element_text(family=font.radar)) + 
    theme(legend.title=element_blank())
  
  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }
  
  return(base)
  
}

