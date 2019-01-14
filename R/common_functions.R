#' Some functions used in one or several functions of PPBstats
#' 
#' @name common_functions
#' 
#' @description
#' This file group functions used in several functions of PPBstats
#' 
#' @author Pierre Riviere
#' 

# check_data_vec_variables ----------
#' check if variable are part of the data
#' @param data data frame
#' @param vec_variables variables to check
#' @export
check_data_vec_variables = function(data, vec_variables){
  for(variable in vec_variables) { if(!is.element(variable, colnames(data))) { stop(variable," is not in data") } }
  for(variable in vec_variables) { 
    if(!is.numeric(data[,variable])) { stop(variable," is not numeric") } }
}

# split_data_for_ggplot ----------
#' split data into several list for a given factor and a number of rows
#' @param data data frame
#' @param factor factor on which the split is done
#' @param nb_param number of rows
#' @export
#' @import plyr
split_data_for_ggplot = function(data, factor, nb_param){
  split_factor = NULL # to avoid no visible binding for global variable 
  ns = unique(data[,factor])
  s = rep(c(1:length(ns)), each = nb_param)[1:length(ns)]
  names(s) = ns
  data$split_factor = s[data[,factor]]
  data_f =  plyr:::splitter_d(data, .(split_factor))
  return(data_f)
}

# get_biplot ----------
#' get ggplot object for a biplot based on output from FactoMineR::PCA
#' @param res.pca output from FactoMineR::PCA
#' @export
#' @import ggplot2
get_biplot = function(res.pca){
  x = y = label = NULL # to avoid no visible binding for global variable
  
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

# get_perpendicular_segment ----------
#' get coordinate to draw perpendicular segment
#' @param x1 x coordinate of point 1 
#' @param y1 y coordinate of point 1 
#' @param x2 x coordinate of point 2 
#' @param y2 y coordinate of point 2 
#' @param x3 x coordinate of point 3 
#' @param y3 y coordinate of point 3 
#' @param longer TRUE or FALSE
#' @details following formulas thanks to jdbertron cf http://stackoverflow.com/questions/10301001/perpendicular-on-a-line-segment-from-a-given-point
#' @export
get_perpendicular_segment = function(x1, y1, x2, y2, x3, y3, longer = FALSE){
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


# check_analysis_argument ----------
#' error message regadring check_model of bayesian analysis
#' @param analysis type of analysis : "experimental_design", "convergence" or "posteriors"
#' @export
check_analysis_argument = function(analysis){
  if(!is.null(analysis)) { 
    if( !is.element(analysis, c("experimental_design", "convergence", "posteriors")) ){ stop("analysis must be \"experimental_design\", \"convergence\" or \"posteriors\".") }  
    if( !is.element(analysis, c("convergence")) ){ warning("\"convergence\" is not chosen! You may make mistakes in the interpretation of the results !!!") }  
  } else { analysis = "all" }
  return(analysis)
}

# check_convergence ----------
#' check convergence of bayesian model 
#' @param out.model output from bayesian model
#' @param model_name name of the model
#' @export
#' @import coda
check_convergence = function(out.model, model_name = "model1"){
  MCMC = out.model$MCMC
  MCMC = rbind.data.frame(MCMC[[1]], MCMC[[2]])
  attributes(MCMC)$model = model_name
  
  s = summary(out.model$MCMC)
  sq_MCMC = as.data.frame(s$quantiles)
  sq_MCMC$parameter = as.factor(rownames(sq_MCMC))
  colnames(sq_MCMC) = c("q1", "q2", "q3", "q4", "q5", "parameter")
  
  message("The Gelman-Rubin test is running for each parameter ...")
  test = coda::gelman.diag(out.model$MCMC, multivariate = FALSE)$psrf[,1]
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

# get.caterpillar.plot ----------
#' get caterpillar plot to view posterior of bayesian model
#' @param x data frame
#' @param xmin xmin of the plot
#' @param xmax xmax of the plot
#' @export
#' @import ggplot2
get.caterpillar.plot = function(x, xmin, xmax){ # cf ggmcmc:ggs_caterpillar
  parameter = q1 = q2 = q3 = q4 = q5 = NULL # to avoid no visible binding for global variable
  
  p = ggplot(x, aes(x = q3, y = reorder(parameter, q3))) 
  p = p + geom_point(size = 3) # median 
  p = p + geom_segment(aes(x = q2, xend = q4, yend = reorder(parameter, q3)), size = 1.5) # 25%-75%
  p = p + geom_segment(aes(x = q1, xend = q5, yend = reorder(parameter, q3)), size = 0.5) # 2.5%-25% and 75%-97.5%
  p = p + ylab("parameter") + xlab("value") + ggtitle(x[1, "environment"])
  p = p + coord_cartesian(xlim = c(xmin, xmax))
  return(p)
}

# get_mcmc_traceplot_density ----------
#' get mcmc traceplot density to view posterior of bayesian model
#' @param MCMC MCMC chain from bayesian model
#' @export
#' @import ggplot2
get_mcmc_traceplot_density = function(MCMC){
  Iteration = value = Chain  = NULL # to avoid no visible binding for global variable
  
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


# get_mean_comparisons_and_Mpvalue ----------
#' get mean comparisons and square matrix with pvalue from MCMC for bayesian models
#' @param MCMC MCMC 
#' @param parameter parameter
#' @param type type
#' @param threshold threshold
#' @param alpha alpha
#' @param p.adj p.adj
#' @param precision precision
#' @param get.at.least.X.groups get.at.least.X.groups
#' @details see \code{\link{mean_comparisons}}
#' @export
get_mean_comparisons_and_Mpvalue = function(MCMC, parameter, type, threshold, alpha, p.adj, precision, get.at.least.X.groups){
  element = NULL # to avoid no visible binding for global variable
  
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

# add_split_col ----------
#' add a column to split a dataframe
#' @param x vector
#' @param each nb of element iln each split
#' @export
add_split_col = function(x, each){ rep(c(1:nrow(x)), each = each)[1:nrow(x)] } 

# is.inside.sector ----------
#' to know if a point is inside an area
#' @param x x coordinate of point to know if it is inside the area or not
#' @param y y coordinate of point to know if it is inside the area or not
#' @param x1 x coordinate of point 1 of the area 
#' @param y1 y coordinate of point 1 of the area
#' @param x2 x coordinate of point 2 of the area 
#' @param y2 y coordinate of point 2 of the area
#' @param x3 x coordinate of point 3 of the area
#' @param y3 y coordinate of point 3 of the area
#' @details it is used for ggplot_which_won_where.R, ggplot_mean_vs_stability.R
#' @export
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


# reshape_data_split_x_axis_in_col ----------
#' Reshape data in a list based on nb_parameters_per_plot arguments
#' @details used in describe_data.data_agro.R and plot.data_network.R
#' @param d data frame
#' @param vec_variables vectors of variables
#' @param labels_on which label to display
#' @param x_axis x axis
#' @param nb_parameters_per_plot_x_axis nb of paramters for x axis on the lpot
#' @param in_col in color
#' @param nb_parameters_per_plot_in_col nb of paramters for color on the plot
#' @export
#' @import dplyr
#' @import plyr
reshape_data_split_x_axis_in_col = function(
  d, 
  vec_variables, 
  labels_on,
  x_axis, 
  nb_parameters_per_plot_x_axis, 
  in_col, 
  nb_parameters_per_plot_in_col
){
  split_x_axis = split_in_col  = NULL  # to avoid no visible binding for global variable
  
  if(!is.null(x_axis)){ d$x_axis = as.factor(as.character(d[,x_axis])) } else { d$x_axis = NA }
  if(!is.null(in_col)){ d$in_col = as.factor(as.character(d[,in_col])) } else { d$in_col = NA }
  if(!is.null(labels_on)){ d$labels_text = d[,labels_on] } else { d$labels_text = NA }
  d_head = d[,c("labels_text", "x_axis", "in_col")]
  
  if( length(vec_variables) == 1) {
    d_var = as.data.frame(as.matrix(d[,vec_variables], ncol = 1))
  } else { 
    d_var = d[,vec_variables]
    }
  
  # get rid off rows with only NA
  tokeep = apply(d_var, 1, function(x){length(which(is.na(x))) != length(x)})
  t = length(which(!tokeep))
  if( t > 0 ) { warning(t, " rows have been deleted for ", paste(vec_variables, collapse = ", "), " because of only NA on the row for these variables.") }

  if( length(vec_variables) == 1) {
    d_var = as.data.frame(as.matrix(d_var[tokeep,], ncol = 1))
  } else {
    d_var = d_var[tokeep,]
  }
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

# ggradar ----------
#' ggradar
#' @param plot.data arg
#' @param font.radar arg
#' @param values.radar arg
#' @param axis.labels arg
#' @param grid.min arg
#' @param grid.mid arg
#' @param grid.max arg
#' @param centre.y arg
#' @param plot.extent.x.sf arg
#' @param plot.extent.y.sf arg
#' @param x.centre.range arg
#' @param label.centre.y arg
#' @param grid.line.width arg
#' @param gridline.min.linetype arg
#' @param gridline.mid.linetype arg
#' @param gridline.max.linetype arg
#' @param gridline.min.colour arg
#' @param gridline.mid.colour arg
#' @param gridline.max.colour arg
#' @param grid.label.size arg
#' @param gridline.label.offset arg
#' @param label.gridline.min arg
#' @param axis.label.offset arg
#' @param axis.label.size arg
#' @param axis.line.colour arg
#' @param group.line.width arg
#' @param group.point.size arg
#' @param group.colours arg
#' @param background.circle.colour arg
#' @param background.circle.transparency arg
#' @param plot.legend arg
#' @param legend.title arg
#' @param plot.title arg
#' @param legend.text.size arg
#' @details taken from https://github.com/ricardo-bion/ggradar
#' @export
#' @import ggplot2
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
  x = y = text = axis.no  = NULL  # to avoid no visible binding for global variable
  
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


# check_freq_anova ----------
#' check freq anova
#' @param model anova model
#' @export
#' @import agricolae
check_freq_anova = function(model){
  r = y = percentage_Sum_sq  = NULL  # to avoid no visible binding for global variable
  
  anova_model = stats::anova(model)
  # 1. Check residuals (qqplot, Skewness & Kurtosis tests) ----------
  r = stats::residuals(model)
  
  # 1.1. Normality ----------
  data_ggplot_normality = data.frame(r)
  data_ggplot_skewness_test = agricolae::skewness(r)
  data_ggplot_kurtosis_test = agricolae::kurtosis(r)
  
  # 1.2. Standardized residuals vs theoretical quantiles ----------
  s = sqrt(deviance(model)/stats::df.residual(model))
  rs = r/s
  data_ggplot_qqplot = data.frame(x = qnorm(ppoints(rs)), y = sort(rs))
  
  # Test for homogeneity of variances
  #ft = fligner.test(variable ~ interaction(germplasm,location), data=data)
  #print(ft)
  
  # 2. repartition of variability among factors ----------
  total_Sum_Sq = sum(anova_model$"Sum Sq")
  Sum_sq = anova_model$"Sum Sq"
  pvalue = anova_model$"Pr(>F)"
  percentage_Sum_sq = Sum_sq/total_Sum_Sq*100
  factor = rownames(anova_model)
  data_ggplot_variability_repartition_pie = cbind.data.frame(factor, pvalue, Sum_sq, percentage_Sum_sq)
  
  # 3. variance intra germplasm
  var_intra = tapply(model$residuals, model$model$germplasm, var, na.rm = TRUE)
  data_ggplot_var_intra = data.frame(x = model$model$germplasm, y = model$residuals)
  
  data_ggplot = list(
      "data_ggplot_residuals" = list(
        "data_ggplot_normality" = data_ggplot_normality,
        "data_ggplot_skewness_test" = data_ggplot_skewness_test,
        "data_ggplot_kurtosis_test" = data_ggplot_kurtosis_test,
        "data_ggplot_qqplot" = data_ggplot_qqplot
      ),
      "data_ggplot_variability_repartition_pie" = data_ggplot_variability_repartition_pie,
      "data_ggplot_var_intra" = data_ggplot_var_intra
    )
  
  return(data_ggplot)
}

# plot check_freq_anova ----------
#' plot check freq anova
#' @param x output from check_model
#' @param variable variable
#' @export
#' @import ggplot2
plot_check_freq_anova = function(x, variable){
  r = y = percentage_Sum_sq = NULL # to avoid no visible binding for global variable 
  
  data_ggplot = x$data_ggplot
  
  data_ggplot_normality = data_ggplot$data_ggplot_residuals$data_ggplot_normality
  data_ggplot_skewness_test = data_ggplot$data_ggplot_residuals$data_ggplot_skewness_test
  data_ggplot_kurtosis_test = data_ggplot$data_ggplot_residuals$data_ggplot_kurtosis_test
  data_ggplot_qqplot = data_ggplot$data_ggplot_residuals$data_ggplot_qqplot
  data_ggplot_variability_repartition_pie = data_ggplot$data_ggplot_variability_repartition_pie
  data_ggplot_var_intra = data_ggplot$data_ggplot_var_intra
  
  # 1. Normality ----------
  # 1.1. Histogram ----------
  p = ggplot(data_ggplot_normality, aes(x = r), binwidth = 2)
  p = p + geom_histogram() + geom_vline(xintercept = 0)
  p = p + ggtitle("Test for normality", paste("Skewness:", signif(data_ggplot_skewness_test, 3), "; Kurtosis:", signif(data_ggplot_kurtosis_test, 3)))
  p1.1 = p + theme(plot.title=element_text(hjust=0.5))
  
  # 1.2. Standardized residuals vs theoretical quantiles ----------
  p = ggplot(data_ggplot_qqplot, aes(x = x, y = y)) + geom_point() + geom_line() 
  p = p + geom_abline(slope = 1, intercept = 0, color = "red")
  p = p + xlab("Theoretical Quantiles") + ylab("Standardized residuals")
  p1.2 = p + ggtitle("QQplot") + theme(plot.title=element_text(hjust=0.5))
  
  # 2. repartition of variability among factors ----------
  p = ggplot(data_ggplot_variability_repartition_pie, 
             aes(x = "", y = percentage_Sum_sq, fill = factor, 
                 label = paste(round(percentage_Sum_sq, 1), "%", sep = "")
                 )
  )
  p = p + ggtitle(paste("Total variance distribution for", variable))
  p = p + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
  p = p + geom_text(position = position_stack(vjust = 0.5))
  p2 = p + ylab("") + xlab("") + theme(plot.title=element_text(hjust=0.5))
  
  
  # 3. variance intra germplasm
  p = ggplot(data_ggplot_var_intra, aes(x = x, y = y))  + geom_boxplot(aes(color=x))
  p = p + ggtitle("Distribution of residuals") + xlab("germplasm") + ylab(variable)
  p = p + theme(legend.position = "none", axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5))
  p3 = p 
  
  # 4. return results
  out = list(
    "residuals" = list(
      "histogram" = p1.1,
      "qqplot" = p1.2),
    "variability_repartition" = p2,
    "variance_intra_germplasm" = p3
  )
  
  return(out)
}


# mean_comparisons_freq_anova ----------
#' mean comparisons for frequentist analysis 
#' @param model anova model
#' @param variable variable
#' @param alpha alpha
#' @param p.adj p.adj
#' @param info info from mean_comparisons
#' @export
#' @import agricolae
mean_comparisons_freq_anova = function(model, variable, alpha = 0.05, 
                                       p.adj = "none", info = NULL,
                                       vec_fac = c("germplasm", "location", "year")
                                       ){

  data_ggplot_LSDbarplot = function(model, fac, p.adj, alpha){
    lsd = agricolae::LSD.test(model, fac, alpha = alpha, p.adj = p.adj)
    
    parameter = factor(rownames(lsd$groups), levels = rownames(lsd$groups))
    means = lsd$groups[,1]
    groups = lsd$groups[,2]
    alpha = rep(alpha, length(parameter))
    alpha.correction = rep(p.adj, length(parameter))
    
    out_LSD = data.frame(parameter, means, groups, alpha, alpha.correction)
    if( nrow(out_LSD) == 0 ) { out_LSD = NULL }
    return(out_LSD)
  }
  
  # vec_fac = attr(model$terms,"term.labels")
  out = list()
  for(fac in vec_fac){
    out = c(out, list(data_ggplot_LSDbarplot(model, fac, p.adj, alpha)))
  }
  names(out) = paste("data_ggplot_LSDbarplot_", vec_fac, sep = "")
  
  # Return results
  out <- c(list("info" = info), out)

  return(out)
}

# plot_mean_comparisons_freq_anova ----------
#' plot mean comparisons for frequentist analysis 
#' @param x output from mean comparison
#' @param variable variable
#' @param nb_parameters_per_plot nb paramter per plot
#' @export
#' @import ggplot2
#' @import dplyr
#' @import plyr
plot_mean_comparisons_freq_anova = function(x, variable, nb_parameters_per_plot = 8){
  
  data_ggplot_LSDbarplot_germplasm = x$data_ggplot_LSDbarplot_germplasm
  data_ggplot_LSDbarplot_location = x$data_ggplot_LSDbarplot_location
  data_ggplot_LSDbarplot_year = x$data_ggplot_LSDbarplot_year
  
  ggplot_LSDbarplot = function(d_LSD, fac, variable, nb_parameters_per_plot){
    parameter = means  = NULL  # to avoid no visible binding for global variable
    
    d_LSD = dplyr::arrange(d_LSD, means) 
    d_LSD$max = max(d_LSD$means, na.rm = TRUE)
    d_LSD$split = add_split_col(d_LSD, nb_parameters_per_plot)
    d_LSD_split = plyr:::splitter_d(d_LSD, .(split))  
    
    out = lapply(d_LSD_split, function(dx){
      p = ggplot(dx, aes(x = reorder(parameter, means), y = means)) + geom_bar(stat = "identity")
      p = p + geom_text(aes(x = reorder(parameter, means), y = means/2, label = groups), angle = 90, color = "white")
      p = p + ggtitle(paste(fac, "\n alpha = ", dx[1, "alpha"], "; alpha correction :", dx[1, "alpha.correction"]))
      p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + coord_cartesian(ylim = c(0, dx[1,"max"])) + ylab(variable)
      return(p)
    })
    
    return(out)
  }
  
  # Germplasm
  if( !is.null(data_ggplot_LSDbarplot_germplasm) ){ 
    ggplot_LSDbarplot_germplasm = ggplot_LSDbarplot(data_ggplot_LSDbarplot_germplasm, "germplasm", variable, nb_parameters_per_plot) 
  } else {
    ggplot_LSDbarplot_germplasm = NULL
  }
  
  # Location
  if( !is.null(data_ggplot_LSDbarplot_location) ){ 
    ggplot_LSDbarplot_location = ggplot_LSDbarplot(data_ggplot_LSDbarplot_location, "location", variable, nb_parameters_per_plot) 
  } else {
    ggplot_LSDbarplot_location = NULL
  }
  
  # Year
  if( !is.null(data_ggplot_LSDbarplot_year) ){ 
    ggplot_LSDbarplot_year = ggplot_LSDbarplot(data_ggplot_LSDbarplot_year, "year", variable, nb_parameters_per_plot) 
  } else {
    ggplot_LSDbarplot_year = NULL
  }
  
  # Return results
  out = list(
    "germplasm" = ggplot_LSDbarplot_germplasm, 
    "location" = ggplot_LSDbarplot_location, 
    "year" = ggplot_LSDbarplot_year
  )
  
  return(out) 
}

# pmap -----
#' map background for plot.data_agro, plot.data_network 
#' @param net network object or data frame
#' @param format network format
#' @param labels_on where display label
#' @param labels_size label size
#' @param zoom zoom of the map
#' @export
#' @import ggmap
#' @import ggnetwork
#' @import intergraph
#' @import png
#' @import grid
#' 
pmap = function(net, format, labels_on, labels_size, zoom){
  wt = mpg = long = lat  = NULL  # to avoid no visible binding for global variable
  
  # As it is not possible to use annotation_custom with polar coordinates (i.e. output from ggmap) in order to add pies on map,
  # I decided to transfer ggmap output to a png that is inserted in a background of a plot with cartesian coordinates
  # Note there is a change in the look of the map because of coordinates change ...
  if( is_igraph(net) ){
    d = ggnetwork::ggnetwork(net, arrow.gap = 0)
    
    if( format == "bipart" ) {
      d = d[which(d$type == "location"), c("lat", "long", "vertex.names")]
      colnames(d)[ncol(d)] = "location"
    } 
    if( format == "unipart_location" ){
      d = d[c("lat", "long", "vertex.names")]
      colnames(d)[ncol(d)] = "location"
    }
  } else { d = net }

  n = unique(d[, c("lat", "long", "location")]) 
  n$lat = as.numeric(as.character(n$lat))
  n$long = as.numeric(as.character(n$long))
  n = na.omit(n)
  center_location = c(mean(n$long), mean(n$lat))
  map = ggmap::get_map(location = center_location, source = "google", zoom = zoom)
  m = ggmap::ggmap(map, extent = "device")
  ggsave("tmp_map.png", m, width = 1, height = 1) # get a perfect square
  p = ggplot() # support for the map background
  p = p + coord_cartesian(xlim = range(m$data$lon), ylim = range(m$data$lat), expand = FALSE)
  img = png::readPNG("tmp_map.png")
  pmap = p + annotation_custom(grid::rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc")), 
                               -Inf, Inf, -Inf, Inf) # change in the look of the map because of coordinates changes
  pmap = pmap + xlab("long") + ylab("lat")
  file.remove("tmp_map.png")
  if( !is.null(labels_on) ){
    if( labels_on == "location" ){ 
      pmap = pmap + geom_nodelabel_repel(data = n, aes(x = long, y = lat, label = location), size = labels_size, inherit.aes = FALSE) 
    }
  }
  return(pmap)
}


# add_pies ----------
#' Add_pies on map or network for plot.data_agro, plot.data_network
#' @param p network or map ggplot
#' @param n network object from igraph or data frame with coordinates
#' @param format format of n
#' @param plot_type network or map
#' @param data_to_pie data with the variables
#' @param variable variable to display
#' @param pie_size size of the pie
#' @details
#' script adapted from 
#' Pies On A Map, Demonstration script, By QDR : 
#' https://qdrsite.wordpress.com/2016/06/26/pies-on-a-map/
#' 
#' Guangchuang YU code :
#' https://cran.r-project.org/web/packages/ggimage/vignettes/ggimage.html#geom_subview
#' https://github.com/GuangchuangYu/ggimage/blob/master/R/geom_subview.R
#' 
#' @export
#' @importFrom ggimage geom_subview
#' @import plyr
#' @import ggnetwork
#' @import intergraph
#' @import ggplot2
#' @import igraph
#' 
add_pies = function(p, n, format, plot_type, data_to_pie, variable, pie_size){
  
  id_ok = NULL # to avoid no visible binding for global variable ‘id_ok’
  
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
  if( plot_type == "map" ) { 
    colnames(data_to_pie)[which(colnames(data_to_pie) == "location")] = "id_ok" 
    xmin = min(p$coordinates$limits$x); xmax = max(p$coordinates$limits$x)
    ymin = min(p$coordinates$limits$y); ymax = max(p$coordinates$limits$y)
  }
  
  if( plot_type == "network" ){ 
    colnames(data_to_pie)[which(colnames(data_to_pie) == "id")] = "id_ok" 
    xmin = min(p$data$x); xmax = max(p$data$x)
    ymin = min(p$data$y); ymax = max(p$data$y)
  }
  
  # Create a list of ggplot objects. Each one is the pie chart for each site with all labels removed.
  pies <- plyr::dlply(data_to_pie, .(id_ok), function(z){
    z = dplyr::arrange(z, variable)
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
  if( igraph::is_igraph(n) ){
    d = ggnetwork::ggnetwork(n, arrow.gap = 0)
  } else { d = n }
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
  if( plot_type == "map" & format == "data_agro" ){
    v_ok = v_id[which(is.element(v_id, as.character(d$location) ))]
    v_not_ok = v_id[which(!is.element(v_id, as.character(d$location) ))]
  }    
  pies = pies[v_ok]
  
  if( length(v_ok) == 0) { 
    warning("In the data with the variable, no id exist in the data with coordinates and therefore no pies are displayed") 
  }
  if( length(v_ok) < length(v_id) ){ 
    warning("In the data with the variable, the following id does not exist in the data with the coordinates: ", paste(v_not_ok, collapse = ", ")) 
  }
  
  if( plot_type == "map" ){ 
    if( length(v_ok) > 0 ) {
      if( format == "bipart" ) {
        d = ggnetwork::ggnetwork(n, arrow.gap = 0)
        d = d[which(d$type == "location"), c("lat", "long", "vertex.names")]
        colnames(d)[ncol(d)] = "location"
      } 
      if( format == "unipart_location" ){
        d = ggnetwork::ggnetwork(n, arrow.gap = 0)
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
  
  
  if( plot_type == "network" ){ 
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
      # p = p + labs(fill = variable) # not good with factor variables ...
      p = p + ggtitle(variable)
    }
  }
  
  return(p)
}

# format_organo ----------
#' format data for organoleptic analysis
#' @param data data frame
#' @param threshold threshold
#' @details see \code{\link{format_data_PPBstats.data_organo_napping}} and 
#' \code{\link{format_data_PPBstats.data_organo_hedonic}}
#' @export
format_organo = function(data, threshold){
  # 1. Merge and create data frame ----------
  N = data
  N$sample = factor(paste(N$location, N$germplasm, sep = ":"))
  
  # 2. Add the occurence of the different descriptors ----------
  descriptors = as.vector(as.character(N$descriptors))
  vec_adj = unlist(strsplit(descriptors, ";"))
  vec_adj = sort(unique(vec_adj))
  if( length(which(vec_adj == "")) > 0 ) { vec_adj = vec_adj[-which(vec_adj == "")] }
  
  df = matrix(0, ncol = length(vec_adj), nrow = nrow(N))
  df = as.data.frame(df)
  colnames(df) = vec_adj
  out = cbind.data.frame(N, df)
  
  for (i in 1:nrow(out)){
    v_adj = out[i, "descriptors"]
    v_adj = unlist(strsplit(as.character(v_adj), ";"))
    if( length(which(v_adj == "")) > 0 ) { v_adj = v_adj[-which(v_adj == "")] }
    
    for (j in 1:length(v_adj)) {
      e = v_adj[j]
      if (length(e)>0) {
        if (!is.na(e)) { out[i, e] = 1 }
      }
    }
  }
  
  N = out[,-which(colnames(out) == "descriptors")]
  
  # 3. Apply the threshold to keep certain descriptors ----------
  if( !is.null(threshold) ) {
    test = apply(N[, vec_adj], 2, sum)
    to_delete = which(test <= threshold)
    to_keep = which(test > threshold)
    if( length(to_delete) > 0 ) { 
      adj_to_delete = vec_adj[to_delete] 
      N = N[,-which(colnames(N) %in% adj_to_delete)]
      message("The following descriptors have been remove because there were less or equal to ", threshold, " occurences : ", paste(adj_to_delete, collapse = ", "))
      if( ncol(N) == 4 ){ stop("There are no more descriptors with threshold = ", threshold, 
                               ". You must set another value.") }
    }
    vec_adj = vec_adj[to_keep]
  }
  
  
  # 4. Get frequency for each descriptor ----------
  N_freq = N_raw = N
  for (ad in vec_adj) { 
    if( sum(N_raw[, ad], na.rm = TRUE) != 0 ) { 
      N_freq[, ad] = N_raw[, ad] / sum(N_raw[, ad], na.rm = TRUE) 
    }  
  }
  
  return(N_freq)
}

# plot descriptive data ----------
#' Plot agro object from format_data_PPBstats()
#'
#' @description
#' \code{plot_descriptive_data} gets ggplot to describe the data
#' 
#' @param x The data frame. It should come from \code{\link{format_data_PPBstats}}
#' 
#' @param data_version data frame coming from \code{\link{format_data_PPBstats.data_agro_version}}
#' 
#' @param plot_type the type of plot you wish. It can be :
#' \itemize{
#'  \item "pam" for presence abscence matrix that represent the combinaison of germplasm x location
#'  \item "histogramm"
#'  \item "barplot", where sd error are displayed
#'  \item "boxplot"
#'  \item "interaction"
#'  \item "biplot"
#'  \item "radar"
#'  \item "raster"
#'  \item "map"
#' }
#' 
#' @param x_axis factor displayed on the x.axis of a plot. 
#' "date_julian" can be choosen: it will  display julian day for a given variable automatically calculated from format_data_PPBstats(). 
#' This is possible only for plot_type = "histogramm", "barplot", "boxplot" and "interaction".
#' @param in_col factor displayed in color of a plot
#' 
#' @param vec_variables vector of variables to display
#' 
#' @param nb_parameters_per_plot_x_axis the number of parameters per plot on x_axis arguments
#' 
#' @param nb_parameters_per_plot_in_col the number of parameters per plot for in_col arguments
#' 
#' @param labels_on factor to display for plot_type = "biplot"
#' 
#' @param labels_size size of the label for plot_type = "biplot" and "radar"
#' 
#' @param pie_size when plot_type = "map" and vec_variables is not NULL, size of the pie 
#' 
#' @param zoom zoom of the map, see ?get_map for more details
#' 
#' @param ... further arguments passed to or from other methods
#' 
#' @return 
#' \itemize{
#'  \item For plot_type "histogramm", "barplot", "boxplot" or "interaction",
#'  the function returns a list with ggplot objects for each variable of vec_variables.
#'  \item For plot_type "biplot",
#'  the function returns a list with ggplot objects for each pairs of variables of vec_variables. 
#'  \item For plot_type "radar" and "raster,
#'  the function returns a list with ggplot objects with all variables of vec_variables. 
#'  \item For plot_type "map", it returns a map with location 
#'  if vec_variables = NULL and labels_on = "location".
#'  If vec_variables is not NULL, it displays pie on map.
#' }
#' Each list is divided in several lists according to values 
#' of nb_parameters_per_plot_x_axis and nb_parameters_per_plot_in_col except for plot_type = "map".
#' 
#' @author Pierre Riviere
#' 
#' @details 
#' S3 method.
#' 
#' @seealso \code{\link{format_data_PPBstats}}
#' 
#' @export
#' 
#' @import ggplot2
#' @import plyr
#' 
plot_descriptive_data = function(
  x,
  data_version = NULL,
  plot_type = c("pam", "histogramm", "barplot", "boxplot", "interaction", "biplot", "radar", "raster", "map"),
  x_axis = NULL,
  in_col = NULL,
  vec_variables = NULL,
  nb_parameters_per_plot_x_axis = 5,
  nb_parameters_per_plot_in_col = 5,
  labels_on = NULL,
  labels_size = 4,
  pie_size = 0.2,
  zoom = 6, ...
){
  
  data = x
  
  # 1. Error message ----------  
  mess = "plot_type must be \"pam\", \"histogramm\", \"barplot\", \"boxplot\", \"interaction\", \"biplot\", \"radar\", \"raster\" or \"map\"."
  if(length(plot_type) != 1) { stop(mess) }
  if(!is.element(plot_type, c("pam", "histogramm", "barplot", "boxplot", "interaction", "biplot", "radar", "raster", "map"))) { 
    stop(mess) 
  }
  
  if(is.null(vec_variables) & plot_type != "map"){ stop("You must settle vec_variables") }
  
  check_arg = function(x, vec_x) { 
    for(i in x) { 
      if(!is.element(i, vec_x)) { 
        stop("Regarding ", substitute(x),", ", i," is not in data") 
      } 
    } 
  }
  
  if(!is.null(x_axis)){ 
    if( x_axis != "date_julian") { 
      check_arg(x_axis, colnames(data)) 
    } else { 
      warning("x_axis = \"date_julian\" is a special feature that will display julian day for a given variable automatically calculated from format_data_PPBstats().") 
      if(!is.element(plot_type, c("histogramm", "barplot", "boxplot", "interaction"))){ 
        stop("x_axis = \"date_julian\" is possible only for plot_type = \"histogramm\", \"barplot\", \"boxplot\" and \"interaction\".") 
      }
    }
  }
  
  if(!is.null(in_col)){ check_arg(in_col, colnames(data)) }
  check_arg(vec_variables, colnames(data))
  if(!is.null(labels_on)){ check_arg(labels_on, colnames(data)) }
  
  if( plot_type == "pam" & (!is.null(x_axis) | !is.null(in_col)) ){ 
    warning("Note than with plot_type == pam, x_axis and in_col are not used.")
  }
  if( plot_type == "histogramm" & !is.null(x_axis) ){ 
    warning("Note than with plot_type == histogramm, x_axis can not be NULL.")
  }
  if( plot_type == "barplot" & is.null(x_axis) & is.null(data_version) ){ 
    stop("With plot_type == barplot, x_axis can not be NULL.")
  }
  if( plot_type == "boxplot" & is.null(x_axis)  & is.null(data_version) ){ 
    stop("With plot_type == boxplot, x_axis can not be NULL.")
  }
  if( plot_type == "interaction" & (is.null(x_axis) | is.null(in_col)) ){ 
    stop("With plot_type == interaction, x_axis and in_col can not be NULL.")
  }
  if( plot_type == "biplot" & !is.null(x_axis) ){ 
    warning("Note than with plot_type == biplot, x_axis is not used can not be NULL.")
  }
  if( plot_type == "biplot" & length(vec_variables) < 2 ){ 
    stop("With plot_type == biplot, vec_variables must have at least 2 elements.")
  }
  if( plot_type == "biplot" & is.null(labels_on) ){ 
    stop("With plot_type == biplot, labels_on can not be NULL.")
  }
  if( plot_type == "biplot" & length(labels_on) != 1 ){ 
    stop("labels_on must be of length one..")
  }
  if( plot_type == "radar" & length(vec_variables) < 2 ){ 
    stop("With plot_type == radar, vec_variables must have at least 2 elements.")
  }
  if( plot_type == "radar" & is.null(in_col) ){ 
    stop("With plot_type == radar, in_col must not be NULL.")
  }
  if( plot_type == "radar" & !is.null(labels_on) ){ 
    warning("Note that with plot_type == radar, labels_on is not used.")
  }
  
  if( plot_type == "raster" & !is.null(in_col) ){ 
    warning("Note that with plot_type == raster, in_col is not used.")
  }
  if( plot_type == "raster" & !is.null(labels_on) ){ 
    warning("Note that with plot_type == raster, labels_on is not used.")
  }
  
  if( plot_type == "map" ){
    test = unique(is.element(c("lat", "long"), colnames(data)))
    if( length(test) == 2 | !test[1] ){ stop("To display map, you must have columns \"lat\" and \"long\" in your data.") }
  }
  
  if( !is.null(data_version) ){
    if( !is.element(plot_type, c("barplot", "boxplot")) ){ stop("With data_version, only plot_type \"barplot\" and \"boxplot\" are possible.") }
  }
  
  # 2. Functions used in the newt steps ----------
  
  # 2.1. Function to run presence abscence matrix ----------
  fun_pam = function(data, vec_variables){
    
    fun_pam_1 = function(variable, data){
      nb_measures = germplasm = NULL  # to avoid no visible binding for global variable
      
      dtmp = droplevels(na.omit(data[,c("germplasm", "location", "year", variable)]))
      dtmp[,variable] = as.numeric(dtmp[,variable])
      
      xlim = c(min(dtmp[,variable], na.rm = TRUE), max(dtmp[,variable], na.rm = TRUE))
      ylim = c(0,max(dtmp[,variable], na.rm = TRUE))
      m = as.data.frame(with(dtmp, table(germplasm, location, year)))
      m$Freq = as.factor(m$Freq)
      colnames(m)[4] = "nb_measures"
      
      p = ggplot(m, aes(x = germplasm, y = location))
      p = p + geom_raster(aes(fill = nb_measures)) + facet_grid(year ~ .)
      nb_NA = round(length(which(m$nb_measures == 0)) / ( length(which(m$nb_measures == 0)) + length(which(m$nb_measures != 0)) ), 2) * 100
      p = p + ggtitle(
        paste("Presence absence repartition for ", variable, sep = ""),
        paste("(",  nb_NA, "% of 0)", sep = "")
      ) + theme(axis.text.x=element_text(angle=90))
      return(p)
    }
    out = lapply(vec_variables, fun_pam_1, data)
    names(out) = vec_variables
    return(out)
  }
  
  
  # 2.2. Function to run histogramm, barplot, boxplot, interaction ----------
  fun_hbbi_1 = function(d, x_axis, in_col, plot_type, variable, ylim){
    
    d$variable = d[,variable]
    
    # histogramm
    if(plot_type == "histogramm") {
      p = ggplot(d, aes( x = variable))
      if( is.null(in_col) ) { 
        p = p + geom_histogram() 
      } else { 
        p = p + geom_histogram(aes(fill = in_col)) 
      }
    }
    
    # barplot
    if(plot_type == "barplot") {	
      if(is.null(in_col)) {	
        mm2 = plyr::ddply(d, "x_axis", summarise, mean = mean(variable, na.rm = TRUE), sd = sd(variable, na.rm = TRUE))
        p = ggplot(mm2, aes(x = x_axis, y = mean)) + geom_bar(stat = "identity") 
        limits <- aes(ymax = mean + sd, ymin = mean - sd)
        p = p + geom_errorbar(limits, position = position_dodge(width=0.9), width=0.25)
      } else {
        d$toto = paste(d$in_col, d$x_axis, sep = "azerty")
        mm = ddply(d, "toto", summarise, mean = mean(variable, na.rm = TRUE), sd = sd(variable, na.rm = TRUE)) 
        mm$in_col = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[1]}))
        mm$x_axis = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[2]}))
        
        p = ggplot(mm, aes(x = x_axis, y = mean, fill = in_col))
        p = p + geom_bar(position = "dodge", stat = "identity") 
        limits <- aes(ymax = mean + sd, ymin = mean - sd)
        p = p + geom_errorbar(limits, position = position_dodge(width=0.9), width=0.25)
      }
    }
    
    
    # boxplot
    if(plot_type == "boxplot") {
      p = ggplot(d, aes( x = x_axis, y = variable))
      if( is.null(in_col) ) { 
        p = p + geom_boxplot(position="dodge") 
      } else { 
        p = p + geom_boxplot(aes(fill = in_col)) 
      }
    }
    
    # interaction
    if(plot_type == "interaction") {										
      p = ggplot(d, aes(y = variable, x = factor(x_axis), colour = factor(in_col), group = factor(in_col)))
      p = p + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line")
    }
    
    if(is.element(plot_type, c("barplot", "boxplot", "interaction"))) {
      p = p + xlab("") + ylab(variable) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank())	
      p = p + coord_cartesian(xlim = NULL, ylim)
    }
    
    return(p)
  }
  
  fun_hbbi = function(d, vec_variables,
                      x_axis, nb_parameters_per_plot_x_axis, 
                      in_col, nb_parameters_per_plot_in_col, 
                      plot_type){ 
    
    out = lapply(vec_variables, 
                 function(variable, d, labels_on,
                          x_axis, nb_parameters_per_plot_x_axis,
                          in_col, nb_parameters_per_plot_in_col,
                          plot_type){
                   
                   if(!is.null(x_axis)){ 
                     if( x_axis == "date_julian") { x_axis = paste(variable, "$date_julian", sep = "") }
                   }
                   
                   d = reshape_data_split_x_axis_in_col(d, variable, labels_on,
                                                        x_axis, nb_parameters_per_plot_x_axis,
                                                        in_col, nb_parameters_per_plot_in_col
                   )
                   ylim = range(unlist(lapply(d, function(x){ range(x[,variable], na.omit = TRUE) } )))
                   
                   out = lapply(d, fun_hbbi_1, x_axis, in_col, plot_type, variable, ylim)
                   return(out)
                 },
                 d, labels_on,
                 x_axis, nb_parameters_per_plot_x_axis,
                 in_col, nb_parameters_per_plot_in_col,
                 plot_type
    )
    names(out) = vec_variables
    return(out)
  }
  
  
  # 2.3. Function to run biplot ----------
  fun_biplot = function(d, vec_variables, labels_on, labels_size,
                        x_axis, nb_parameters_per_plot_x_axis, 
                        in_col, nb_parameters_per_plot_in_col
  ){
    labels_text = NULL  # to avoid no visible binding for global variable
    
    d = reshape_data_split_x_axis_in_col(d, vec_variables, labels_on,
                                         x_axis, nb_parameters_per_plot_x_axis, 
                                         in_col, nb_parameters_per_plot_in_col
    )
    
    ylim = NULL
    for(variable in vec_variables){
      ylim = c(ylim, list(
        range(unlist(lapply(d, function(x){ range(x[,variable], na.omit = TRUE) } )))
      )
      )
    }
    names(ylim) = vec_variables
    
    fun_biplot_1 = function(pair_var, d, in_col, labels_size, ylim){
      fun_biplot_2 = function(d, pair_var, in_col, labels_size, ylim){
        var_ = unlist(strsplit(pair_var, " -azerty- "))
        var1 = var_[1]; d$var1 = d[,var1]
        var2 = var_[2]; d$var2 = d[,var2]
        ylim = range(unlist(ylim[c(var_[1], var_[2])]))
        if(!is.null(in_col)){ 
          dtmp = d[,c("in_col", "var1", "var2", "labels_text")] 
        } else {
          dtmp = d[,c("var1", "var2", "labels_text")] 
        }
        dtmp = na.omit(dtmp)
        if( nrow(dtmp) == 0){
          warning("No biplot is done for ", var1, " and ", var2, " as there are only NA. This can be due to missing data."); 
          p = NULL
        } else {
          p = ggplot(dtmp, aes(x = var1, y = var2, label = labels_text)) 
          if(!is.null(in_col)){
            p = p + geom_text(aes(colour = in_col), size = labels_size)             
          } else {
            p = p + geom_text(size = labels_size) 
          }
          p = p + coord_cartesian(xlim = NULL, ylim = ylim)
          p = p + stat_smooth(method = "lm", se = FALSE)
          p = p  + xlab(var1) + ylab(var2) + theme(axis.text.x = element_text(angle=90, hjust=1), legend.title = element_blank()) 
          
          m <- lm(var2 ~ var1, dtmp)
          eq = paste("y = ", format(coef(m)[1], digits = 2), " x +", format(coef(m)[2], digits = 2), "; r2 = ", format(summary(m)$r.squared, digits = 3), sep = "")
          p = p + ggtitle(eq) + theme(plot.title = element_text(hjust = 0.5))
        }
        return(p)
      }
      p = lapply(d, fun_biplot_2, pair_var, in_col, labels_size, ylim)
      return(p)
    }
    
    pair_var = apply(combn(vec_variables, 2), 2, function(x){paste(x, collapse = " -azerty- ")})
    out = lapply(pair_var, fun_biplot_1, d, in_col, labels_size, ylim)
    names(out) = sub(" -azerty- ", " - ", pair_var)
    return(out)
  }
  
  # 2.4. Function to run radar ----------
  fun_radar = function(d, vec_variables, in_col, labels_size){
    d$group = d[,in_col]
    
    m = data.frame(matrix(levels(d$group), ncol = 1))
    for(variable in vec_variables){
      value = tapply(d[,variable], d$group, mean, na.rm = TRUE)
      # rescale all variables to lie between 0 and 1
      value_ok = value / sum(value, na.rm = TRUE)
      m = cbind.data.frame(m, value_ok)
    }
    colnames(m) = c("group", vec_variables)
    p = ggradar(m, 
                grid.label.size = labels_size, 
                axis.label.size = labels_size,
                group.point.size = labels_size,
                legend.text.size = labels_size*2.5,
                group.line.width= labels_size/4)
    p = p + theme(legend.title = element_blank()) 
    return(p)
  }
  
  # 2.5. Function to run raster representation for factor variables ----------
  fun_raster_1 = function(data, vec_variable){
    variable = value = NULL  # to avoid no visible binding for global variable
    
    vv = vm = vx = NULL
    for(v in vec_variables) { 
      vv = c(vv, as.character(rep(v, nrow(data))))
      vm = c(vm, as.character(data[,v]))
      vx = c(vx, as.character(data$x_axis))
    }
    
    dtmp = cbind.data.frame(
      variable = as.factor(vv),
      value = as.factor(vm),
      x_axis = as.factor(vx)
    )
    
    p = ggplot(dtmp, aes(x = x_axis, y = variable))
    p = p + geom_raster(aes(fill = value))
    p = p + theme(axis.text.x=element_text(angle=90))
    return(p)
  }
  
  fun_raster = function(d, vec_variables,
                        x_axis, nb_parameters_per_plot_x_axis){ 
    vec_variable = NULL  # to avoid no visible binding for global variable
    
    vv = vm = vx = NULL
    for(v in vec_variables) { 
      vv = c(vv, as.character(rep(v, nrow(d))))
      vm = c(vm, as.character(d[,v]))
      vx = c(vx, as.character(d[,x_axis]))
    }
    
    dtmp = cbind.data.frame(
      variable = as.factor(vv),
      value = as.factor(vm),
      x_axis = as.factor(vx)
    )
    
    test = table(dtmp$x_axis)
    if( sum(test) != length(test) ) { 
      warning("There are no single value for each x_axis, therefore block, X and Y colums have been added in order to have single value.") 
      d$new_x_axis = paste(d[,x_axis], d$block, d$X, d$Y, sep = "-")
    } else { d$new_x_axis = d[,x_axis] }
    d = d[,-which(colnames(d) == x_axis)]
    x_axis = paste(x_axis, "block", "X", "Y", sep = "-")
    colnames(d)[which(colnames(d) == "new_x_axis")] = x_axis
    
    d = reshape_data_split_x_axis_in_col(d, vec_variables, labels_on = NULL,
                                         x_axis, nb_parameters_per_plot_x_axis,
                                         in_col = NULL, nb_parameters_per_plot_in_col = NULL
    )
    out = lapply(d, fun_raster_1, vec_variable)
    return(out)
  }
  
  # 2.6. Functions to run map ----------
  fun_pies_on_map = function(variable, p, data, pie_size){
    data_to_map = droplevels(unique(data[c("location", "long", "lat")]))
    p = add_pies(p, data_to_map, format = "data_agro", plot_type = "map", data, variable, pie_size)
    return(p) 
  }
  
  fun_map = function(data, vec_variables, labels_on, labels_size, pie_size){
    data_to_map = droplevels(unique(data[c("location", "long", "lat")]))
    p = pmap(data_to_map, format = NULL, labels_on, labels_size, zoom) 
    if( !is.null(vec_variables) ){
      out = lapply(vec_variables, fun_pies_on_map, p, data, pie_size)
      names(out) = paste("pies_on_map", vec_variables, sep="_")
    } else { out = list(p); names(out) = "map" }
    return(out)
  }
  
  # 2.7. Function to run data_version ----------
  fun_data_version_1 = function(variable, data, data_version, plot_type){
    id_azerty = group = NULL  # to avoid no visible binding for global variable
    
    data_version_class = class(data_version)[2]
    
    factor_to_split = "location" # default value
    
    if( data_version_class == "data_agro_version_SR" ){
      data_version$lg = paste(data_version$location, data_version$germplasm, sep = ":")
      factor_to_split = "lg"
      data_version$group_bis = paste(data_version$germplasm, data_version$group, sep = "-")
    }
    
    if( data_version_class == "data_agro_version_HA" ){
      data_version$group_bis =  paste("sown at", data_version$location, ", coming from", data_version$group)
      factor_to_split = "germplasm"
    }
    
    if( data_version_class == "data_agro_version_LF" ){
      data_version$group_bis = paste("sown at", data_version$location, ", coming from", data_version$group)
      factor_to_split = "location"
    }
    
    colnames(data)[which(colnames(data) == variable)] = "variable"
    data$id_azerty = paste(data$location, data$year, data$germplasm, sep = "-")
    data_version$id_azerty = paste(data_version$location, data_version$year, data_version$germplasm, sep = "-")

    if( data_version_class == "data_agro_version_SR" ){ 
      # get row where id is present in both data set
      t1 = is.element(data_version$id_azerty, data$id_azerty)
      id_ok = data_version$id_azerty[t1]
      id_not_ok = data_version$id_azerty[!t1]
    }
    
    
    if( data_version_class == "data_agro_version_HA" | data_version_class == "data_agro_version_LF" ){ 
      # get row where id is present in both data set
      t1 = is.element(data_version$id_azerty, data$id_azerty)
      # get rid of row where group (location of origin) is not present in the data set
      t2 = is.element(data_version[t1,]$group, data_version[t1,]$location)
      id_ok = data_version[t1,]$id_azerty[t2]
      id_not_ok = data_version[t2,]$id_azerty[!t2]
    }
      
    
    if( length(id_not_ok) > 0 ) { 
      warning("The following rows are not taken into account in data_version: ", paste(id_not_ok, collape = ", ")) 
      }
    if( length(id_not_ok) == length(t) ) { stop("There is not match between data_version and data. Not plot can be done.") }
    data_version = droplevels(dplyr::filter(data_version, id_azerty %in% id_ok))
    d = plyr::join(data_version, data, by = "id_azerty")
    d = droplevels(na.omit(d))
    
    if( data_version_class == "data_agro_version_HA" | data_version_class == "data_agro_version_LF" ){ 
      # single plot with version for all germplasm/location merged
      p = ggplot(d, aes(x = version, y = variable)) 
      p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
      if( plot_type == "barplot"){ p = p + geom_bar(stat = "identity", position = "dodge") }
      if( plot_type == "boxplot"){ p = p + geom_boxplot(position = "dodge") }
      p1 = p
      
      # single plot with version for each germplasm/location
      if( data_version_class == "data_agro_version_HA" ){ p = ggplot(d, aes(x = germplasm, y = variable)) }
      if( data_version_class == "data_agro_version_LF" ){ p = ggplot(d, aes(x = location, y = variable)) } 
      p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
      if( plot_type == "barplot"){
        p = p + geom_bar(aes(fill = version), stat = "identity", position = "dodge")      
      }
      if( plot_type == "boxplot"){
        p = p + geom_boxplot(aes(fill = version), position = "dodge")      
      }
      p2 = p
      }
    
    # plot for each germplasm/location with all version separated
    colnames(d)[which(colnames(d) == factor_to_split)] = "factor_to_split"
    dd = plyr:::splitter_d(d, .(factor_to_split))
    out = lapply(dd, function(x){
      p = ggplot(x, aes(x = group_bis, y = variable))
      p = p + ggtitle(x[1, "factor_to_split"]) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      p = p + xlab("")
      if( plot_type == "barplot"){
        p = p + geom_bar(aes(fill = version), stat = "identity", position = "dodge")      
      }
      if( plot_type == "boxplot"){
        p = p + geom_boxplot(aes(fill = version), position = "dodge")      
      }
      return(p)  
    })
    
    if( data_version_class == "data_agro_version_SR" ){ 
      out = out
    } 
    
    if( data_version_class == "data_agro_version_HA" ){ 
      out = list("home_away_merged" = p1, 
                 "home_away_merged_per_germplasm" = p2, 
                 "home_away_per_germplasm" = out)
    }
    
    if( data_version_class == "data_agro_version_LF" ){ 
      out = list("local_foreign_merged" = p1, 
                 "local_foreign_merged_per_location" = p2, 
                 "local_foreign_per_location" = out)
    } 
    
    
    return(out)
  }
  
  fun_data_version = function(vec_variables, data, data_version, plot_type){
    out = lapply(vec_variables, fun_data_version_1, data, data_version, plot_type)
    names(out) = vec_variables
    return(out)
  }
  
  
  # 3. Run code ----------
  # 3.1. Presence absence for each germplasm, location and year
  if(plot_type == "pam"){ 
    p_out = fun_pam(data, vec_variables) 
  }
  
  # 3.2. histogramm, barplot, boxplot, interaction ----------
  if( is.element(plot_type, c("histogramm", "barplot", "boxplot", "interaction") )) { 
    p_out = fun_hbbi(data, vec_variables,
                     x_axis, nb_parameters_per_plot_x_axis, 
                     in_col, nb_parameters_per_plot_in_col, 
                     plot_type)  
  }
  
  # 3.3. biplot ----------
  if(plot_type == "biplot") {
    p_out = fun_biplot(data, vec_variables, labels_on, labels_size,
                       x_axis, nb_parameters_per_plot_x_axis, 
                       in_col, nb_parameters_per_plot_in_col)
  }
  
  # 3.4. radar ----------
  if(plot_type == "radar") {
    p_out = fun_radar(data, vec_variables, in_col, labels_size)
  }
  
  # 3.5. raster ----------
  if(plot_type == "raster") {
    p_out = fun_raster(data, vec_variables, x_axis, nb_parameters_per_plot_x_axis)
  } 
  
  # 3.6. map ------------
  if(plot_type == "map"){
    p_out = fun_map(data, vec_variables, labels_on, labels_size, pie_size)
  }  
  
  # 3.7. data_version ----------
  if( !is.null(data_version) ){
    p_out = fun_data_version(vec_variables, data, data_version, plot_type)
  }  
  
  
  # 4. Return results ----------
  return(p_out)
}

# transform home away data to local foreign data
#' transform home away data to local foreign data
#' @param data_version_HA data home away
#' @details change home to local and away to foreign
#' @return a data frame of class data_version_LF
#' @importFrom methods is
#' @export
#' 
HA_to_LF = function(data_version_HA){
  if(!is(data_version_HA, "data_agro_version_HA")){ stop(substitute(data_version_HA), " must be formated with type = \"data_agro_version\" with home away format, see PPBstats::format_data_PPBstats().") }
  is(data_version_HA)
  data_version_LF = data_version_HA
  data_version_LF$version = as.character(data_version_LF$version)
  data_version_LF$version[which(data_version_LF$version == "home")] = "local"
  data_version_LF$version[which(data_version_LF$version == "away")] = "foreign"
  data_version_LF$version = as.factor(data_version_LF$version)
  class(data_version_LF) = c("PPBstats", "data_agro_version_LF", "data.frame")
  return(data_version_LF)
}


