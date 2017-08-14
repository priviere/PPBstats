#' Some functions used in several functions of PPBstats
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
  
  dimvar = round(res.pca$eig$`percentage of variance`[1:2], 1)
  
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



