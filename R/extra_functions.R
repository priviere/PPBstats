# 0. help -----------------------------------------------------------------
#' Some functions used in several functions of PPBstats
#' 
#' @name extra_functions
#' 
#' @description
#' This file group several functions used in several functions of PPBstats
#' 
#' @author Pierre Riviere
#' 

# Function use in describe_data.R, GxE.R
check_data_vec_variables = function(data, vec_variables){
  mess = "The following column are compulsory : c(\"year\", \"germplasm\", \"location\", \"block\", \"X\", \"Y\"."
  if(!is.element("year", colnames(data))) { stop(mess) }
  if(!is.element("germplasm", colnames(data))) { stop(mess) }
  if(!is.element("location", colnames(data))) { stop(mess) }
  if(!is.element("block", colnames(data))) { stop(mess) }
  if(!is.element("X", colnames(data))) { stop(mess) }
  if(!is.element("Y", colnames(data))) { stop(mess) }
  
  for(variable in vec_variables) { if(!is.element(variable, colnames(data))) { stop(variable," is not in data") } }
}

# Function use in describe_data.R
split_data_for_ggplot = function(data, factor, nb_param){
  ns = unique(data[,factor])
  s = rep(c(1:length(ns)), each = nb_param)[1:length(ns)]
  names(s) = ns
  data$split_factor = s[data[,factor]]
  data_f =  plyr:::splitter_d(data, .(split_factor))
  return(data_f)
}

# Function use in which_won_where.R, mean_vs_stability.R
get_biplot = function(res.pca){
  
  var = as.data.frame(res.pca$var$coord)
  var = cbind.data.frame(rownames(var), var, color = "darkgreen"); colnames(var)[1:3] = c("label", "x", "y")
  
  ind = as.data.frame(res.pca$ind$coord)
  ind = cbind.data.frame(rownames(ind), ind, color = "black"); colnames(ind)[1:3] = c("label", "x", "y")
  
  r <- min((max(ind[, "x"]) - min(ind[, "x"])/(max(var[, "x"]) - min(var[, "x"]))), (max(ind[, "y"]) - min(ind[, "y"])/(max(var[, "y"]) - min(var[, "y"]))))
  var[, c("x", "y")] <- var[, c("x", "y")] * r * 0.7 # taken from factoextra::fviz_pca_biplot
  
  vi = rbind.data.frame(var, ind)
  
  dimvar = round(res.pca$eig$`percentage of variance`[1:2], 1)
  
  p = ggplot(data = vi, aes(x = x, y = y, label = label)) + geom_text(color = as.character(vi$color)) + geom_point(color = as.character(vi$color))
  p = p + xlab(paste("Dim 1 (", dimvar[1], "%)", sep = "")) + ylab(paste("Dim 2 (", dimvar[2], "%)", sep = ""))
  p = p + ggtitle("Biplot germplasm and environments")
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
  if(longer & x4 > 0){
    y4 = y4/x4 * x4*2
    x4 = x4*2
  }
  
  return(c(x1 = x3, y1 = y3, x2 = x4, y2 = y4))
}

