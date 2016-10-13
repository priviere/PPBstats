# 0. help -----------------------------------------------------------------
#' Some functions used in several functions of PPBstats
#' 
#' @name extra_functions
#' 
#' @description
#' This file group several functions used in several functions of PPBstats:
#' \itemize{
#' 	\item describe_data
#' }
#' 
#' @author Pierre Riviere
#' 

# describe_data
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

# describe_data
split_data_for_ggplot = function(data, factor, nb_param){
  ns = unique(data[,factor])
  s = rep(c(1:length(ns)), each = nb_param)[1:length(ns)]
  names(s) = ns
  data$split_factor = s[data[,factor]]
  data_f =  plyr:::splitter_d(data, .(split_factor))
  return(data_f)
}



