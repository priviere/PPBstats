#' Check and format the data to be used in PPBstats functions
#'
#' @description
#' \code{format_data_PPBstats} checks and formats the data to be used in PPBstats functions
#' 
#' @param data The data frame. See details.
#' 
#' @param type type of format : "data_network", "data_agro" or "data_agro_version". See details.
#' 
#' @details 
#' \itemize{
#'  \item For type = "data_network" :
#'  \item For type = "data_agro" : 
#'  It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "..."), with "..." the variables.
#'  The variables can be linked to their corresponding dates. 
#'  The dates are associated to their corresponding variable by $.
#'  For example the date associated to variable y1 is y1$date.
#'  \item For type = "data_agro_version" : 
#'  It should have the following columns: c("year", "germplasm", "location", "group", "version").
#'  The group refers to an id that contains two different versions.
#'  For example for group 1, there is version 1 and 2. 
#'  See data(data_version) for an example.
#' }
#' 
#' 
#' @return 
#' The function returns the data with the right format (i.e. class)
#' 
#' @author Pierre Riviere
#' 
format_data_PPBstats = function(
  data, 
  type = "data_agro"
  )
  {
  match.arg(type, c("data_network", "data_agro_version", "data_agro"))
  
  d = data
  
  if(type == "data_agro"){
    mess = "The following column are compulsory : c(\"location\", \"year\", \"germplasm\", \"block\", \"X\", \"Y\"."
    if(!is.element("location", colnames(d))) { stop(mess) }
    if(!is.element("year", colnames(d))) { stop(mess) }
    if(!is.element("germplasm", colnames(d))) { stop(mess) }
    if(!is.element("block", colnames(d))) { stop(mess) }
    if(!is.element("X", colnames(d))) { stop(mess) }
    if(!is.element("Y", colnames(d))) { stop(mess) }
    
    mess = "The following column must be set as factor : c(\"location\", \"year\", \"germplasm\", \"block\", \"X\", \"Y\"."
    if(!is.factor(d$location)) { stop(mess) }
    if(!is.factor(d$year)) { stop(mess) }
    if(!is.factor(d$germplasm)) { stop(mess) }
    if(!is.factor(d$block)) { stop(mess) }
    if(!is.factor(d$X)) { stop(mess) }
    if(!is.factor(d$Y)) { stop(mess) }
    
    class(d) <- c("PPBstats", "data.frame", "data_agro")
    message(substitute(data), " has been formated for PPBstats functions.")
  }
  
  if(type == "data_agro_version"){
    mess = "To do !!!."
  }
  
  return(d)
}

