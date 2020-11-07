#' Check and format the data to be used by PPBstats functions for data agro HA to study local adaptation.
#'
#' @description
#' \code{format_data_PPBstats.data_agro_HA} checks and formats the data to be used by PPBstats functions for agro HA to study local adpatation
#'  
#' @param data The data frame to format.
#'  It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "origin", "version", "..."), 
#'  with "..." the variables.
#'  The variables can be linked to their corresponding dates. 
#'  The dates are associated to their corresponding variable by $.
#'  For example the date associated to variable y1 is y1$date.
#'  The date must have format year-month-day, e.g. 2017-12-05
#' 
#' @details 
#' \itemize{
#'  \item origin: the location where the germplasm come from
#'  \item version: away or home
#' }
#' 
#' More details can be found in the book \href{https://priviere.github.io/PPBstats_book/home-away.html}{here}.
#'     
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{format_data_PPBstats}}
#' 
#' @export
#' 
format_data_PPBstats.data_agro_HA = function(data){
  
  d = suppressMessages(format_data_PPBstats(data, type = "data_agro"))
  
  mess = "The following column are compulsory : \"origin\", \"version\"."
  # check columns
  if(!is.element("origin", colnames(d))) { stop(mess) }
  if(!is.element("version", colnames(d))) { stop(mess) }
  
  mess = "The following column must be set as factor : \"origin\", \"version\"."
  if(!is.factor(d$origin)) { stop(mess) }
  if(!is.factor(d$version)) { stop(mess) }
  
  test = which(!is.element(na.omit(d$version), c("home", "away")))
  if( length(test) > 0 ) { stop("In colum version, only \"home\" and \"away\" are allowed, which is not the case for the following rows:", paste(test, collapse = ",")) }
  
  class(d) <- c("PPBstats", "data_agro_HA", "data.frame")
  
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}

