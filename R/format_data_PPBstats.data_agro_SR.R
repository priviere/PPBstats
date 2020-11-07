#' Check and format the data to be used by PPBstats functions for data agro SR to study response to selection.
#'
#' @description
#' \code{format_data_PPBstats.data_agro_SR} checks and formats the data to be used by PPBstats functions for agro SR to study response to selection
#' 
#' @param data The data frame to format.
#'  It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "expe_id", "group", "version", "..."), 
#'  with "..." the variables.
#'  The variables can be linked to their corresponding dates. 
#'  The dates are associated to their corresponding variable by $.
#'  For example the date associated to variable y1 is y1$date.
#'  The date must have format year-month-day, e.g. 2017-12-05
#' 
#' @details 
#' \itemize{
#'  \item expe_id: an id with
#'   \itemize{
#'    \item S couple and R couple (i.e. four rows) OR
#'    \item S couple (i.e. two rows) OR
#'    \item R couple (i.e. two rows)
#'   }
#'  \item group: S or R
#'  \item version: bouquet or vrac
#' }
#' 
#' expe_id is useful for example if there are several selection in one germplasm or if there are several origin for a given germplasm
#' 
#' More details explaining these two particular cases can be found in the book \href{https://priviere.github.io/PPBstats_book/response-to-selection.html}{here}.
#'     
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{format_data_PPBstats}}
#' 
#' @export
#' 
format_data_PPBstats.data_agro_SR = function(data){
  
  d = suppressMessages(format_data_PPBstats(data, type = "data_agro"))
  
  mess = "The following column are compulsory : \"expe_id\", \"group\", \"version\"."
  # check columns
  if(!is.element("expe_id", colnames(d))) { stop(mess) }
  if(!is.element("group", colnames(d))) { stop(mess) }
  if(!is.element("version", colnames(d))) { stop(mess) }
  
  mess = "The following column must be set as factor : \"expe_id\", \"group\", \"version\"."
  if(!is.factor(d$expe_id)) { stop(mess) }
  if(!is.factor(d$group)) { stop(mess) }
  if(!is.factor(d$version)) { stop(mess) }
  
  test = which(!is.element(na.omit(d$group), c("S", "R")))
  if( length(test) > 0 ) { stop("In colum group, only \"S\" and \"R\" are allowed, which is not the case for the following rows:", paste(test, collapse = ",")) }
  
  test = which(!is.element(na.omit(d$version), c("bouquet", "vrac")))
  if( length(test) > 0 ) { stop("In colum version, only \"bouquet\" and \"vrac\" are allowed, which is not the case for the following rows:", paste(test, collapse = ",")) }
  
  class(d) <- c("PPBstats", "data_agro_SR", "data.frame")
  
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}

