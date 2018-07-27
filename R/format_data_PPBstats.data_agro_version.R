#' Check and format the data to be used by PPBstats functions for data version in agronomic analyses
#'
#' @description
#' \code{format_data_PPBstats} checks and formats the data to be used by PPBstats functions for data version in agronomic analyses
#' 
#' @param data The data frame to format
#'  It should have the following columns: c("year", "germplasm", "location", "group", "version").
#'  The group refers to an id that contains two different versions.
#'  For example for group 1, there are version 1 and 2.
#' 
#' @details 
#' Regarding response to selection, data_agro_version must have the following format:
#' \itemize{
#'  \item In group there are either S or R.
#'  \item In version there are either bouquet or vrac
#' }
#'  
#' Regarding study "home away", data_agro_version must have the following format:
#' \itemize{
#'  \item In group there are same type of value than in column location.
#'  \item In version there are either migrant or residant
#' }
#' 
#' More details explaining these two particular cases can be found in the book \href{https://priviere.github.io/PPBstats_book/family-4.html}{here}.
#'     
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{format_data_PPBstats}}
#' 
#' @export
#' 
format_data_PPBstats.data_agro_version = function(data){
  d = data
  
  mess = "The following column are compulsory : \"year\", \"germplasm\", \"location\", \"group\", \"version\"."
  # check columns
  if(!is.element("year", colnames(d))) { stop(mess) }
  if(!is.element("germplasm", colnames(d))) { stop(mess) }
  if(!is.element("location", colnames(d))) { stop(mess) }
  if(!is.element("group", colnames(d))) { stop(mess) }
  if(!is.element("version", colnames(d))) { stop(mess) }
  
  mess = "The following column must be set as factor : \"year\", \"germplasm\", \"location\", \"group\", \"version\"."
  if(!is.factor(d$year)) { stop(mess) }
  if(!is.factor(d$germplasm)) { stop(mess) }
  if(!is.factor(d$location)) { stop(mess) }
  if(!is.factor(d$group)) { stop(mess) }
  if(!is.factor(d$version)) { stop(mess) }
  
  test_1 = length(which(is.element(c("S", "R"), d$group)))
  test_2 = length(which(is.element(c("bouquet", "vrac"), d$version)))
  test_3 = length(which(is.element(d$location, d$group)))
  test_4 = length(which(is.element(c("residant", "migrant"), d$version)))
  
  if( test_1 > 0 & test_2 > 0) {   class(d) <- c("PPBstats", "data_agro_version_SR", "data.frame") }
  if( test_3 > 0 & test_4 > 0) {   class(d) <- c("PPBstats", "data_agro_version_MR", "data.frame") }
  if( test_1 == 0 & test_2 == 0 & test_3 ==0 & test_4 == 0) { class(d) <- c("PPBstats", "data_agro_version", "data.frame") }
  
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}

