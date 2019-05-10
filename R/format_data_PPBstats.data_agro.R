#' Check and format the data to be used by PPBstats functions for data version in agronomic analyses
#'
#' @description
#' \code{format_data_PPBstats.data_agro} checks and formats the data to be used by PPBstats functions for data version in agronomic analyses
#' 
#' @param data The data frame to format.
#'  It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "..."), with "..." the variables.
#'  The variables can be linked to their corresponding dates. 
#'  The dates are associated to their corresponding variable by $.
#'  For example the date associated to variable y1 is y1$date.
#'  The date must have format year-month-day, e.g. 2017-12-05
#'  
#' @author Pierre Riviere
#' 
#' @details 
#' See the book for more details \href{https://priviere.github.io/PPBstats_book/intro-agro.html#data-agro}{here}.
#' 
#' @seealso \code{\link{format_data_PPBstats}}
#' 
#' @export
#' 
format_data_PPBstats.data_agro = function(data){
  d = data
  
  mess = "The following column are compulsory : c(\"seed_lot\", \"location\", \"year\", \"germplasm\", \"block\", \"X\", \"Y\"."
  # check columns
  # Factors compulsory
  if(!is.element("seed_lot", colnames(d))) { stop(mess) }
  if(!is.element("location", colnames(d))) { stop(mess) }
  if(!is.element("year", colnames(d))) { stop(mess) }
  if(!is.element("germplasm", colnames(d))) { stop(mess) }
  if(!is.element("block", colnames(d))) { stop(mess) }
  if(!is.element("X", colnames(d))) { stop(mess) }
  if(!is.element("Y", colnames(d))) { stop(mess) }
  
  mess = "The following column must be set as factor : c(\"seed_lot\", \"location\", \"year\", \"germplasm\", \"block\", \"X\", \"Y\"."
  if(!is.factor(d$seed_lot)) { stop(mess) }
  if(!is.factor(d$location)) { stop(mess) }
  if(!is.factor(d$year)) { stop(mess) }
  if(!is.factor(d$germplasm)) { stop(mess) }
  if(!is.factor(d$block)) { stop(mess) }
  if(!is.factor(d$X)) { stop(mess) }
  if(!is.factor(d$Y)) { stop(mess) }
  
  # Variable in option to get map
  vec_variables = c("long", "lat")
  for(i in vec_variables) {
    if(!is.element(i, colnames(d))) { 
      warning("Column \"", i, "\" is needed to get map and not present in data.") 
    }
  }
  
  # check and format date
  vec_date = grep("\\$date", colnames(d))
  if( length(vec_date) == 0 ) { vec_date = NULL }
  
  if(!is.null(vec_date)){

    for(i in 1:length(vec_date)){
      # check date
      v = na.omit(d[, vec_date[i]])
      if( length(v) > 0 ){
        for(j in 1:length(v)){
          t = try(format(as.Date(as.character(v[j])), format = "%Y-%m-%d", origin = "1900-01-01"), silent = TRUE)
          if( class(t) == "try-error" || is.na(t) ) stop( 
            "Date must be at format year-month-day for row for column", i, "."
          )
        }
      }
      
      # format date
      v = format(as.Date(d[, vec_date[i]]), format = "%Y/%m/%d")
      vjd = sapply(v, function(x) {
        if(is.na(x)){
          x
        } else {
          julian(as.Date(x), origin = as.Date(paste(unlist(strsplit(x, "/"))[1], "/01/01", sep = "")))
        }
      })
      d = cbind.data.frame(d, vjd)
      colnames(d)[ncol(d)] = paste(colnames(d)[vec_date[i]], "_julian", sep = "")
    }
  }
  
  class(d) <- c("PPBstats", "data_agro", "data.frame")
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}