#' Check and format the data to be used by PPBstats functions for hedonic analyses
#'
#' @description
#' \code{format_data_PPBstats.data_organo_hedonic} checks and formats the data to be used by PPBstats functions for hedonic analyses
#' 
#' @param data The data frame with the following columns : sample, juges, note, descriptors, germplasm, location. 
#' The descriptors must be separated by ";". Any other column can be added as supplementary variables.
#' 
#' @param threshold number of occurence of descriptors <= threshold are kept
#' 
#' @details
#' See the book for more details \href{https://priviere.github.io/PPBstats_book/hedonic.html#format-the-data-8}{here}.
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{format_data_PPBstats}}
#' 
#' @export
#' 
format_data_PPBstats.data_organo_hedonic = function(data, threshold){

  mess = "In data, the following column are compulsory : \"sample\", \"germplasm\", \"location\", \"juges\", \"note\", \"descriptors\"."
  if(!is.element("sample", colnames(data))) { stop(mess) }
  if(!is.element("germplasm", colnames(data))) { stop(mess) }
  if(!is.element("location", colnames(data))) { stop(mess) }
  if(!is.element("juges", colnames(data))) { stop(mess) }
  if(!is.element("note", colnames(data))) { stop(mess) }
  if(!is.element("descriptors", colnames(data))) { stop(mess) }
  
  if(!is.factor(data$sample)) { stop("sample must be factor") }
  if(!is.factor(data$germplasm)) { stop("germplasm must be factor") }
  if(!is.factor(data$location)) { stop("location must be factor") }
  if(!is.factor(data$juges)) { stop("juges must be factor") }
  if(!is.numeric(data$note)) { stop("note must be numeric") }
  if(!is.factor(data$descriptors)) { stop("descriptors must be factor") }
  
  remove_row_with_no_descriptors = which(data$descriptors == "")
  if( length(remove_row_with_no_descriptors) > 0 ) { 
    d_ok = data[-remove_row_with_no_descriptors,]
    warning("The following row in data have been remove because there are no descriptors :", paste(remove_row_with_no_descriptors, collapse = ", "))
  } else { d_ok = data}
  
  var_sup = colnames(data)[-which(colnames(data) == "descriptors")]
  d = format_organo(d_ok, threshold)
  descriptors = colnames(d)[!is.element(colnames(d), var_sup)]
  
  d = list("data" = d, 
           "var_sup" = var_sup,
           "descriptors" = descriptors
           )
  
  class(d) <- c("PPBstats", "data_organo_hedonic")
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}

  
  
