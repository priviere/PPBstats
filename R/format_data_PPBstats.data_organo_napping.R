#' Check and format the data to be used by PPBstats functions for napping analyses
#'
#' @description
#' \code{format_data_PPBstats.data_organo_napping} checks and formats the data to be used by PPBstats functions for napping analyses
#' 
#' @param data The data frame with the following columns: sample, juges, X, Y, descriptors, germplasm, location.
#' The descriptors must be separated by ";"
#' 
#' @param threshold number of occurence of descriptors <= threshold are kept
#' 
#' @details
#' See the book for more details \href{https://priviere.github.io/PPBstats_book/napping.html#format-the-data-9}{here}.
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{format_data_PPBstats}}
#' 
#' @export
#' 
format_data_PPBstats.data_organo_napping = function(data, threshold){
  
  mess = "In data, the following column are compulsory : \"juges\", \"X\", \"Y\", \"descriptors\", \"germplasm\", \"location\"."
  if(!is.element("juges", colnames(data))) { stop(mess) }
  if(!is.element("X", colnames(data))) { stop(mess) }
  if(!is.element("Y", colnames(data))) { stop(mess) }
  if(!is.element("descriptors", colnames(data))) { stop(mess) }
  if(!is.element("germplasm", colnames(data))) { stop(mess) }
  if(!is.element("location", colnames(data))) { stop(mess) }
  
  if( !is.factor(data$juges) ) { stop("juge must be a factor") }
  if( !is.numeric(data$X) ) { stop("X must be a numeric") }
  if( !is.numeric(data$Y) ) { stop("Y must be a numeric") }
  if( !is.factor(data$descriptors) ) { stop("descriptors must be a factor") }
  if( !is.factor(data$germplasm) ) { stop("germplasm must be a factor") }
  if( !is.factor(data$location) ) { stop("location must be a factor") }
  
  N = format_organo(data, threshold)
  N = N[,c(6, 1, 2, 3, c(7:ncol(N)))]
  descriptors = colnames(N)[c(7:ncol(N))]
  
  # Get table with, for each judge, the X and Y for each sample tasted ----------
  juges = levels(N$juges)
  f = nlevels(N$sample)
  d_juges = as.data.frame(levels(N$sample))
  colnames(d_juges) = "sample"
  nb = c(1:length(juges)); names(nb) = juges
  juges_to_delete = NULL
  
  for (i in juges){
    dtmp = droplevels(subset(N, juges == i))
    sample = dtmp$sample
    if(length(sample) < length(d_juges$sample)) {
      warning("juges ", i, " is not taken into account because he did not taste all the samples.")
      juges_to_delete = c(juges_to_delete, i)
    } else {
      Xi = dtmp$X
      Yi = dtmp$Y
      d_juges_tmp = cbind.data.frame(sample, Xi, Yi)
      colnames(d_juges_tmp) = c("sample", 
                                paste("X", nb[i], "-juge-", i, sep = ""), 
                                paste("Y", nb[i], "-juge-", i, sep = "")
      )
      d_juges = plyr::join(d_juges, d_juges_tmp, by = "sample")
    }
  }
  
  # 3.1. update juges for MFA after
  if( !is.null(juges_to_delete) ) { juges = juges[!is.element(juges, juges_to_delete)] }
  
  # 3.2. Add to d_juges the number of time the adjective was cited
  adj = colnames(N)[5:ncol(N)]
  b = as.data.frame(matrix(0, ncol = length(adj), nrow = nrow(d_juges)))
  colnames(b) = adj
  
  d_juges = cbind.data.frame(d_juges, b)
  
  sample = as.character(d_juges$sample)
  
  for (ech in sample){
    dtmp = droplevels(subset(N, sample == ech))
    for (ad in adj){
      d_juges[which(d_juges$sample == ech), ad] = sum(dtmp[,ad])
    }
  }
  
  select <- is.na (d_juges) # on vire les NA: pourquoi? Pourquoi ne pas mettre 0 ou la moyenne ?
  aeliminer <- apply(select, MARGIN = 1, FUN = any)
  d_MFA <- d_juges[!aeliminer, ]
  row.names(d_MFA) <- d_MFA[, 1]
  d = d_MFA[,2:ncol(d_MFA)]
  
  d$sample = factor(rownames(d))
  
  d = list("data" = d, 
           "descriptors" = descriptors
           )
  
  class(d) <- c("PPBstats", "data_organo_napping")
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}
