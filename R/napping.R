#' Run napping analysis
#'
#' @description
#' \code{napping} runs napping analysis
#'
#' @param data data frame with the following columns: sample, juges, X, Y, descriptors. The descriptors must be separated by ";"
#' 
#' @param code data frame with the following columns germplasm, location, code
#' 
#' @param threshold number of occurence of descriptorss <= threshold are kept
#' 
#' @details
#' The function merge data and code to join the information
#' 
#' The Multiple Factor Analysis is run with FactoMineR::MFA on d_freq
#' 
#' @return 
#' A list of three elements :
#' \itemize{
#'  \item d_raw : data frame with the following columns : sample, X1, Y1, ..., Xn, Yn, descriptors_1, ..., descriptors_n. In the column of descriptorss it is the total number of occurence.
#'  \item d_freq : same as d_raw but with frequence in colum of descriptorss
#'  \item MFA : outptut from Multiple Factor Analysis
#' }
#' 
#' @author Pierre Riviere and Camille Vindras
#' 
#' 

library(FactoMineR)
library(plyr)

napping = function(
  data, 
  code, 
  threshold = NULL)
{
  
  # 0. Error message ----------
  
  mess = "In data, the following column are compulsory : \"sample\", \"juges\", \"X\", \"Y\", \"descriptors\"."
  if(!is.element("sample", colnames(data))) { stop(mess) }
  if(!is.element("juges", colnames(data))) { stop(mess) }
  if(!is.element("X", colnames(data))) { stop(mess) }
  if(!is.element("Y", colnames(data))) { stop(mess) }
  if(!is.element("descriptors", colnames(data))) { stop(mess) }
  
  mess = "In code, the following column are compulsory : \"germplasm\", \"location\", \"code\"."
  if(!is.element("germplasm", colnames(code))) { stop(mess) }
  if(!is.element("location", colnames(code))) { stop(mess) }
  if(!is.element("code", colnames(code))) { stop(mess) }
  
  colnames(code)[3] = "sample"
  
  # 1. Merge and create data frame ----------
  N = join(data, code, by = "sample")
  N = N[,-1]
  N$sample = factor(paste(N$location, N$germplasm, sep = ":"))
  N = N[,c("sample", "juges", "X", "Y", "descriptors")]
  
  N$juges<-as.factor(as.character(N$juges))
  N$sample<-as.factor(N$sample)
  
  # 2. Add the occurence of the different descriptorss ----------
  descriptors = as.vector (as.character(N$descriptors))
  vec_adj = unlist(strsplit(descriptors, ";"))
  vec_adj = sort(unique(vec_adj))
  vec_adj = vec_adj[-which(vec_adj == "")]
  
  df = matrix(0, ncol = length(vec_adj), nrow = nrow(N))
  df = as.data.frame(df)
  colnames(df) = vec_adj
  out = cbind.data.frame(N, df)
  
  for (i in 1:nrow(out)){
    v_adj = out[i, "descriptors"]
    v_adj = unlist(strsplit(as.character(v_adj), ";"))
    if( length(which(v_adj == "")) > 0 ) { v_adj = v_adj[-which(v_adj == "")] }
    
    for (j in 1:length(v_adj)) {
      e = v_adj[j]
      if (length(e)>0) {
        if (!is.na(e)) { out[i, e] = 1 }
      }
    }
  }
  
  N = out[,-which(colnames(out) == "descriptors")]
  
  # 3. Get table with, for each judge, the X and Y for each sample tasted ----------
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
      colnames(d_juges_tmp) = c("sample", paste("X", nb[i], sep = ""), paste("Y", nb[i], sep = ""))
      d_juges = join(d_juges, d_juges_tmp, by = "sample")
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
  
  d_juges_raw = d_juges
  
  # 3.3. Apply the threshold to keep certain descriptors ----------
  if( !is.null(threshold) ) {
    test = apply(d_juges_raw[, vec_adj], 2, sum)
    to_delete = which(test <= threshold)
    if( length(to_delete) > 0 ) { 
      adj_to_delete = vec_adj[to_keep] 
      d_juges_raw[-which(colnames(d_juges_raw) == adj_to_delete)]
      message("The following descriptors have been remove because there were less or equal to ", threshold, " occurences : ", paste(adj_to_delete, collapse = ", "))
    }
  }
  
  # 3.4. Get frequency for each descriptor in d_juges ----------
  d_juges_freq = d_juges_raw
  
  for (ad in adj) { 
    if( sum(d_juges_raw[, ad], na.rm = TRUE) != 0 ) { 
      d_juges_freq[, ad] = d_juges_raw[, ad] / sum(d_juges_raw[, ad], na.rm = TRUE) 
    }  
  }
  
  # 4. Run the MFA on d_juges_freq ----------
  
  # on vire les NA: pourquoi? Pourquoi ne pas mettre 0 ou la moyenne ?
  select <- is.na (d_juges_freq)
  aeliminer <- apply(select, MARGIN = 1, FUN = any)
  d_MFA <- d_juges_freq[!aeliminer, ]
  
  row.names(d_MFA) <- d_MFA[, 1]
  
  d_MFA = d_MFA[,2:ncol(d_MFA)]
  
  gp_XY = rep(2, length(juges))
  gp_adj = ncol(d_MFA) - sum(gp_XY)
  group = c(gp_XY, gp_adj)
  
  type = c(rep( "c", length(juges)), "f")
  name.group = c(paste("J-", juges, sep=""),"descripteurs")
  num.group.sup = NULL #c(1, length(juges)*2)
  
  out_MFA <-MFA(d_MFA, group = group, type = type, ind.sup = NULL, ncp = 5, axes = c(1, 2), name.group  = name.group, num.group.sup = num.group.sup)
  
  # Return results ----------
  out = list(
    "d_raw" = d_juges_raw,
    "d_freq" = d_juges_freq,
    "MFA" = out_MFA
  )
  
  return(out)
}

