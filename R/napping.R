#' Run napping analysis
#'
#' @description
#' \code{napping} runs napping analysis
#'
#' @param data data frame coming from format_data_PPBstats().
#' It has the following columns: sample, juges, X, Y, descriptors. 
#' The descriptors must be separated by ";"
#' 
#' @details
#' The Multiple Factor Analysis is run with FactoMineR::MFA on d_freq
#' 
#' @return 
#' A list of ? elements being the MFA and obecjt and its graph :
#' \itemize{
#'  \item out_MFA
#'  \item 
#'  \item 
#' }
#' 
#' @author Pierre Riviere and Camille Vindras
#' 
#' 
napping = function(
  data
  )
{
  # 0. Error message ----------
  if(!is(data, "data_organo_napping")){ 
    stop(substitute(data), " must be formated, see PPBstats::format_data_PPBstats().") 
    }
  
  # 1.Format data ----------
  
  j = as.character(colnames(data)[grep("-juge-", colnames(data))])
  j = unlist(strsplit(j, "-juge-"))
  juges = unique(j[seq(2, length(j), 2)])

  gp_XY = rep(2, length(juges))
  gp_adj = ncol(data) - sum(gp_XY)
  group = c(gp_XY, gp_adj)
  
  type = c(rep( "c", length(juges)), "f")
  name.group = c(paste("J-", juges, sep=""),"descriptors")
  num.group.sup = NULL #c(1, length(juges)*2)

  out_MFA = MFA(data, group = group, type = type, ind.sup = NULL, ncp = 5, axes = c(1, 2), name.group  = name.group, num.group.sup = num.group.sup)
  
  # Return results ----------
  out = list(
    "out_MFA" = out_MFA
  )
  
  return(out)
}

