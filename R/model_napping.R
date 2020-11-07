#' Run napping analysis
#'
#' @description
#' \code{model_napping} runs napping analysis
#'
#' @param data data frame coming from \code{\link{format_data_PPBstats.data_organo_napping}}
#' 
#' @details
#' The Multiple Factor Analysis is run with FactoMineR::MFA
#' 
#' More information can be found in the book : https://priviere.github.io/PPBstats_book/napping.html
#' 
#' @return 
#' A list with two elements : the MFA object and the data
#' 
#' @author Pierre Riviere and Camille Vindras
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}
#' \item \code{\link{check_model.fit_model_napping}}
#' }
#' 
#' @import FactoMineR
#' @importFrom methods is
#' 
#' @export
#' 
model_napping = function(
  data
  )
{
  # 0. Error message ----------
  if(!is(data, "data_organo_napping")){ 
    stop(substitute(data), " must be formated with type = \"data_organo_napping\", see PPBstats::format_data_PPBstats().") 
    }
  
  # 1.Format data ----------
  data_raw = data$data
  data = data_raw[,-which(is.element(colnames(data_raw), c("sample", "germplasm", "location")))] # delete columns
  j = as.character(colnames(data)[grep("-juge-", colnames(data))])
  j = unlist(strsplit(j, "-juge-"))
  juges = unique(j[seq(2, length(j), 2)])

  gp_XY = rep(2, length(juges))
  gp_adj = ncol(data) - sum(gp_XY)
  group = c(gp_XY, gp_adj)
  
  type = c(rep( "c", length(juges)), "f")
  name.group = c(paste("J-", juges, sep=""),"descriptors")
  num.group.sup = NULL #c(1, length(juges)*2)

  out_MFA = FactoMineR::MFA(data, group = group, type = type, 
            ind.sup = NULL, ncp = 5, axes = c(1, 2), 
            name.group  = name.group, num.group.sup = num.group.sup,
            graph = FALSE)
  
  out = list(out_MFA = out_MFA, data = data_raw)
  
  # Return results ----------
  class(out) <- c("PPBstats", "fit_model_napping")
  return(out)
}

