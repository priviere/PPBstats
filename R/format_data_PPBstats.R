#' Check and format the data to be used by PPBstats functions
#'
#' @description
#' \code{format_data_PPBstats} checks and formats the data to be used by PPBstats functions
#' 
#' @param data The data frame to format
#' 
#' @param type type of format : 
#' \itemize{
#'  \item data_network
#'  \item data_agro
#'  \item data_agro_SR
#'  \item data_agro_HA
#'  \item data_agro_LF
#'  \item data_organo_napping
#'  \item data_organo_hedonic
#'  }
#' 
#' @param threshold For type = data_organo, number of occurence of descriptors <= threshold are kept
#' 
#' @param network_part For type = "data_network", element of the network, it can be "unipart" or "bipart"
#' 
#' @param vertex_type For type = "data_network",
#' \itemize{
#'  \item for unipart network : "seed_lots" or "location"
#'  \item for bipart network : c("germplasm", "location")
#'  }
#'  
#' @param network_split For type = "data_network" and network_part = "unipart" and 
#' vertex_type = "location", split of the data that can be "germplasm" or "relation_year_start"
#' 
#' 
#' @details
#' See for more details :
#' \itemize{
#'  \item \code{\link{format_data_PPBstats.data_network}} 
#'  \item \code{\link{format_data_PPBstats.data_agro}} 
#'  \item \code{\link{format_data_PPBstats.data_agro_SR}} 
#'  \item \code{\link{format_data_PPBstats.data_agro_HA}} 
#'  \item \code{\link{format_data_PPBstats.data_agro_LF}} 
#'  \item \code{\link{format_data_PPBstats.data_organo_napping}} 
#'  \item \code{\link{format_data_PPBstats.data_organo_hedonic}} 
#' }
#' 
#' @author Pierre Riviere
#' 
#' @export
#' 
format_data_PPBstats = function(
  data, 
  type,
  threshold,
  network_part = c("unipart", "bipart"), 
  network_split = c("germplasm", "relation_year_start"),
  vertex_type = NULL
  )
  {
  # 0. Error messages ----------
  match.arg(type, c("data_network", "data_agro",
                    "data_agro_SR", "data_agro_HA", "data_agro_LF",  
                    "data_organo_napping", "data_organo_hedonic")
            )
  
  # 1.Network ----------
  if(type == "data_network"){
    d = format_data_PPBstats.data_network(data, network_part, network_split, vertex_type)
  }
  
  # 2.Agro ----------
  if(type == "data_agro"){
    d = format_data_PPBstats.data_agro(data)
  }
  
  if(type == "data_agro_SR"){
    d = format_data_PPBstats.data_agro_SR(data)
  }
  
  if(type == "data_agro_HA"){
    d = format_data_PPBstats.data_agro_HA(data)
  }
  
  if(type == "data_agro_LF"){
    d = format_data_PPBstats.data_agro_LF(data)
  }
  
  # 3.Organo ----------
  if(type == "data_organo_napping"){
    d = format_data_PPBstats.data_organo_napping(data, threshold)
  }
  
  if(type == "data_organo_hedonic"){
    d = format_data_PPBstats.data_organo_hedonic(data, threshold)
  }
  
  # 4.Return results ----------
  return(d)
}

