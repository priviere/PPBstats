#' Check and format the data to be used by PPBstats functions
#'
#' @description
#' \code{format_data_PPBstats} checks and formats the data to be used by PPBstats functions
#' 
#' @param data The data frame. See details.
#' 
#' @param type type of format : 
#' \itemize{
#'  \item data_network
#'  \item data_agro
#'  \item data_agro_version
#'  \item data_organo_napping
#'  \item data_organo_hedonic
#'  }
#' See details.
#' 
#' @param code For type = data_organo, data frame with code of samles. See details.
#' 
#' @param threshold For type = data_organo, number of occurence of descriptors <= threshold are kept
#' 
#' @param network_part For type = "data_network", element of the network, it can be "unipart" or "bipart"
#' 
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
#' \itemize{
#'  \item For type = "data_network" : The data frame are different regarding type of network
#'  \itemize{
#'   \item for unipart network, two vertex_type are possible :
#'   \itemize{
#'    \item "seed_lots" : the data must have the following columns : 
#'    \itemize{
#'     \item "seed_lot_parent" : name of the seed lot parent in the relation
#'     \item "seed_lot_child" ; name of the seed lots child in the relation
#'     \item "relation_type" : the type of relation between the seed lots
#'     \item "relation_year_start" : the year when the relation starts
#'     \item "relation_year_end" : the year when the relation stops
#'     \item "germplasm_parent" : the germplasm associated to the seed lot father
#'     \item "location_parent" : the location associated to the seed lot father
#'     \item "year_parent" : represents the year of the last relation event of the seed lot father
#'     \item "germplasm_child" : the germplasm associated to the seed lot child
#'     \item "location_child" : the location associated to the seed lot child
#'     \item "year_child" : represents the year of the last relation event of the seed lot child
#'    }
#'    
#'    
#'    It can have in option : "alt_parent", "long_parent", "lat_parent",
#'    "alt_child", "long_child", "lat_child" to get map representation
#'    
#'    It can have supplementary variables with tags "_parent", "_child" or "_relation".
#'    
#'    \item "location" that represents each diffusion between location : the data can have two formats:
#'    \itemize{
#'     \item the same format than for unipart network and vertex_type = seed_lots
#'     \item the following columns (explained above): 
#'     "location_parent", "location_child"
#'     "relation_year_start", "relation_year_end"
#'     It can have in option : "germplasm_parent", "year_parent",
#'     "germplasm_child", "year_child"
#'     It can have in option : "alt_parent", "long_parent", "lat_parent",
#'     "alt_child", "long_child", "lat_child" to get map representation
#'     }
#'    }
#'   
#'   \item for bipartite network where a vertex can be a location or a germplasm, the data can have two formats:
#'   \itemize{
#'    \item the same format than for unipart network and vertex_type = seed_lots. 
#'    In this case, relation type diffusion or reproduction are kept.
#'    \item the following columns : "germplasm", "location", "year"
#'    It can have in option : "alt", "long", "lat" to get map representation
#'   }
#'  }
#'  
#'  
#'  \item For type = "data_agro" : 
#'  It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "..."), with "..." the variables.
#'  The variables can be linked to their corresponding dates. 
#'  The dates are associated to their corresponding variable by $.
#'  For example the date associated to variable y1 is y1$date.
#'  The date must have format year-month-day, e.g. 2017-12-05
#'  
#'  \item For type = "data_agro_version" : 
#'  It should have the following columns: c("year", "germplasm", "location", "group", "version").
#'  The group refers to an id that contains two different versions.
#'  For example for group 1, there are version 1 and 2.
#'  Within each group, there must have different version and different germplasm 
#'  (i.e. each version must have different germplasms.).
#'  
#'  \item For type = "data_organo_napping"
#'  \itemize{
#'   \item data is a data frame with the following columns: sample, juges, X, Y, descriptors. 
#'   The descriptors must be separated by ";"
#'   \item code is data frame with the following columns germplasm, location, code.
#'   The function merge data and code to join the information.
#'  }
#' 
#'  \item For type = "data_organo_hedonic"
#'  
#' }
#' 
#' @return 
#' The function returns the data with the right format (i.e. class)
#' 
#' \itemize{
#'  \item For "data_network",
#'  \itemize{
#'   \item it returns a igraph object coming from igraph::graph_from_data_frame().
#'   \item for unipart network on seed lots, it a list of one element
#'   \item for unipart network on location
#'   \itemize{
#'    \item for network_split = "germplasm", 
#'   it returns a list with as many elements as germplam in the data
#'   as well as all germplasms merged in the first element of the list.
#'    \item for network_split = "relation_year_start", 
#'   it returns a list with as many elements as year in the data
#'   as well as all years merged in the first element of the list.
#'   }
#'   \item for bipart network, it returns a list with as many elements as year in the data 
#'   as well as all years merged in the first element of the list.
#'   If no year are provided into the data, all information are merged.
#'  }
#' }
#' 
#' 
#' 
#' @author Pierre Riviere
#' 
format_data_PPBstats = function(
  data, 
  type = "data_agro",
  code,
  threshold,
  network_part = c("unipart", "bipart"), 
  network_split = c("germplasm", "relation_year_start"),
  vertex_type = NULL
  )
  {
  # 0. Error messages ----------
  match.arg(type, c("data_network", 
                    "data_agro_version", "data_agro", 
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
  
  if(type == "data_agro_version"){
    d = format_data_PPBstats.data_agro_version(data)
  }
  
  # 3.Organo ----------
  format_organo = function(data, code, threshold){
    # 1. Merge and create data frame ----------
    N = join(data, code, by = "sample")
    N = N[,-1]
    N$sample = factor(paste(N$location, N$germplasm, sep = ":"))
    
    N$juges<-as.factor(as.character(N$juges))
    N$sample<-as.factor(N$sample)
    
    # 2. Add the occurence of the different descriptors ----------
    descriptors = as.vector(as.character(N$descriptors))
    vec_adj = unlist(strsplit(descriptors, ";"))
    vec_adj = sort(unique(vec_adj))
    if( length(which(vec_adj == "")) > 0 ) { 
      vec_adj = vec_adj[-which(vec_adj == "")]
    }
    
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
    
    # 3. Apply the threshold to keep certain descriptors ----------
    if( !is.null(threshold) ) {
      test = apply(N[, vec_adj], 2, sum)
      to_delete = which(test <= threshold)
      if( length(to_delete) > 0 ) { 
        adj_to_delete = vec_adj[to_delete] 
        N = N[,-which(colnames(N) %in% adj_to_delete)]
        message("The following descriptors have been remove because there were less or equal to ", threshold, " occurences : ", paste(adj_to_delete, collapse = ", "))
        if( ncol(N) == 4 ){ stop("There are no more descriptors with threshold = ", threshold, 
                                 ". You must set another value.") }
      }
    }
    
    
    # 4. Get frequency for each descriptor ----------
    N_freq = N_raw = N
    for (ad in vec_adj) { 
      if( sum(N_raw[, ad], na.rm = TRUE) != 0 ) { 
        N_freq[, ad] = N_raw[, ad] / sum(N_raw[, ad], na.rm = TRUE) 
      }  
    }
    
    return(N_freq)
  }
  
  
  if(type == "data_organo_napping"){
    d = format_data_PPBstats.data_organo_napping(data, code, threshold)
  }
  
  if(type == "data_organo_hedonic"){
    d = format_data_PPBstats.data_organo_hedonic(data,code, threshold)
  }
  
  # 4.Return results ----------
  return(d)
}

