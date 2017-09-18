#' Check and format the data to be used in PPBstats functions
#'
#' @description
#' \code{format_data_PPBstats} checks and formats the data to be used in PPBstats functions
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
#' @param network_part For type = "data_network", element of the network, it can
#' \itemize{
#'  \item for unipartite network : "seed_lots"
#'  \item for bipartite network : 
#'  \itemize{
#'   \item c("germplasm", "location") or
#'   \item c("location", "germplasm") or
#'   \item c("germplasm", "year") or
#'   \item c("year", "germplasm") or
#'   \item c("location", "year") or
#'   \item c("year", "location")
#'  }
#' } 
#' 
#' 
#' @details 
#' \itemize{
#'  \item For type = "data_network" : The data frame are different regarding type of network
#'  \itemize{
#'   \item for unipartite network, the data must have the following columns : 
#'   "seed_lot_parent", "seed_lot_child", "relation_type",
#'   "germplasm_parent", "location_parent", "year_parent",
#'   "germplasm_child", "location_child", "year_child".
#'   
#'   It can have in option : "alt_parent", "long_parent", "lat_parent", 
#'   "alt_child", "long_child", "lat_child" to get map representation
#'   
#'   It can have supplementary variables with tags "_parent", "_child" or "_relation".
#'   
#'   
#'   \item for bipartite network, the data must have the following columns : "germplasm", "location", "year"
#'   
#'   It can have in option : "alt", "long", "lat" to get map representation
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
#'  For example for group 1, there is version 1 and 2. 
#'  See data(data_version) for an example.
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
#' For "data_network", it return a igraph object coming from igraph::graph_from_data_frame()
#' 
#' @author Pierre Riviere
#' 
format_data_PPBstats = function(
  data, 
  type = "data_agro",
  code,
  threshold = NULL,
  network_part = NULL
  )
  {
  # 0. Error messages ----------
  match.arg(type, c("data_network", 
                    "data_agro_version", "data_agro", 
                    "data_organo_napping", "data_organo_hedonic")
            )
  
  # 1.Network ----------
  if(type == "data_network"){
    d = format_data_PPBstats.data_network(data, network_part)
  }
  
  # 2.Agro ----------
  if(type == "data_agro"){
    d = format_data_PPBstats.data_agro(data)
  }
  
  if(type == "data_agro_version"){
    mess = "To do !!!."
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

