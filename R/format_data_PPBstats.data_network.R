format_data_PPBstats.data_network = function(data, network_part){
  d = data
  
  # 1.1. Error message ----------
  mess = "
  You must settle appropriate network_part arg.\n
  - It can be unipartite network with network_part = \"seed_lots\".\n
  - It can be bipartite network with network_part = \n
  \t c(\"germplasm\", \"location\") or
  \t c(\"location\", \"germplasm\") or
  \t c(\"germplasm\", \"year\") or
  \t c(\"year\", \"germplasm\") or
  \t c(\"location\", \"year\") or
  \t c(\"year\", \"location\").
  "
  if( is.null(network_part) ) { stop(mess) }
  
  if( length(network_part) == 1 & network_part[1] != "seed_lots"  ) { stop(mess) }
  
  if( length(network_part) == 2 & network_part[1] == network_part[2] ) { stop(mess) }
  if( length(network_part) == 2 & !is.element(network_part[1], c("germplasm", "location", "year"))  ) { stop(mess) }
  if( length(network_part) == 2 & !is.element(network_part[2], c("germplasm", "location", "year"))  ) { stop(mess) }
  
  if( length(network_part) > 2 ) { stop(mess) }
  
  # 1.2.1. Error message and data formating regarding unipartite network object ----------
  if( network_part[1] == "seed_lots" ){
    # Factors compulsory
    vec_factor = c("seed_lot_parent", "seed_lot_child", "relation_type",
                   "germplasm_parent", "location_parent", "year_parent",
                   "germplasm_child", "location_child", "year_child"
    )
    
    for(i in vec_factor) {
      if(!is.element(i, colnames(d))) { stop("Column \"", i, "\" is compulsory in data.") }
    }
    
    
    # 1.2.2. Variable in option to get map
    vec_variables = c("alt_parent", "long_parent", "lat_parent", 
                      "alt_child", "long_child", "lat_child"
    )
    for(i in vec_variables) {
      if(!is.element(i, colnames(d))) { warning("Column \"", i, "\" is needed to get map and not present in data.") }
    }
    
    # 1.2.3. Variable in option to attributes to vertex or edge
    # Check that there are variable for _child and _parent
    col_d = colnames(d)
    col_d = col_d[-which(is.element(col_d, c(vec_factor, vec_variables)))]
    
    t = grep("_parent", col_d)
    if(length(t)==0){ tp = NULL } else { tp = sub("_parent", "", col_d[grep("_parent", col_d)]) }
    t = grep("_child", col_d)
    if(length(t)==0){ ts = NULL } else { ts = sub("_child", "", col_d[grep("_child", col_d)]) }
    t = unique(c(tp, ts))
    
    for(i in t){
      vec_t = col_d[grep(i, col_d)]
      if(length(grep("_parent", vec_t)) == 0){ stop(i, " must be settle for _parent") }
      if(length(grep("_child", vec_t)) == 0){ stop(i, " must be settle for _child") }
    }
    
  }
  
  # 1.3.1. Error message and data formating regarding bipartite network object ----------
  if( length(network_part) == 2 ){
    # Factors compulsory
    vec_factor = c("germplasm", "location", "year")
    
    for(i in vec_factor) {
      if(!is.element(i, colnames(d))) { stop("Column \"", i, "\" is compulsory in data.") }
    }
    
    # 1.3.2. Variable in option to get map
    vec_variables = c("alt", "long", "lat")
    for(i in vec_variables) {
      if(!is.element(i, colnames(d))) { warning("Column \"", i, "\" is needed to get map and not present in data.") }
    }
    
  }
  
  
  # 1.4. Format unipartite network object ----------
  if(network_part[1] == "seed_lots") {
    
    t = grep("_parent", colnames(d))
    tp = sub("_parent", "", colnames(d)[grep("_parent", colnames(d))])
    t = grep("_child", colnames(d))
    ts = sub("_child", "", colnames(d)[grep("_child", colnames(d))])
    t = unique(tp, ts)
    if( length(which(t == "")) > 0  ) { t = t[-which(t == "")] }
    t = t[c(which(t == "seed_lot"), which(t != "seed_lot"))]
    
    d_vertex = data.frame(c(as.character(d[,paste(t[1], "_parent", sep = "")]), as.character(d[,paste(t[1], "_child", sep = "")])))
    for(i in 2:length(t)){
      d_vertex = cbind.data.frame(
        d_vertex,
        c(as.character(d[,paste(t[i], "_parent", sep = "")]), as.character(d[,paste(t[i], "_child", sep = "")]))
      )
    }
    colnames(d_vertex) = t
    d_vertex = unique(d_vertex)
    
    d_vertex = d_vertex[!duplicated(d_vertex$seed_lot),] # should not arrise !
    
    relation = unique(d[,c("seed_lot_parent", "seed_lot_child", "relation_type")])
  }
  
  # 1.5. Format bipartite network object ----------
  if(length(network_part) == 2) {
    
    d_vertex = data.frame(c(as.character(d[,network_part[1]]), 
                            as.character(d[,network_part[2]]))
    )
    colnames(d_vertex) = "vertex"
    d_vertex$type = c(rep("network_part_1", nrow(d)), rep("network_part_2", nrow(d)))
    d_vertex = unique(d_vertex)
    
    relation = unique(d[,c(network_part[1], network_part[2])])
  }
  
  
  # 1.6. Format igraph object ----------
  d = igraph::graph_from_data_frame(d = relation, directed = TRUE, vertices = d_vertex) 
  class(d) <- c("PPBstats", "igraph", "data_network")
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}
