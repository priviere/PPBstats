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
  
  d = data
  
  # 1.Network ----------
  if(type == "data_network"){
    
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
  }
  
  # 2.Agro ----------
  if(type == "data_agro"){
    mess = "The following column are compulsory : c(\"location\", \"year\", \"germplasm\", \"block\", \"X\", \"Y\"."
    # check columns
    if(!is.element("location", colnames(d))) { stop(mess) }
    if(!is.element("year", colnames(d))) { stop(mess) }
    if(!is.element("germplasm", colnames(d))) { stop(mess) }
    if(!is.element("block", colnames(d))) { stop(mess) }
    if(!is.element("X", colnames(d))) { stop(mess) }
    if(!is.element("Y", colnames(d))) { stop(mess) }
    
    mess = "The following column must be set as factor : c(\"location\", \"year\", \"germplasm\", \"block\", \"X\", \"Y\"."
    if(!is.factor(d$location)) { stop(mess) }
    if(!is.factor(d$year)) { stop(mess) }
    if(!is.factor(d$germplasm)) { stop(mess) }
    if(!is.factor(d$block)) { stop(mess) }
    if(!is.factor(d$X)) { stop(mess) }
    if(!is.factor(d$Y)) { stop(mess) }
    
    # check and format date
    vec_date = colnames(d)[grep("\\$date", colnames(d))]
    if( length(vec_date) == 0 ) { vec_date = NULL }
    if(!is.null(vec_date)){
      vec_date = colnames(d)[grep("\\$date", colnames(d))]
      
      for(i in 1:length(vec_date)){
        # check date
        v = d[, vec_date[i]]
        for(j in 1:length(v)){
          t = try(format(as.Date(as.character(v[j])), format = "%Y-%m-%d", origin = "1900-01-01"), silent = TRUE)
          if( class(t) == "try-error" || is.na(t) ) stop( 
            "Date must be at format year-month-day for row ", j, " for ", vec_date[i], "."
            )
        }
        
        # format date
        v = format(as.Date(data_GxE[, vec_date[i]]), format = "%Y/%m/%d")
        vjd = sapply(v, function(x) {
          julian(as.Date(x), origin = as.Date(paste(unlist(strsplit(x, "/"))[1], "/01/01", sep = "")))
        })
        d = cbind.data.frame(d, vjd)
        colnames(d)[ncol(d)] = paste(vec_date[i], "_julian", sep = "")
      }
    }
    
    class(d) <- c("PPBstats", "data.frame", "data_agro")
    message(substitute(data), " has been formated for PPBstats functions.")
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
    
    N = format_organo(data, code, threshold)
    N = N[,c(6, 1, 2, 3, c(7:ncol(N)))]
    
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
    
    select <- is.na (d_juges) # on vire les NA: pourquoi? Pourquoi ne pas mettre 0 ou la moyenne ?
    aeliminer <- apply(select, MARGIN = 1, FUN = any)
    d_MFA <- d_juges[!aeliminer, ]
    row.names(d_MFA) <- d_MFA[, 1]
    d = d_MFA[,2:ncol(d_MFA)]
    
    class(d) <- c("PPBstats", "data.frame", "data_organo_napping")
    message(substitute(data), " has been formated for PPBstats functions.")
  }
  
  if(type == "data_organo_hedonic"){
    mess = "In data, the following column are compulsory : \"sample\", \"juges\", \"note\", \"descriptors\"."
    if(!is.element("sample", colnames(data))) { stop(mess) }
    if(!is.element("juges", colnames(data))) { stop(mess) }
    if(!is.element("note", colnames(data))) { stop(mess) }
    if(!is.element("descriptors", colnames(data))) { stop(mess) }
    
    mess = "In code, the following column are compulsory : \"germplasm\", \"location\", \"code\"."
    if(!is.element("germplasm", colnames(code))) { stop(mess) }
    if(!is.element("location", colnames(code))) { stop(mess) }
    if(!is.element("code", colnames(code))) { stop(mess) }
    
    colnames(code)[3] = "sample"
    
    d = format_organo(data, code, threshold)
    
    class(d) <- c("PPBstats", "data.frame", "data_organo_hedonic")
    message(substitute(data), " has been formated for PPBstats functions.")
  }
  
  # 4.Return results ----------
  return(d)
}

