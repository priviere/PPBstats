format_data_PPBstats.data_network = function(
  data, 
  network_part = c("unipart", "bipart"), 
  network_split = c("germplasm", "relation_year_start"),
  vertex_type = NULL
  ){
  d = data
  
  # 1.1. Error message ----------
  if( !is.element(network_part, c("unipart", "bipart")) ) { stop("network_part must be either \"unipart\" or \"bipart\".") }
  
  mess = "
  You must settle appropriate vertex_type arg.\n
  - It can be unipart network with vertex_type = \"seed_lots\" or \"location\".\n
  - It can be bipart network with vertex_type = c(\"germplasm\", \"location\").
  "
  if( is.null(vertex_type) ) { stop(mess) }
  
  if( length(vertex_type) == 1 & !is.element(vertex_type[1], c("seed_lots", "location")) ) { stop(mess) }
  
  if( network_part == "bipart" & length(vertex_type) != 2 ) { stop(mess) }
  if( length(vertex_type) == 2 & vertex_type[1] != "germplasm"  ) { stop(mess) }
  if( length(vertex_type) == 2 & vertex_type[2] != "location" ) { stop(mess) }
  
  if( length(vertex_type) > 2 ) { stop(mess) }
  
  mess = "With network_part = \"bipart\", vertex_type must be c(\"germplasm\", \"location\")."
  if( network_part == "bipart" & vertex_type[1] == "seed_lots"){ stop(mess) }
  if( network_part == "bipart" & vertex_type[1] == "person"){ stop(mess) }
  
  
  mess = "network_part must be either \"germplasm\" or \"relation_year_start\" and 
         can be used only with network_part = \"unipart\" and vertex_type = \"location\"."
  
  if(!is.element(network_split[1], c("germplasm", "relation_year_start"))) { stop(mess) }
  
  # Functions used in this function ----------
  # Check data format ----------
  check_unipart_sl_data = function(d, display_mess = TRUE){
    
    return_d = TRUE
    
    # Factors compulsory
    vec_factor = c("seed_lot_parent", "seed_lot_child", 
                   "relation_type", "relation_year_start", "relation_year_end",
                   "germplasm_parent", "location_parent", "year_parent",
                   "germplasm_child", "location_child", "year_child"
    )
    
    for(i in vec_factor) {
      if(!is.element(i, colnames(d))) { 
        if( display_mess ){ stop("Column \"", i, "\" is compulsory in data.") }  else { return_d = FALSE }
        }
    }
    
    # Variable in option to get map
    vec_variables = c("alt_parent", "long_parent", "lat_parent", 
                      "alt_child", "long_child", "lat_child"
    )
    for(i in vec_variables) {
      if(!is.element(i, colnames(d))) { 
        if( display_mess ){ warning("Column \"", i, "\" is needed to get map and not present in data.") } 
        }
    }
    
    # Variable in option to attributes to vertex or edge
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
      if(length(grep("_parent", vec_t)) == 0){ 
        if( display_mess){ stop(i, " must be settle for _parent") } else { return_d = FALSE }
        if( display_mess){ stop(i, " must be settle for _child") } else { return_d = FALSE }
        }
    }
    
    if(!return_d){ d = NULL }
    return(d)
  }
  
  check_unipart_person_data = function(d, display_mess = TRUE){
    
    return_d = TRUE
    
    # Factors compulsory
    vec_factor = c("location_parent", "location_child",
                   "relation_year_start", "relation_year_end"
                   )
    
    for(i in vec_factor) {
      if(!is.element(i, colnames(d))) { 
        if( display_mess ){ stop("Column \"", i, "\" is compulsory in data.") } else { return_d = FALSE }
        }
      }
    
    # Year and Germplasm in option
    vec_factor_2 = c("germplasm_parent", "year_parent",
                   "germplasm_child", "year_child"
                   )
    
    for(i in vec_factor_2) {
      if(!is.element(i, colnames(d))) { 
        if( display_mess ){ warning("Column \"", i, "\" is optionnal and is not present in data.") } else { return_d = FALSE }
      }
    }
    
    # Variable in option to get map
    vec_variables = c("alt_parent", "long_parent", "lat_parent", 
                      "alt_child", "long_child", "lat_child"
    )
    for(i in vec_variables) {
      if(!is.element(i, colnames(d))) { 
        if( display_mess ){ warning("Column \"", i, "\" is needed to get map and not present in data.") } 
      }
    }
    
    # Variable in option to attributes to vertex or edge
    # Check that there are variable for _child and _parent
    col_d = colnames(d)
    col_d = col_d[-which(is.element(col_d, c(vec_factor, vec_factor_2, vec_variables)))]
    
    t = grep("_parent", col_d)
    if(length(t)==0){ tp = NULL } else { tp = sub("_parent", "", col_d[grep("_parent", col_d)]) }
    t = grep("_child", col_d)
    if(length(t)==0){ ts = NULL } else { ts = sub("_child", "", col_d[grep("_child", col_d)]) }
    t = unique(c(tp, ts))
    
    for(i in t){
      vec_t = col_d[grep(i, col_d)]
      if(length(grep("_parent", vec_t)) == 0){ 
        if( display_mess){ stop(i, " must be settle for _parent") } else { return_d = FALSE }
        if( display_mess){ stop(i, " must be settle for _child") } else { return_d = FALSE }
      }
    }
    
    if(!return_d){ d = NULL }
    return(d)
  }
  
  check_bipart_data = function(d, display_mess = FALSE){
    
    return_d = TRUE
    
    # Factors compulsory
    if( !is.element("germplasm", colnames(d)) ) { 
      if(display_mess){ stop("Column \"germplasm\" is compulsory in data.") } else { return_d = FALSE }
      }
    if( !is.element("location", colnames(d)) ) { 
      if(display_mess){ stop("Column \"location\" is compulsory in data.") } else { return_d = FALSE }
      }
    
    # Year in option
    if(!is.element("year", colnames(d))) { 
      if(display_mess){ warning("Column \"year\" is optionnal and is not present in data.") }
      }
    
    # Variable in option to get map
    vec_variables = c("alt", "long", "lat")
    for(i in vec_variables) {
      if(!is.element(i, colnames(d))) { 
        if(display_mess){ warning("Column \"", i, "\" is needed to get map and not present in data.") }
        }
    }
    
    if(!return_d){ d = NULL }
    return(d)
  }
  

  # Transform format data ----------
  unipart_sl_data_to_unipart_location_data = function(data){
    d = check_unipart_sl_data(data)
    
    if( !is.element("diffusion", as.character(data$relation_type)) ){ 
      stop("There are no diffusion event in the column relation_type in data. Transform data for vertex_type = \"location\" is not possible.") 
    }
    
    d = filter(d, relation_type == "diffusion")
    d_person = d[,c(
      "location_parent", "location_child", 
      "relation_year_start", "relation_year_end",
      "germplasm_parent", "year_parent", 
      "alt_parent", "long_parent", "lat_parent",
      "germplasm_child", "year_child",
      "alt_child", "long_child", "lat_child"
    )]
    
    return(d_person)
  }
  
  unipart_sl_data_to_bipart_data = function(data){
    d = check_unipart_sl_data(data)
    
    if( !is.element("reproduction", as.character(data$relation_type)) ){ 
      stop("There are no reproduction event in the column relation_type in data. Transform data for network_part = \"bipart\" is not possible.") 
    }
    
    d = filter(d, relation_type == "reproduction")
    d_bipart = data.frame(
      c(as.character(d[,"germplasm_parent"]), as.character(d[,"germplasm_child"])),
      c(as.character(d[,"location_parent"]), as.character(d[,"location_child"])),
      c(as.character(d[,"year_parent"]), as.character(d[,"year_child"]))
    )
    colnames(d_bipart) = c("germplasm", "location", "year")
    
    if( is.element("alt_parent", colnames(d)) ){
      d_bipart = cbind.data.frame(
        d_bipart, 
        c(as.character(d[,"alt_parent"]), as.character(d[,"alt_child"]))
        )
      colnames(d_bipart)[ncol(d_bipart)] = "alt"
    }
    
    if( is.element("long_parent", colnames(d)) ){
      d_bipart = cbind.data.frame(
        d_bipart, 
        c(as.character(d[,"long_parent"]), as.character(d[,"long_child"]))
      )
      colnames(d_bipart)[ncol(d_bipart)] = "long"
    }
    
    if( is.element("lat_parent", colnames(d)) ){
      d_bipart = cbind.data.frame(
        d_bipart, 
        c(as.character(d[,"lat_parent"]), as.character(d[,"lat_child"]))
      )
      colnames(d_bipart)[ncol(d_bipart)] = "lat"
    }
    
    return(d_bipart)
  }
  
  # unipart & seed_lots ----------
  if( network_part == "unipart" & vertex_type[1] == "seed_lots" ){
    d = check_unipart_sl_data(d)
    
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
    
    dup = which(duplicated(d_vertex$seed_lot))
    if( length(dup) > 0 ){
      warning(paste("The following seed_lots are duplicated : ",
                    paste(as.character(d_vertex$seed_lot[dup]), collapse = ","),
                    ". Only one information per seed_lots has been kept.", sep = ""
                    )
              )
      d_vertex = d_vertex[!duplicated(d_vertex$seed_lot),]
    }
    
    relation = unique(d[,c("seed_lot_parent", "seed_lot_child", "relation_type")])
    
    OUT = list(list("d_vertex" = d_vertex, "relation" = relation))
  }

  # unipart & location ----------
  if( network_part == "unipart" & vertex_type[1] == "location" ){
    
    d1 = check_unipart_sl_data(d, display_mess = FALSE)
    d2 = check_unipart_person_data(d, display_mess = FALSE)
    
    if( is.null(d1) & is.null(d2) ) { 
      stop(
        "With network = \"unipart\" and vertex_type = \"location\", two data are possible:\n
        1. the data must have the following columns : \n
        \"seed_lot_parent\", \"seed_lot_child\",\n
        \"relation_type\", \"relation_year_start\", \"relation_year_end\",\n
        \"germplasm_parent\", \"location_parent\", \"year_parent\",\n
        \"germplasm_child\", \"location_child\", \"year_child\". \n
        It can have in option : \"alt_parent\", \"long_parent\", \"lat_parent\",\n
        \"alt_child\", \"long_child\", \"lat_child\" to get map representation \n
        It can have supplementary variables with tags \"_parent\", \"_child\" or \"_relation\". \n
        \n OR \n
        2. the data must have the following colums :\n
        \"location_parent\", \"location_child\", \n
        \"relation_type\", \"relation_year_start\", \"relation_year_end\",\n
        It can have in option : \"year\", \"germplasm\" and \n
        \"alt\", \"long\", \"lat\" to get map representation "
        ) 
      }
    if( !is.null(d1) & is.null(d2) ) { d = unipart_sl_data_to_unipart_location_data(d) }
    if( is.null(d1) & !is.null(d2) ) { d = check_unipart_person_data(d) }
    
    if( !is.element("germplasm_parent", colnames(d)) ){ 
      d$germplasm_parent = d$germplasm_child = "unknown_germplasm" 
      }
    
    # Split on germplasm ----------
    if( network_split == "germplasm" ){
      vec_germplasm = sort(unique(c(as.character(d$germplasm_parent), as.character(d$germplasm_child))))
      d_all_germplasm = d
      d_all_germplasm$germplasm_parent = paste(vec_germplasm, collapse = "-")
      d_all_germplasm$germplasm_child = paste(vec_germplasm, collapse = "-")
      d = rbind.data.frame(d, d_all_germplasm)
      vec_germplasm = c(paste(vec_germplasm, collapse = "-"), vec_germplasm)
      
      OUT = NULL
      
      for(g in vec_germplasm){
        dg = filter(d, germplasm_parent == g | germplasm_child == g)
        
        t = grep("_parent", colnames(dg))
        tp = sub("_parent", "", colnames(dg)[grep("_parent", colnames(dg))])
        t = grep("_child", colnames(dg))
        ts = sub("_child", "", colnames(dg)[grep("_child", colnames(dg))])
        t = unique(tp, ts)
        if( length(which(t == "")) > 0  ) { t = t[-which(t == "")] }
        if( length(which(t == "seed_lot")) > 0  ) { t = t[-which(t == "seed_lot")] }
        t = t[c(which(t == "location"), which(t != "location"))]
        
        d_vertex = data.frame(c(as.character(dg[,paste(t[1], "_parent", sep = "")]), as.character(dg[,paste(t[1], "_child", sep = "")])))
        for(i in 2:length(t)){
          d_vertex = cbind.data.frame(
            d_vertex,
            c(as.character(dg[,paste(t[i], "_parent", sep = "")]), as.character(dg[,paste(t[i], "_child", sep = "")]))
          )
        }
        colnames(d_vertex) = t
        
        d_vertex_bis = data.frame(
          c(as.character(dg[,"relation_year_start"]), as.character(dg[,"relation_year_start"])),
          c(as.character(dg[,"relation_year_end"]), as.character(dg[,"relation_year_end"]))
        )
        colnames(d_vertex_bis) = c("relation_year_start", "relation_year_end")
        
        d_vertex = cbind.data.frame(d_vertex, d_vertex_bis)
        
        t = as.data.frame(with(d_vertex, table(location, germplasm)))
        colnames(t)[3] = "nb_sl"
        
        d_vertex = d_vertex[,-which(colnames(d_vertex) == "relation_year_start")]
        d_vertex = d_vertex[,-which(colnames(d_vertex) == "relation_year_end")]
        d_vertex = d_vertex[,-which(colnames(d_vertex) == "year")]
        d_vertex = d_vertex[,-which(colnames(d_vertex) == "germplasm")]
        d_vertex = join(d_vertex, t, by = "location")
        d_vertex = d_vertex[,-which(colnames(d_vertex) == "germplasm")]
        d_vertex = unique(d_vertex)
        
        dup = which(duplicated(d_vertex$location))
        if( length(dup) > 0 ){
          warning(paste("The following location are duplicated : ",
                        paste(as.character(d_vertex$location[dup]), collapse = ","),
                        ". Only one information per location has been kept.", sep = ""
          )
          )
          d_vertex = d_vertex[!duplicated(d_vertex$location),]
        }
        
        relation = unique(dg[,c("location_parent", "location_child")]) 
        
        out = list(d_vertex, relation); names(out) = c("d_vertex", "relation")
        OUT = c(OUT, list(out))
      }
      names(OUT) = vec_germplasm
    }
    
    # Split on relation_year_start ----------
    if( network_split == "relation_year_start" ){
      vec_year = sort(unique(as.character(d$relation_year_start))) 
      d_all_year = d
      d_all_year$relation_year_start = paste(vec_year, collapse = "-")
      d = rbind.data.frame(d, d_all_year)
      vec_year = c(paste(vec_year, collapse = "-"), vec_year)
      
      OUT = NULL
      
      for(y in vec_year){
        dy = filter(d, relation_year_start == y) # Only on child because year of the last event
        
        t = grep("_parent", colnames(dy))
        tp = sub("_parent", "", colnames(dy)[grep("_parent", colnames(dy))])
        t = grep("_child", colnames(dy))
        ts = sub("_child", "", colnames(dy)[grep("_child", colnames(dy))])
        t = unique(tp, ts)
        if( length(which(t == "")) > 0  ) { t = t[-which(t == "")] }
        if( length(which(t == "seed_lot")) > 0  ) { t = t[-which(t == "seed_lot")] }
        t = t[c(which(t == "location"), which(t != "location"))]
        
        d_vertex = data.frame(c(as.character(dy[,paste(t[1], "_parent", sep = "")]), as.character(dy[,paste(t[1], "_child", sep = "")])))
        for(i in 2:length(t)){
          d_vertex = cbind.data.frame(
            d_vertex,
            c(as.character(dy[,paste(t[i], "_parent", sep = "")]), as.character(dy[,paste(t[i], "_child", sep = "")]))
          )
        }
        colnames(d_vertex) = t
        
        d_vertex_bis = data.frame(
          c(as.character(dy[,"relation_year_start"]), as.character(dy[,"relation_year_start"])),
          c(as.character(dy[,"relation_year_end"]), as.character(dy[,"relation_year_end"]))
        )
        colnames(d_vertex_bis) = c("relation_year_start", "relation_year_end")
        
        d_vertex = cbind.data.frame(d_vertex, d_vertex_bis)
        
        t = as.data.frame(with(d_vertex, table(location, relation_year_start)))
        colnames(t)[3] = "nb_sl"
        
        d_vertex = d_vertex[,-which(colnames(d_vertex) == "relation_year_start")]
        d_vertex = d_vertex[,-which(colnames(d_vertex) == "relation_year_end")]
        d_vertex = d_vertex[,-which(colnames(d_vertex) == "year")]
        d_vertex = d_vertex[,-which(colnames(d_vertex) == "germplasm")]
        d_vertex = join(d_vertex, t, by = "location")
        d_vertex = unique(d_vertex)
        d_vertex = d_vertex[,-which(colnames(d_vertex) == "relation_year_start")]
        
        dup = which(duplicated(d_vertex$year))
        if( length(dup) > 0 ){
          warning(paste("The following year are duplicated : ",
                        paste(as.character(d_vertex$year[dup]), collapse = ","),
                        ". Only one information per year has been kept.", sep = ""
          )
          )
          d_vertex = d_vertex[!duplicated(d_vertex$year),]
        }
        
        relation = unique(dy[,c("location_parent", "location_child")]) 
        
        out = list(d_vertex, relation); names(out) = c("d_vertex", "relation")
        OUT = c(OUT, list(out))
      }
      names(OUT) = vec_year
    }
    
    
  }
  
  # bipart ----------
  if( network_part == "bipart" ){
    
    d1 = check_unipart_sl_data(d, display_mess = FALSE)
    d2 = check_bipart_data(d, display_mess = FALSE)
    
    if( is.null(d1) & is.null(d2) ) { 
      stop(
        "With network = \"bipart\", two data are possible:\n
        1. the data must have the following columns : \n
        \"seed_lot_parent\", \"seed_lot_child\",\n
        \"relation_type\", \"relation_year_start\", \"relation_year_end\",\n
        \"germplasm_parent\", \"location_parent\", \"year_parent\",\n
        \"germplasm_child\", \"location_child\", \"year_child\". \n
        It can have in option : \"alt_parent\", \"long_parent\", \"lat_parent\",\n
        \"alt_child\", \"long_child\", \"lat_child\" to get map representation \n
        It can have supplementary variables with tags \"_parent\", \"_child\" or \"_relation\". \n
        \n OR \n
        2. the data must have the following colums :\n
        \"germplasm\", \"location\", \n
        It can have in option : \"year\" and \n
        \"alt\", \"long\", \"lat\" to get map representation "
      ) 
    }
    if( !is.null(d1) & is.null(d2) ) { d = unipart_sl_data_to_bipart_data(d) }
    if( is.null(d1) & !is.null(d2) ) { d = check_bipart_data(d) }
    
    if( !is.element("year", colnames(d)) ){ 
      d$year = "unknown_year" 
    }
    
    vec_year = sort(unique(c(as.character(d$year))))
    d_all_year = d
    d_all_year$year = paste(vec_year, collapse = "-")
    d = rbind.data.frame(d, d_all_year)
    vec_year = c(paste(vec_year, collapse = "-"), vec_year)
    
    OUT = NULL
    
    for(y in vec_year){
      dy = filter(d, year == y)
      
      d_vertex = data.frame(
        c(as.character(dy[,"germplasm"]), as.character(dy[,"location"])),
        c(as.character(dy[,"year"]), as.character(dy[,"year"])),
        c(rep("vertex_type_1", nrow(dy)), rep("vertex_type_2", nrow(dy)))
      )
      colnames(d_vertex) = c("vertex", "year", "type")
      d_vertex = unique(d_vertex)
      
      dup = which(duplicated(d_vertex$vertex))
      if( length(dup) > 0 ){
       warning(paste("The following vertex are duplicated : ",
                     paste(as.character(d_vertex$vertex[dup]), collapse = ","),
                     ". Only one information per vertex has been kept.", sep = ""
                     )
               )
       d_vertex = d_vertex[!duplicated(d_vertex$location),]
       }
      
      relation = unique(dy[,c("germplasm", "location")]) 
      
      out = list(d_vertex, relation); names(out) = c("d_vertex", "relation")
      OUT = c(OUT, list(out))
    }
    names(OUT) = vec_year
  }

  
  # Format igraph object ----------

  d = lapply(OUT, function(out){
    i = igraph::graph_from_data_frame(d = out$relation, directed = TRUE, vertices = out$d_vertex) 
    return(i)
    })
  
  class(d) <- c("PPBstats", "data_network")
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}
