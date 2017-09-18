format_data_PPBstats.data_agro = function(data){
  d = data
  
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
      v = format(as.Date(d[, vec_date[i]]), format = "%Y/%m/%d")
      vjd = sapply(v, function(x) {
        julian(as.Date(x), origin = as.Date(paste(unlist(strsplit(x, "/"))[1], "/01/01", sep = "")))
      })
      d = cbind.data.frame(d, vjd)
      colnames(d)[ncol(d)] = paste(vec_date[i], "_julian", sep = "")
    }
  }
  
  class(d) <- c("PPBstats", "data.frame", "data_agro")
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}