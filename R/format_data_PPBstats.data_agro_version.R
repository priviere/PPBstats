format_data_PPBstats.data_agro_version = function(data){
  d = data
  
  mess = "The following column are compulsory : \"year\", \"germplasm\", \"location\", \"group\", \"version\"."
  # check columns
  if(!is.element("year", colnames(d))) { stop(mess) }
  if(!is.element("germplasm", colnames(d))) { stop(mess) }
  if(!is.element("location", colnames(d))) { stop(mess) }
  if(!is.element("group", colnames(d))) { stop(mess) }
  if(!is.element("version", colnames(d))) { stop(mess) }
  
  mess = "The following column must be set as factor : \"year\", \"germplasm\", \"location\", \"group\", \"version\"."
  if(!is.factor(d$year)) { stop(mess) }
  if(!is.factor(d$germplasm)) { stop(mess) }
  if(!is.factor(d$location)) { stop(mess) }
  if(!is.factor(d$group)) { stop(mess) }
  if(!is.factor(d$version)) { stop(mess) }
  
  class(d) <- c("PPBstats", "data_agro_version", "data.frame")
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}

