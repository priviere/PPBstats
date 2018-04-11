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
  
  test_1 = length(which(is.element(c("S", "R"), d$group)))
  test_2 = length(which(is.element(c("bouquet", "vrac"), d$version)))
  test_3 = length(which(is.element(d$location, d$group)))
  test_4 = length(which(is.element(c("residant", "migrant"), d$version)))
  
  if( test_1 > 0 & test_2 > 0) {   class(d) <- c("PPBstats", "data_agro_version_SR", "data.frame") }
  if( test_3 > 0 & test_4 > 0) {   class(d) <- c("PPBstats", "data_agro_version_MR", "data.frame") }
  if( test_1 == 0 & test_2 == 0 & test_3 ==0 & test_4 == 0) { class(d) <- c("PPBstats", "data_agro_version", "data.frame") }
  
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}

