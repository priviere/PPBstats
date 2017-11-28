format_data_PPBstats.data_organo_hedonic = function(data,code, threshold){
  d = data
  
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
  
  class(d) <- c("PPBstats", "data_organo_hedonic", "data.frame")
  message(substitute(data), " has been formated for PPBstats functions.")
  return(d)
}

  
  
