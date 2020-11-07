#' Transform home away data to local foreign data
#' @param data_agro_HA data home away
#' @details change home to local and away to foreign
#' @return a data frame of class data_agro_LF
#' @importFrom methods is
#' @export
#'
HA_to_LF = function(data_agro_HA){
  if(!is(data_agro_HA, "data_agro_HA")){ stop(substitute(data_agro_HA), " must be formated with type = \"data_agro_HA\", see PPBstats::format_data_PPBstats().") }
  data_agro_LF = data_agro_HA
  data_agro_LF$version = as.character(data_agro_LF$version)
  data_agro_LF$version[which(data_agro_LF$version == "home")] = "local"
  data_agro_LF$version[which(data_agro_LF$version == "away")] = "foreign"
  data_agro_LF$version = as.factor(data_agro_LF$version)
  class(data_agro_LF) = c("PPBstats", "data_agro_LF", "data.frame")
  return(data_agro_LF)
}
