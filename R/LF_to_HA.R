#' transform local foreign data to home away data
#' @param data_agro_LF data local foreign
#' @details change local to home and foreign to away
#' @return a data frame of class data_agro_HA
#' @importFrom methods is
#' @export
#'
LF_to_HA <- function(data_agro_LF){
  if(!is(data_agro_LF, "data_agro_LF")){ stop(substitute(data_agro_LF), " must be formated with type = \"data_agro_LA\", see PPBstats::format_data_PPBstats().") }
  data_agro_HA = data_agro_LF
  data_agro_HA$version = as.character(data_agro_HA$version)
  data_agro_HA$version[which(data_agro_HA$version == "local")] = "home"
  data_agro_HA$version[which(data_agro_HA$version == "foreign")] = "away"
  data_agro_HA$version = as.factor(data_agro_HA$version)
  class(data_agro_HA) = c("PPBstats", "data_agro_HA", "data.frame")
  return(data_agro_HA)
}
