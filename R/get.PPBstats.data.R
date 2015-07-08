# 0. help ----------
#' Get PPBstats datas to run example of the vignette
#'
#' @description
#' \code{get.PPBstats.data} download Rdata from internet. This is because Rdata file are too big to be stored in the package.
#'
#'@param folder_name name of the folder where the RData will be. "data" by default.
#'
#' @details
#' It creates a PPBstats_data folder in the current directory and download the Rdata files
#' 
#' @author Pierre Riviere
#' 
#' 
#' 
get.PPBstats.data = function(folder_name = "data")
  # let's go !!! ---------- 
{
  #system(paste("mkdir ",folder_name"))
  #download.file(url = "todo", destfile = folder_name)
  #message("The data are downloaded in ", getwd(), "/", folder_name)
}

