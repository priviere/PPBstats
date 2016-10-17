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
get.PPBstats.data = function()
  # let's go !!! ---------- 
{
  system(paste("mkdir ", "data_PPBstats"))
  path_in = paste(getwd(), "/data_PPBstats/", sep = "")
  path_out = "https://www.dropbox.com/sh/6qvl515k5484zg4/AADZKkaM2XZvmr9e6l5aWxN2a?dl=0"
  download.file(url = path_out, destfile = path_in)
  message("The data are downloaded in ", path_in, "\nYou can now load the data, for example load(\"./data_PPBstats/out1.Rdata\").")
}

