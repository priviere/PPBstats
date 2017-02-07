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
{
  # function from Kay Cichini
  # https://www.r-bloggers.com/download-files-from-dropbox-programmatically-with-r/
  dl_from_dropbox <- function(x, key) {
    bin <- RCurl::getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x), ssl.verifypeer = FALSE)
    con <- file(x, open = "wb")
    writeBin(bin, con)
    close(con)
  }
  
  system(paste("mkdir ", "data_PPBstats"))
  vec_files = c("comp.mu.RData", "out1.RData", "out.cv.RData", "out.model1.RData", "out.model2_y1.RData", "out.model2_y3.RData", "out1_bis.RData", "out2.RData", "out.model1_bis.RData", "out.model2.RData", "out.model2_y2.RData", "out.predict.the.past.RData")
  
  setwd("data_PPBstats")
  
  pb <- txtProgressBar(min = 0, max = length(vec_files), style = 3)
  for(i in c(1:length(vec_files))){
    dl_from_dropbox(vec_files[i], "6qvl515k5484zg4")  
    setTxtProgressBar(pb, i)
  }
  close(pb)
  setwd("../")

  message("The data are downloaded in ./data_PPBstats/ \nYou can now load the data, for example load(\"./data_PPBstats/out1.Rdata\").")
}

