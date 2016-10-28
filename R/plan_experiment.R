# 0. help ----------
#' Provides experimental plan for several situations
#'
#' @description
#' \code{plan_experiment} provides experimental plan for several situations
#' 
#' @param expe.type The type of experiment to settle "satellite-farm", "regional-farm", "row-columns", "fully-repicated", "IBD".
#' 
#' @param nb.entries Number of entries
#' 
#' @param nb.controls Number of controls. This is useful only for expe.type "satellite-farm" and "regional-farm"
#' 
#' @param nb.blocks Number of blocks
#' 
#' @param nb.rows Number of rows
#'
#' @return 
#' The function returns a list with
#' \itemize{
#'  \item A data frame with X, Y and block
#'  \item A picture of the experimental plan, which is also exported to csv files
#'  }
#' 
#' @author Pierre Riviere
#' 
plan_experiment = function(
  expe.type,
  nb.entries,
  nb.controls,
  nb.blocks,
  nb.rows
)
  # let's go !!! ----------
  {
    # 1. Error message ----------  
    
    # 2. expe.type == "satellite-farm" ----------
    if( expe.type == "satellite-farm" ) {
      nb.entries = 10
      nb.controls = 1; message("nb.controls = 1 with expe.type == \"satellite-farm\".")
      nb.blocks = 1; message("nb.blocks = 1 with expe.type == \"satellite-farm\".")
      nb.rows = 5
      
      entries = paste("entry-", c(1:nb.entries), sep = "")
      entries = c("control", sample(entries, length(entries), replace = FALSE), "control")
      nb.cols = ceiling(length(entries) / nb.rows)
      X = rep(LETTERS[1:nb.rows], times = nb.cols)
      Y = rep(c(1:nb.cols), each = nb.rows)
      block = rep(1, times = length(Y))
      if( length(entries) < length((Y))) { entries = c(entries, rep("", length(Y)-length(entries))) }
      d = cbind.data.frame(entries, block, X, Y)
    }
    
    # 3. expe.type == "regional-farm" ----------
    # 4. expe.type == "row-columns" ----------
    # 5. expe.type == "fully-repicated" ----------
    # 6. expe.type == "IBD" ----------
 
    return(OUT)
    }
