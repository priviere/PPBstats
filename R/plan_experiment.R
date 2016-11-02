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
#' @param nb.cols Number of columns
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
  nb.cols
)
  # let's go !!! ----------
  {
    # 1. Error message ----------  
    
    OUT = NULL
    
    get_data.frame = function(entries, block, X, Y) {
      if( length(entries) < length((Y))) { entries = c(entries, rep("", length(Y)-length(entries))) }
      d = cbind.data.frame(entries, block, X, Y)
      d$entries = as.factor(d$entries)
      d$block = as.factor(d$block)
      d$X = as.factor(d$X)
      d$Y = as.factor(d$Y)
      return(d)
    }
    
    get_ggplot_plan = function(d){
      color_till = rep("white", length(d$entries))
      color_till[which(d$entries == "control")] = "black"
      
      color_text = color_till
      b = which(color_till == "black")
      w = which(color_till == "white")
      color_text[w] = "black"
      color_text[b] = "white"
      
      p = ggplot(d, aes(x = X, y = Y, label = entries)) + geom_tile(color = "black", fill = color_till) + geom_text(color = color_text) + theme(legend.position="none") + theme_bw()
      
      # to do: entourer les blocks
      
      return(p)        
    }
    
    # 2. expe.type == "satellite-farm" ----------
    if( expe.type == "satellite-farm" ) {
      nb.controls = 1; message("nb.controls = 1 with expe.type == \"satellite-farm\".")
      nb.blocks = 1; message("nb.blocks = 1 with expe.type == \"satellite-farm\".")
      nb.cols = 2; message("nb.cols = 1 with expe.type == \"satellite-farm\".")
      
      entries = paste("entry-", c(1:nb.entries), sep = "")
      entries = c("control", sample(entries, length(entries), replace = FALSE), "control")
      nb.rows = ceiling(length(entries) / nb.cols)
      X = rep(LETTERS[1:nb.cols], each = nb.rows)
      Y = rep(c(1:nb.rows), times = nb.cols)
      block = rep("block 1", times = length(Y))
      
      d = get_data.frame(entries, block, X, Y)
     
      p = get_ggplot_plan(d)
      
      out = list("data.frame" = d, "plan" = p)
      out = list("satellite-farms" = out); OUT = c(OUT, out)
    }
    
    # 3. expe.type == "regional-farm" ----------
    if( expe.type == "regional-farm" ) {
      nb.entries = 21
      nb.controls = 2
      nb.blocks = 2
      nb.cols = 3
      
      entries = paste("entry-", c(1:nb.entries), sep = "")
      entries = sample(entries, length(entries), replace = FALSE)
      
      test = ceiling(nb.entries / nb.blocks) * nb.blocks
      if( test > nb.entries ) { entries = c(entries, rep("XXX", times = (test - nb.entries))) }
      
      l = split(entries, (1:nb.blocks))
      l = lapply(l, function(x){c(x, rep("control", times = nb.controls))})
      
      vec_Y = c(1:(nb.entries*2)) # to be ok, it is always less than nb.entries*2
      d = data.frame()
      for(i in 1:length(l)){
        entries = l[[1]]
        nb.rows = ceiling(length(entries) / nb.cols)
        X = rep(LETTERS[1:nb.cols], each = nb.rows)
        Y = rep(vec_Y[c(1:nb.rows)], times = nb.cols); vec_Y = vec_Y[-c(1:nb.rows)]
        if( length(X) > length(entries) ) { entries = c(entries, rep("XXX", times = length(X) - length(entries)))}
        block = rep(i, length(X))
        d = rbind.data.frame(d, cbind.data.frame(entries, block, X, Y))
      }
      d$entries = as.factor(d$entries)
      d$block = as.factor(d$block)
      d$X = as.factor(d$X)
      d$Y = as.factor(d$Y)
      
      # faire une fonction pour mettre en ligne et en colonnes (utilisée aussi dans row-columns avec block = 1)
      
      p = get_ggplot_plan(d)
      
      out = list("data.frame" = d, "plan" = p)
      out = list("regional-farms" = out); OUT = c(OUT, out)
    }
    
    
    # 4. expe.type == "row-columns" ----------
    # 5. expe.type == "fully-repicated" ----------
    # 6. expe.type == "IBD" ----------
 
    return(OUT)
    }
