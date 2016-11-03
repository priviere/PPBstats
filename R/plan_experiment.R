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
    
    get_data.frame = function(nb.entries, nb.blocks, nb.controls, nb.cols) {
      entries = paste("entry-", c(1:nb.entries), sep = "")
      entries = sample(entries, length(entries), replace = FALSE)
      
      test = ceiling(nb.entries / nb.blocks) * nb.blocks
      if( test > nb.entries ) { entries = c(entries, rep("XXX", times = (test - nb.entries))) }
      
      l = split(entries, (1:nb.blocks))
      l = lapply(l, function(x){c(x, rep("control", times = nb.controls))})
      
      L = rep(LETTERS, times = 30)
      vec_X = c(LETTERS, paste(L, rep(c(1:(length(L)/26)), each = 26), sep = ""))
      
      vec_Y = c(1:(nb.entries*2)) # to be ok, it is always less than nb.entries*2
      d = data.frame()
      for(i in 1:length(l)){
        entries = l[[i]]
        nb.rows = ceiling(length(entries) / nb.cols)
        X = rep(vec_X[1:nb.cols], each = nb.rows); vec_X = vec_X[-c(1:nb.cols)]
        Y = rep(vec_Y[c(1:nb.rows)], times = nb.cols); vec_Y = vec_Y[-c(1:nb.rows)]
        if( length(X) > length(entries) ) { entries = c(entries, rep("XXX", times = length(X) - length(entries)))}
        block = rep(i, length(X))
        d = rbind.data.frame(d, cbind.data.frame(entries, block, X, Y))
      }
      d$entries = as.factor(d$entries)
      d$block = as.factor(d$block)
      d$X = as.factor(d$X)
      d$Y = as.factor(d$Y)
      return(d)
    }
    
    place_controls = function(d){
      dok = data.frame()
      vec_block = levels(d$block)
      for(b in vec_block){
        dtmp = droplevels(filter(d, block == b))
        ent = c(as.character(dtmp$entries[which(dtmp$entries=="control")]), as.character(dtmp$entries[which(dtmp$entries!="control")]))
        # Put at least one control per row
        m = matrix(ent, ncol = nlevels(dtmp$X), nrow = nlevels(dtmp$Y)) 
        mtmp = m
        if( nrow(m) > 1 ){
          mtmp[2,] = m[nrow(m),]
          mtmp[nrow(m),] = m[2,]
          m = mtmp # be sur to have controls in opposite rows
        }
        rownames(m) = levels(dtmp$Y)
        colnames(m) = levels(dtmp$X)
        
        # For each row, put control in different column
        possible_col = rep(sample(1:ncol(m)), times = nrow(m))
        
        for(i in 1:nrow(m)){
          r = m[i,]
          c = which(r=="control")
          e = which(r!="control")
          if(length(c)>1){ e = c(e, c[2:length(c)]); c = c[1]}
          
          if(length(c)==0){
            col_with_c = NULL
            col_with_e = c(1:ncol(m))
          } else {
            col_with_c = possible_col[i]
            col_with_e = c(1:ncol(m))
            col_with_e = col_with_e[-which(col_with_e==col_with_c)]
          }
          
          if(!is.null(col_with_c)){ m[i,col_with_c] = r[c]}
          m[i,col_with_e] = r[e]
        }

        # Sample the columns
        m2 = m[,sample(c(1:ncol(m)))]
        if(is.vector(m2)){ m2 = matrix(m2, nrow = nrow(m))}
        colnames(m2) = sort(colnames(m))
        rownames(m2) = rownames(m)
        m = m2

        dtmp = data.frame(entries = as.vector(m), block = b, X = rep(colnames(m), each = nrow(m)), Y = rep(rownames(m), times = ncol(m)))
        
        dok = rbind.data.frame(dok, dtmp)
      }
      
      dok$entries = as.factor(dok$entries)
      dok$block = as.factor(dok$block)
      dok$X = as.factor(dok$X)
      dok$Y = as.factor(dok$Y)
      
      return(dok)
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
      nb.controls = 2; message("nb.controls = 2 with expe.type == \"satellite-farm\".")
      nb.blocks = 1; message("nb.blocks = 1 with expe.type == \"satellite-farm\".")
      nb.cols = 2; message("nb.cols = 1 with expe.type == \"satellite-farm\".")
      
      d = get_data.frame(nb.entries, nb.blocks, nb.controls, nb.cols)
      d = place_controls(d)
      p = get_ggplot_plan(d)
      
      out = list("data.frame" = d, "plan" = p)
      out = list("satellite-farms" = out); OUT = c(OUT, out)
    }
    
    # 3. expe.type == "regional-farm" ----------
    if( expe.type == "regional-farm" ) {
      if( nb.blocks < 2) { stop("nb.blocks must be more than 1 with expe.type == \"regional-farm\".") }
      if( nb.controls < 2) { stop("nb.controls must be more than 1 with expe.type == \"regional-farm\".") }
      
      d = get_data.frame(nb.entries, nb.blocks, nb.controls, nb.cols)
      d = place_controls(d)
      p = get_ggplot_plan(d)
      
      out = list("data.frame" = d, "plan" = p)
      out = list("regional-farms" = out); OUT = c(OUT, out)
    }
    
    
    # 4. expe.type == "row-columns" ----------
    if( expe.type == "row-columns" ) {
      if( nb.blocks != 1) { stop("nb.blocks must be 1 with expe.type == \"row-columns\".") }

      d = get_data.frame(nb.entries, nb.blocks, nb.controls, nb.cols)
      d = place_controls(d)
      p = get_ggplot_plan(d)
      
      out = list("data.frame" = d, "plan" = p)
      out = list("regional-farms" = out); OUT = c(OUT, out)
    }
    
    
    # 5. expe.type == "fully-repicated" ----------
    
    # 6. expe.type == "IBD" ----------
 
    return(OUT)
    }
