# 0. help ----------
#' Provides experimental plan for several situations
#'
#' @description
#' \code{plan_experiment} provides experimental plan for several situations
#' 
#' @param expe.type The type of experiment to settle "satellite-farm", "regional-farm", "row-column", "fully-repicated", "IBD".
#' 
#' @param nb.entries Number of entries
#' 
#' @param nb.controls.per.block Number of controls per blocks.
#' 
#' @param nb.blocks Number of blocks
#' 
#' @param nb.cols Number of columns in the design. The number of rows is computed automaticaly
#'
#' @return 
#' The function returns a list with
#' \itemize{
#'  \item A data frame with entries, block, X, Y
#'  \item A picture of the experimental plan
#'  }
#' 
#' @details 
#' Note that expe.type = "row-column" is particular case of expe.type = "regional-farm" where the number of controls must be at least the number of columns or rows.
#' 
#' @author Pierre Riviere
#' 
plan_experiment = function(
  expe.type,
  nb.entries,
  nb.controls.per.block,
  nb.blocks,
  nb.cols
)
  # let's go !!! ----------
  {
    # 1. Error message ----------  
    if(!is.element(expe.type, c("satellite-farm", "regional-farm", "row-column", "fully-repicated", "IBD"))) { stop("expe.type must be either \"satellite-farm\", \"regional-farm\", \"row-column\", \"fully-repicated\" or \"IBD\".") }
    
    # 2. Functions used in the code ----------  
    
    get_data.frame = function(nb.entries, nb.blocks, nb.controls.per.block, nb.cols, expe.type) {
      entries = paste("entry-", c(1:nb.entries), sep = "")
      entries = sample(entries, length(entries), replace = FALSE)
      
      test = ceiling(nb.entries / nb.blocks) * nb.blocks
      if( test > nb.entries ) { entries = c(entries, rep("XXX", times = (test - nb.entries))) }
      
      l = split(entries, (1:nb.blocks))
      
      if( expe.type == "row-column" | expe.type == "satellite-farm") {
        l = lapply(l, function(x){c(x, rep("control", times = nb.controls.per.block))})  
      }
      
      if( expe.type == "regional-farm" ) {
        l = lapply(l, function(x){c(x, paste("control", c(1:nb.controls.per.block), sep ="-"))})
      }
      
      L = rep(LETTERS, times = 30)
      vec_X = c(LETTERS, paste(L, rep(c(1:(length(L)/26)), each = 26), sep = ""))
      
      vec_Y = c(1:(nb.entries*2)) # to be ok, it is always less than nb.entries*2
      d = data.frame()
      for(i in 1:length(l)){
        entries = l[[i]]
        nb.rows = ceiling(length(entries) / nb.cols)
        X = rep(vec_X[1:nb.cols], each = nb.rows)
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
    
    place_controls = function(d, expe.type){
      dok = data.frame()
      vec_block = levels(d$block)
      for(b in vec_block){
        dtmp = droplevels(filter(d, block == b))
        c = grep("control", dtmp$entries)
        e = c(1:length(dtmp$entries))[-c]
        ent = c(as.character(dtmp$entries[c]), as.character(dtmp$entries[e]))

        # Put at least one control per row
        arrange.m = function(m){
          if( nrow(m) > 1 ){
            mtmp = m
            mtmp[2,] = m[nrow(m),]
            mtmp[nrow(m),] = m[2,]
            m = mtmp # be sur to have controls in opposite rows
          }
          return(m)
        }
        
        if( nlevels(dtmp$X) <= nlevels(dtmp$Y)){
          m = matrix(ent, ncol = nlevels(dtmp$X), nrow = nlevels(dtmp$Y)) 
          m = arrange.m(m)
          rownames(m) = levels(dtmp$Y)
          colnames(m) = levels(dtmp$X)
        } else { 
          m = matrix(ent, ncol = nlevels(dtmp$Y), nrow = nlevels(dtmp$X)) 
          m = arrange.m(m)
          rownames(m) = levels(dtmp$X)
          colnames(m) = levels(dtmp$Y)
          }
        
        # For each row, put control in different column
        possible_col = rep(1:ncol(m), nrow(m))

        for(i in 1:nrow(m)){
          r = m[i,]
          c = grep("control", r)
          
          if(length(c)==0){ e = c(1:length(r)) } else { e = c(1:length(r))[-c] }
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
          m[i,col_with_e] = sample(r[e])
        }
        
        if( nlevels(dtmp$X) <= nlevels(dtmp$Y)){ m = m } else { m = t(m) }
        
        # Sample columns and rows
        sample_col_row = function(m, expe.type){
          # Sample the columns
          m2 = m[,sample(c(1:ncol(m)))]
          if(is.vector(m2)){ m2 = matrix(m2, nrow = nrow(m))}
          colnames(m2) = sort(colnames(m))
          rownames(m2) = rownames(m)
          m = m2
          
          # Sample the rows
          if( expe.type != "satellite-farm" ){
            m2 = m[sample(c(1:nrow(m))),]
            if(is.vector(m2)){ m2 = matrix(m2, nrow = nrow(m))}
            colnames(m2) = sort(colnames(m))
            rownames(m2) = rownames(m)
            m = m2
          }
        }
        m = sample_col_row(m, expe.type)
        
        # Check controls do not touch each other
        check_controls = function(m){
          test = c()
          for(i in 1:(nrow(m)-1)){
            for(j in 1:ncol(m)){
              if(m[i,j] == m[i+1,j]){ test = c(test, TRUE) } else { test = c(test, FALSE) }
            }
          }
          for(j in 1:(ncol(m)-1)){
            for(i in 1:nrow(m)){
              if(m[i,j] == m[i,j+1]){ test = c(test, TRUE) } else { test = c(test, FALSE) }
            }
          }
          go = length(which(test))>0
          return(go)
        }
        
        i = 1
        while(check_controls(m)& i < 1000){ m = sample_col_row(m, expe.type); i = i +1 }

          
        # Check number of controls in col and row
        fun_test = function(x){
          a = grep("control", x)
          if(length(a)==0){t=0}else{t=1}
          return(t)
        }
        test_col = which(apply(m, 2, fun_test) == 0)
        test_row = which(apply(m, 1, fun_test) == 0)
          
        mess_col = paste("Controls are missing in columns ", paste(test_col, collapse = ","), ". You can rise nb.controls.per.block.", sep = "")
        mess_row = paste("Controls are missing in rows ", paste(test_row, collapse = ","), ". You can rise nb.controls.per.block.", sep = "")
          
        if( expe.type == "regional-farm" ){
          if( length(test_col) > 0 ){ warning(mess_col) }
          if( length(test_row) > 0 ){ warning(mess_row) }
        }
          
        if( expe.type == "row-column" ){
          if( length(test_col) > 0 ){ stop(mess_col) }
          if( length(test_row) > 0 ){ stop(mess_row) }
        }
        
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
      color_till[grep("control", d$entries)] = "black"
      
      color_text = color_till
      b = which(color_till == "black")
      w = which(color_till == "white")
      color_text[w] = "black"
      color_text[b] = "white"
      
      a = tapply(as.numeric(d$X), d$block, min) - 0.5
      d$xmin = a[d$block]
      a = tapply(as.numeric(d$X), d$block, max) + 0.5
      d$xmax = a[d$block]
      a = tapply(as.numeric(d$Y), d$block, min) - 0.45
      d$ymin = a[d$block]
      a = tapply(as.numeric(d$Y), d$block, max) + 0.45
      d$ymax = a[d$block]
      
      p = ggplot(data = d, aes(x = X, y = Y, label = entries))
      p = p + geom_tile(color = "black", fill = color_till) + geom_text(color = color_text) + theme(legend.position="none") + theme_bw()
      p = p + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = block), fill = NA, size = 1)
      
      return(p)        
    }
    
    
    # 3. Compute for different expe.type ----------
    
    OUT = NULL
    
    # 3.1. expe.type == "satellite-farm" ----------
    if( expe.type == "satellite-farm" ) {
      if(nb.entries > 10){ message("With expe.type == \"satellite-farm\", it is recommanded to have less than 10 entries. With more than 10 entries, go for expe.type == \"regional-farm\".") }
      if(nb.controls.per.block!=2){stop("nb.controls.per.block = 2 with expe.type == \"satellite-farm\".")}
      if(nb.blocks != 1){stop("nb.blocks = 1 with expe.type == \"satellite-farm\".")}
      if(nb.cols > 2){stop("nb.cols = 1 or 2 with expe.type == \"satellite-farm\".")}
      
      d = get_data.frame(nb.entries, nb.blocks, nb.controls.per.block, nb.cols, expe.type)
      d = place_controls(d, expe.type)
      p = get_ggplot_plan(d)
      
      out = list("data.frame" = d, "plan" = p)
      out = list("satellite-farms" = out); OUT = c(OUT, out)
    }
    
    # 3.2. expe.type == "regional-farm" ----------
    if( expe.type == "regional-farm" ) {
      if( nb.controls.per.block < 2) { stop("nb.controls.per.block must be more than 1 with expe.type == \"regional-farm\".") }
      
      d = get_data.frame(nb.entries, nb.blocks, nb.controls.per.block, nb.cols, expe.type)
      d = place_controls(d, expe.type)
      p = get_ggplot_plan(d)
      
      out = list("data.frame" = d, "plan" = p)
      out = list("regional-farms" = out); OUT = c(OUT, out)
    }
    
    
    # 3.3. expe.type == "row-column" ----------
    if( expe.type == "row-column" ) {
      d = get_data.frame(nb.entries, nb.blocks, nb.controls.per.block, nb.cols, expe.type)
      d = place_controls(d, expe.type)
      p = get_ggplot_plan(d)
      
      out = list("data.frame" = d, "plan" = p)
      out = list("row-column" = out); OUT = c(OUT, out)
    }
    
    
    # 3.4. expe.type == "fully-repicated" ----------
    
    
    # 3.5. expe.type == "IBD" ----------
    if( expe.type == "IBD" ) {
      d = ibd(v = nb.entries, b = nb.blocks, k = nb.cols)$design
      if( is.null(nrow(d)) ){ stop("Design not found") }

      d = data.frame(
        entries = as.vector(d), 
        block = rep(c(1:nrow(d)), times = ncol(d)),
        X = rep(LETTERS[1:ncol(d)], each = nrow(d)),
        Y = rep(c(1:nrow(d)), times = ncol(d))
      )
      d$entries = as.factor(d$entries)
      d$block = as.factor(d$block)
      d$X = as.factor(d$X)
      d$Y = as.factor(d$Y)
      
      p = get_ggplot_plan(d)
      
      out = list("data.frame" = d, "plan" = p)
      out = list("IBD" = out); OUT = c(OUT, out)
    }
     
    return(OUT)
    }
