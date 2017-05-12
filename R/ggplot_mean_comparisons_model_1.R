#' Get ggplot from mean_comparisons_model_1
#'
#' @description
#' \code{ggplot_mean_comparisons_model_1} returns ggplot from \code{\link{mean_comparisons_model_1}}
#' 
#' @param out_mean_comparisons_model_1 outputs from \code{\link{mean_comparisons_model_1}}
#' 
#' @details See \code{\link{get_ggplot}}
#' 
#' @return See \code{\link{get_ggplot}}
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{get_ggplot}}, 
#' \item \code{\link{mean_comparisons_model_1}}
#' }
#' 
#'
ggplot_mean_comparisons_model_1 = function(
  out_mean_comparisons_model_1,
  data_version = NULL,
  ggplot.type = "interaction",
  nb_parameters_per_plot = 10
  ){
  
  # 1. Error message ----------
  if( !is.element(ggplot.type, c("interaction", "barplot", "score")) ) { stop("ggplot.type must be score, barplot or interaction with output from mean_comparisons and model_1") }
  
  # 2. get data ----------
  all_data = list(
    "data_mean_comparisons" = out_mean_comparisons_model_1$data_mean_comparisons,
    "data_env_with_no_controls" = out_mean_comparisons_model_1$data_env_with_no_controls,
    "data_env_whose_param_did_not_converge" = out_mean_comparisons_model_1$data_env_whose_param_did_not_converge
  )
  
  # 3. get ggplot ----------
  
  # 3.1. function used in the code ----------
  add_stars_version = function(dx, data, data_version){
    # Add letters of significant groups
    env = dx$environment[1]
    if( !is.null(data) ){ data_Mpvalue_env = data[[env]]$Mpvalue } else { data_Mpvalue_env = NULL }
    data_version_tmp = droplevels(filter(data_version, environment == env))
    gp = unique(data_version_tmp$group)
    
    STARS = NULL
    for(g in gp){
      dtmp = droplevels(filter(data_version_tmp, group == g))
      vec_version = levels(dtmp$version)
      v1 = unique(as.character(filter(dtmp, version == vec_version[1])$mu))
      v2 = unique(as.character(filter(dtmp, version == vec_version[2])$mu))
      
      if( !is.null(data_Mpvalue_env) ){ 
        for (i in 1:ncol(data_Mpvalue_env)) { 
          if (colnames(data_Mpvalue_env)[i] == v1) {c1 = i}
          if (colnames(data_Mpvalue_env)[i] == v2) {c2 = i}
        }
        pvalue = data_Mpvalue_env[min(c1,c2), max(c1,c2)]
      } else {
        if( length(v1) > 1 & length(v2) > 1) {
          pvalue = t.test(v1, v2)$p.value
        } else { 
          pvalue = NULL
          warning(attributes(data)$PPBstats.object, ": no t.test are done as there are not enough observations.") 
        }
      }
      
      if(is.null(pvalue)) { stars = " "} else {
        if(pvalue < 0.001) { stars = "***" }
        if(pvalue > 0.001 & pvalue < 0.05) { stars = "**" }
        if(pvalue > 0.05 & pvalue < 0.01) { stars = "*" }
        if(pvalue > 0.01) { stars = "." }
      }
      names(stars) = g
      STARS = c(STARS, stars)
    }
    
    colnames(dx)[which(colnames(dx) == "parameter")] = "mu"
    d = join(data_version_tmp, dx, by = "mu")
    
    # delete version where there are not v1 AND v2
    group_to_keep = NULL
    vec_group = unique(d$group)
    
    for(gp in vec_group){
      d_tmp = droplevels(d[d$group %in% gp,])
      t = tapply(d_tmp$median, d_tmp$version, mean, na.rm = TRUE)
      if(!is.na(t[1]) & !is.na(t[2])){ group_to_keep = c(group_to_keep, gp)} 
    }
    
    d = droplevels(d[d$group %in% group_to_keep,])
    STARS = STARS[is.element(names(STARS), group_to_keep)]
    
    p = ggplot(d, aes(x = group, y = median)) + geom_bar(aes(fill = version), stat = "identity", position = "dodge")
    y = tapply(d$median, d$group, mean, na.rm = TRUE)
    y = y + (max(y) * 0.2)
    label_stars = data.frame(group = names(STARS), median = y[names(STARS)], STARS = STARS)
    p = p + geom_text(data = label_stars, aes(label = STARS))
    p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + coord_cartesian(ylim = c(0, dx[1,"max"]))
    return(p)
  }
  
  
  get.loc.year = function(data, nb_parameters_per_plot){
    
    if( length(data) > 0 ) {
      dtmp = data.frame()
      for(i in 1:length(data)){ dtmp = rbind.data.frame(dtmp, data[[i]]$mean.comparisons) }
      data = dtmp
      
      d_loc = plyr:::splitter_d(data, .(location))
      
      d_loc_b = lapply(d_loc, function(x){
        x = arrange(x, entry)
        x$split = as.numeric(factor(x$entry))
        seq_nb_para = unique(c(seq(1, max(x$split), nb_parameters_per_plot), max(x$split)*2))
        for(i in 1:(length(seq_nb_para) - 1) ) { x$split[seq_nb_para[i] <= x$split & x$split < seq_nb_para[i+1]] = i }
        x_split = plyr:::splitter_d(x, .(split))
        return(x_split)
      } )
    } else { d_loc_b = NULL }
    
    return(d_loc_b)
  }
  
  
  
  # 3.2. run function for barplot ----------
  
  if( ggplot.type == "barplot") {
    
    fun_barplot = function(data, data_version, nb_parameters_per_plot){
      if(!is.null(data_version)) {
        data_version$environment = paste(data_version$location, ":", data_version$year, sep = "")
        data_version$mu = paste("mu[", data_version$germplasm, ",", data_version$environment, "]", sep = "")
        vec_env = unique(data_version$environment)
        vec_env_to_get = vec_env[is.element(vec_env, names(data))]
        vec_env_not_to_get = vec_env[!is.element(vec_env, names(data))]
        if( length(vec_env_not_to_get) > 0 ){ 
          warning(attributes(data)$PPBstats.object, ": the following environments in data_version are not taken: ", paste(vec_env_not_to_get, collapse = ", "),".") 
          }
        
        if( length(vec_env_to_get) == 0 ) { 
          OUT = NULL
          warning(attributes(data)$PPBstats.object, ": there are no environment to display") 
        } else {
          d_env = data[vec_env_to_get]
          
          fun = function(x, data_version, nb_parameters_per_plot){
            x = x$mean.comparisons
            p_to_get = filter(data_version, environment == x$environment[1])$mu
            x = filter(x, parameter %in% p_to_get)
            x$max = max(x$median, na.rm = TRUE)
            x = arrange(x, parameter)
            x$split = add_split_col(x, nb_parameters_per_plot)
            x_split = plyr:::splitter_d(x, .(split))
            return(x_split)
          }
          
          d_env_b = lapply(d_env, fun, data_version, nb_parameters_per_plot)
          
          fun_barplot_version = function(dx, data, data_version){
            if(attributes(data)$PPBstats.object == "data_mean_comparisons") { 
              p = add_stars_version(dx, data, data_version)
            }
            
            if(attributes(data)$PPBstats.object == "data_env_with_no_controls" |
               attributes(data)$PPBstats.object == "data_env_whose_param_did_not_converge") {
              p = add_stars_version(dx, data = NULL, data_version)
            }
            
            return(p)
          }
          
          fun1 = function(x, data){ lapply(x, fun_barplot_version, data, data_version) }
          
          OUT = lapply(d_env_b, fun1, data)
          names(OUT) = names(d_env_b)
          }
        
      
        } else {
        
        d_env_b = lapply(data, function(x){
          x = x$mean.comparisons
          x = arrange(x, median)
          x$max = max(x$median, na.rm = TRUE)
          x$split = add_split_col(x, nb_parameters_per_plot)
          x_split = plyr:::splitter_d(x, .(split))
          return(x_split)
        } )
        
        OUT = lapply(d_env_b, function(x){
          out = lapply(x, function(dx){
            p = ggplot(dx, aes(x = reorder(parameter, median), y = median)) + geom_bar(stat = "identity")
            
            if(attributes(data)$PPBstats.object == "data_mean_comparisons") { 
              # Add letters of significant groups
              p = p + geom_text(data = dx, aes(x = reorder(parameter, median), y = median/2, label = groups), angle = 90, color = "white")
              p = p + ggtitle(paste(dx[1, "environment"], "\n alpha = ", dx[1, "alpha"], "; alpha correction :", dx[1, "alpha.correction"])) + ylab("")
            }
            
            if(attributes(data)$PPBstats.object == "data_env_with_no_controls" |
               attributes(data)$PPBstats.object == "data_env_whose_param_did_not_converge") {
              p = p + ggtitle(dx[1, "environment"]) + ylab("")
            }
            
            p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, dx[1,"max"])
            return(p)
          })
          return(out)
        })
        names(OUT) = names(d_env_b)
      }
      return(OUT)
    }
    
    out = lapply(all_data, fun_barplot, data_version, nb_parameters_per_plot)
    names(out) = names(all_data)
    
  }
  
  # 3.3. run function for score ----------
  
  if(ggplot.type == "score") {  
    
    d_loc_out = get.loc.year(all_data$data_mean_comparisons, nb_parameters_per_plot)
    
    if( !is.null(d_loc_out) ) {
      out = lapply(d_loc_out, function(x){
        lapply(x, function(env){
          # assign a number according to the group
          vec_letters = sort(unique(unlist(sapply(as.character(env[,"groups"]), function(x){unlist(strsplit(x, ""))}))), decreasing = TRUE)
          
          SCORE = c(1:1000)
          SEQ = sort(unique(c(seq(1, length(SCORE), floor(length(SCORE)/length(vec_letters))), max(SCORE))), decreasing = TRUE)
          GP = as.character(env[,"groups"])
          gp_nb = rep(0, length(GP))
          
          for(l in 1:length(vec_letters)) {
            b = grep(vec_letters[l], GP)
            GP[b] = "" 
            gp_nb[b] = SEQ[l]
          }
          env$score = gp_nb  
          
          alpha.info = paste(env$alpha, "|", env$alpha.correction)
          env$alpha.info_year = paste(env$year, alpha.info, sep = " - ")
          
          p = ggplot(env, aes(y = entry, x = alpha.info_year)) + geom_tile(aes(fill = score))
          p = p + scale_fill_gradient(low = "blue",high = "red")
          p = p + xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(env[1, "location"])
          return(p)
        })
      })
      names(out) = names(d_loc_out)
    } else { out = NULL }
    
  }
  
  
  # 3.4. run function for interaction ----------
  if(ggplot.type == "interaction") {

    fun_interaction = function(data, nb_parameters_per_plot){
      
      d_loc_out = get.loc.year(data, nb_parameters_per_plot)
      
      if( !is.null(d_loc_out) ) {
        out = lapply(d_loc_out, function(x){
          lapply(x, function(x_loc) {
            # Same scale for all the plots
            ymin = min(x_loc$median, na.rm = TRUE)
            ymax = max(x_loc$median, na.rm = TRUE)  
            
            if( attributes(data)$PPBstats.object == "data_mean_comparisons" ){
              alpha.info = paste(x_loc$alpha, "|", x_loc$alpha.correction)
              x_loc$alpha.info_year = as.factor(paste(x_loc$year, alpha.info, sep = " - "))      
            }
            
            if( attributes(data)$PPBstats.object == "data_env_with_no_controls" | 
                attributes(data)$PPBstats.object == "data_env_whose_param_did_not_converge"
            ) {
              x_loc$alpha.info_year = x_loc$year
            }
            
            p = ggplot(x_loc, aes(y = median, x = alpha.info_year, colour = entry, group = entry))
            p = p + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line") + ggtitle(x_loc[1, "location"])
            p = p + xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank())
            
            x_loc_year = plyr:::splitter_d(x_loc, .(year))    
            
            # Put lines for significant groups
            if( attributes(data)$PPBstats.object == "data_mean_comparisons" ){
              SEG = NULL
              
              letters = c(letters, paste(rep(LETTERS, times = 15), rep(c(1:5), each = 15), sep = ""))
              gp_letters = letters[1:max(x_loc$nb_group)]
              xadjust = seq(0.05, 0.9, length = 25) # it is assumed max 25 letters (i.e. groups)
              xadjust = xadjust[c(1:length(gp_letters))]
              names(xadjust) = gp_letters
              
              for(i in 1:length(x_loc_year)) {
                subx = droplevels(x_loc_year[[i]])
                
                # get rid of non sens information
                # This is useful if a germplasm is in group 'a' and 'b' alone. This is possible because other germplasm in groups 'a' and 'b' are in another year
                subx = arrange(subx, median) # To get the letter in the right order
                groups = as.character(subx$groups)
                
                a = lapply(groups, function(x) { unlist(strsplit(x, "")) } )
                row = unique(unlist(a))
                m = matrix(0, ncol = length(groups), nrow = length(row))
                rownames(m) = row
                for(jj in 1:length(a)) { m[a[[jj]], jj] = 1 }
                m = unique(m)
                if( is.vector(m) ) { m = as.data.frame(matrix(m, nrow = 1)) }
                
                if( nrow(m) > 1) {
                  todelete = NULL
                  for(jj in 1:ncol(m)) {
                    toget = which(m[,jj] == 1)
                    if( length(toget) >  1 ) {    
                      for(ii in toget) {
                        if( sum(m[ii,]) == 1 ) { todelete = c(todelete, ii) }
                      } 
                    } 
                  }
                  
                  if (!is.null(todelete)) { m = m[-todelete,] }
                  if( is.vector(m) ) { m = as.data.frame(matrix(m, nrow = 1)) }
                }
                
                
                # Following code to discard redondant informations
                # For example
                # 1 1 0 0
                # 0 1 1 1
                # 0 1 1 0
                # will give
                # 1 1 0 0
                # 0 1 1 1
                # indeed, the last row brings no informations
                
                if( nrow(m) > 1) {
                  todelete = NULL
                  for(ii in 1:(nrow(m)-1) ) { 
                    w1 = which( m[ii,] == 1 )
                    w2 = which( m[ii+1,] == 1 )
                    t = which(is.element(w1, w1[is.element(w1, w2)]))
                    test = length(w2) == length(t)
                    
                    if( test ) { todelete = c(todelete, ii+1)}
                  }
                  if( !is.null(todelete) ) { m = m[-todelete,] }
                  if( is.vector(m) ) { m = as.data.frame(matrix(m, nrow = 1)) }
                }
                
                
                # Initialize the letters
                if( nrow(m) > 1) {
                  for(ii in 1:nrow(m)) { m[ii, which(m[ii,] == 1)] = letters[ii]; m[ii, which(m[ii,] == 0)] = ""  }
                } else { m[1, which(m[1,] == 1)] = letters[1]  }
                
                groups = apply(m, 2, function(x){paste(x, collapse="")})
                
                subx$groups = factor(groups)
                
                for(l in gp_letters) {
                  togrep = grep(l, subx[,"groups"])
                  
                  if(length(togrep) > 0) {
                    # check if it is continuous
                    vec.togrep = NULL
                    
                    a = togrep
                    if( length(a) > 1 ){ 
                      test = a[1:(length(a)-1)] + 1 == a[2:length(a)]
                      t = c(FALSE, test)
                      t[which(t)] = 1; t[which(!t)] = 0
                      b = paste("0",unlist(strsplit(paste(as.character(t[2:length(t)]),collapse=""),"0")),sep="")
                      if(t[length(t)] == 0) { b = c(b, 0) }
                      c = c(0, cumsum(sapply(b, function(x){nchar(x)})))
                      for(k in 1:(length(c)-1)) { vec.togrep = c(vec.togrep, (a[(c[k]+1):c[k+1]])) }
                    } else { vec.togrep = c(vec.togrep, togrep) }
                    
                    toto = subx[vec.togrep, "median"]
                    
                    y.seg = range(toto)
                    x.seg = i + xadjust[l]
                    alpha.info = paste(as.character(subx[1, "alpha"]),"|", as.character(subx[1, "alpha.correction"]))
                    seg = cbind.data.frame(ymin = y.seg[1], ymax = y.seg[2], x = x.seg, groups = paste("group", l), alpha.info, year = names(x_loc_year)[i])
                    SEG = rbind.data.frame(SEG, seg)
                  }
                }
              }
              
              # For germplasm alone in one group, change ymax in order to see it
              for(i in 1:nrow(SEG)){
                if( SEG$ymin[i] == SEG$ymax[i] ) { SEG$ymax[i] = SEG$ymax[i] + min(SEG$ymax)/30 }
              }
              
              
              if(!is.null(SEG)) {
                p = p + geom_segment(aes(x = x, y = ymin, xend = x, yend = ymax, group = NULL), colour =  as.numeric(as.factor(SEG$groups)), data = SEG)
                p = p + ylim(ymin - ymin/10, ymax + ymax/10) # To be sure to see the line of the groups
              }      
            }
            
            return(p)        
          })
        } )
        names(out) = names(d_loc_out)
      } else { out = NULL }
      
      return(out)
    }
    
    out = lapply(all_data, fun_interaction, nb_parameters_per_plot)
    
  }
  
  # return results
  return(out)
}




