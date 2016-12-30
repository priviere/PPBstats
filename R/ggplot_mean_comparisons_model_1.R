ggplot_mean_comparisons_model_1 = function(
  mean_comparisons_model_1,
  data_version = NULL,
  ggplot.type = "interaction",
  nb_parameters_per_plot = 10
  ){
  
  # 1. Error message
  if( attributes(mean_comparisons_model_1)$PPBstats.object != "mean_comparisons_model_1" ) { stop("data must come from mean_comparisons and model_1") }
  
  data_mean_comparisons = mean_comparisons_model_1$data_mean_comparisons
  data_env_with_no_controls = mean_comparisons_model_1$data_env_with_no_controls
  data_env_whose_param_did_not_converge = mean_comparisons_model_1$data_env_whose_param_did_not_converge
  
  
  env = "env1-1:2010"
  
  data = data_mean_comparisons[[env]]
  para = unlist(strsplit(as.character(data$parameter)[1], "\\["))[1]
  
  data$entry = sub(paste(para, "\\[", sep=""), "", sapply(data$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
  data$environment =  sub("\\]", "", sapply(data$parameter, function(x){unlist(strsplit(as.character(x), ","))[2]}))
  data$location = sapply(data$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
  data$year = sapply(data$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
  
  
  
  data_Mpvalue = data$Mpvalue
  d_env = data = data$mean.comparisons
  
  test.mu.m1 = length(grep("mu\\[", data$parameter)) > 0
  test.beta.m1 = length(grep("beta\\[", data$parameter)) > 0
  
  
  get.loc.year = function(data, nb_parameters_per_plot){
    
    d_loc = plyr:::splitter_d(data, .(location))
    
    d_loc_b = lapply(d_loc, function(x){
      x = arrange(x, entry)
      x$split = as.numeric(factor(x$entry))
      seq_nb_para = unique(c(seq(1, max(x$split), nb_parameters_per_plot), max(x$split)*2))
      for(i in 1:(length(seq_nb_para) - 1) ) { x$split[seq_nb_para[i] <= x$split & x$split < seq_nb_para[i+1]] = i }
      x_split = plyr:::splitter_d(x, .(split))
      return(x_split)
    } )
    
    return(d_loc_b)
  }
  
  if( ggplot.type == "barplot") {
  
    if(!is.null(data_version)) {
      data_version$environment = paste(data_version$location, ":", data_version$year, sep = "")
      data_version$mu = paste("mu[", data_version$germplasm, ",", data_version$environment, "]", sep = "")
      vec_env = unique(data_version$environment)
      vec_env_to_get = vec_env[is.element(vec_env, names(d_env))]
      vec_env_not_to_get = vec_env[!is.element(vec_env, names(d_env))]
      if( length(vec_env_not_to_get) > 0 ){ warning("The following environments in data_version are not taken: ", paste(vec_env_not_to_get, collapse = ", "),".") }
      
      if( length(vec_env_to_get) == 0 ) { stop("There are no environment to display.") }
      
      d_env = d_env[vec_env_to_get]
      
      fun = function(x, data_version){
        test = grep("mu", x$parameter)
        if( length(test) == 0 ) { x$parameter = paste("mu", x$parameter, sep = "") } # i.e. "data_env_with_no_controls.model1" | "model1.data_env_whose_param_did_not_converge")
        p_to_get = filter(data_version, environment == x$environment[1])$mu
        x = filter(x, parameter %in% p_to_get)
        x$max = max(x$median, na.rm = TRUE)
        x = arrange(x, parameter)
        x$split = add_split_col(x, nb_parameters_per_plot)
        x_split = plyr:::splitter_d(x, .(split))
        return(x_split)
      }
      
      d_env_b = lapply(d_env, fun, data_version)
      
      OUT = lapply(d_env_b, function(x){
        out = lapply(x, function(dx){
          if(attributes(data)$PPBstats.object == "data_mean_comparisons") { # Add letters of significant groups
            
            env = dx$environment[1]
            data_Mpvalue_env = data_Mpvalue[[env]]
            data_version_tmp = droplevels(filter(data_version, environment == env))
            gp = unique(data_version_tmp$group)
            STARS = NULL
            for(g in gp){
              dtmp = droplevels(filter(data_version_tmp, group == g))
              vec_version = levels(dtmp$version)
              v1 = unique(as.character(filter(dtmp, version == vec_version[1])$mu))
              v2 = unique(as.character(filter(dtmp, version == vec_version[2])$mu))
              for (i in 1:ncol(data_Mpvalue_env)) { 
                if (colnames(data_Mpvalue_env)[i] == v1) {c1 = i}
                if (colnames(data_Mpvalue_env)[i] == v2) {c2 = i}
              }
              pvalue = data_Mpvalue_env[min(c1,c2), max(c1,c2)]
              
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
            p = p + xlab("") + theme(axis.text.x = element_text(angle = 90))
          }
          
          
          if(attributes(data)$PPBstats.object == "data_env_with_no_controls" |
             attributes(data)$PPBstats.object == "data_env_whose_param_did_not_converge") {
            
            env = dx$environment[1]
            data_version_tmp = droplevels(filter(data_version, environment == env))
            
            gp = unique(data_version_tmp$group)
            STARS = NULL
            for(g in gp){
              dtmp = droplevels(filter(data_version_tmp, group == g))
              vec_version = levels(dtmp$version)
              v1 = unique(as.character(filter(dtmp, version == vec_version[1])$median))
              v2 = unique(as.character(filter(dtmp, version == vec_version[2])$median))
              
              if( length(v1) > 1 & length(v2) > 1) {
                pvalue = t.test(v1, v2)$p.value
              } else { pvalue = NULL; warning("No t.test are done as there are not enough observations.") }
              
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
            d = join(data_version_tmp, dx, "mu")
            
            p = ggplot(d, aes(x = group, y = median)) + geom_bar(aes(fill = version), stat = "identity", position = "dodge")
            
            y = tapply(d$median, d$group, mean, na.rm = TRUE)
            y = y + (max(y) * 0.2)
            label_stars = data.frame(group = names(STARS), median = y[names(STARS)], STARS = STARS)
            p = p + geom_text(data = label_stars, aes(label = STARS))
            p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, dx[1,"max"])
          }
          
          return(p)
        })
        return(out)
      })
      names(OUT) = names(d_env_b)
      
      
    } else {
      
      d_env_b = lapply(d_env, function(x){
        x = arrange(x, median)
        x$max = max(x$median, na.rm = TRUE)
        x$split = add_split_col(x, nb_parameters_per_plot)
        x_split = plyr:::splitter_d(x, .(split))
        return(x_split)
      } )
      
      OUT = lapply(d_env_b, function(x){
        out = lapply(x, function(dx){
          p = ggplot(dx, aes(x = reorder(parameter, median), y = median)) + geom_bar(stat = "identity")
          
          if(attributes(data)$PPBstats.object == "data_mean_comparisons") { # Add letters of significant groups
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
    
  }
  
  }
  
  
  if(ggplot.type == "score") {  
    
    d_loc_out = get.loc.year(data, nb_parameters_per_plot)
    OUT = lapply(d_loc_out, function(x){
      out = lapply(x, function(env){
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
      return(out)
    })
    
    names(OUT) = names(d_loc_out)
  }
  
  

  # return results
  out = list(
    
  )
  
  return(out)
}

