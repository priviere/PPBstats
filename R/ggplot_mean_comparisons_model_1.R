ggplot_mean_comparisons_model_1 = function(
  mean_comparisons_model_1,
  nb_parameters_per_plot = 10
  ){
  
  # 1. Error message
  if( attributes(mean_comparisons_model_1)$PPBstats.object != "mean_comparisons_model_1" ) { stop("data must come from mean_comparisons and model_1") }
  
  data_Mpvalue = mean_comparisons_model_1$Mpvalue
  data = mean_comparisons_model_1$mean.comparisons
  
  test.mu.m1 = length(grep("mu\\[", data$parameter)) > 0
  test.beta.m1 = length(grep("beta\\[", data$parameter)) > 0  
  
  
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

