ggplot_mean_comparisons_model_2 = function(
  mean_comparisons_model_2,
  nb_parameters_per_plot = 10
){
  
  # 1. Error message
  if( attributes(mean_comparisons_model_1)$PPBstats.object != "mean_comparisons_model_2" ) { stop("data must come from mean_comparisons and model_2") }
  
  if( is.element(ggplot.type, c("score", "interaction")) ) { stop("ggplot.type must be barplot with output from model_2") }
  
  
  data_Mpvalue = mean_comparisons_model_2$Mpvalue
  data = mean_comparisons_model_2$mean.comparisons
  attributes(data)$PPBstats.object = "mean.comparisons.model2"
  
  test.alpha.m2 = length(grep("alpha\\[", data$parameter)) > 0
  test.beta.m2 = length(grep("beta\\[", data$parameter)) > 0
  test.theta.m2 = length(grep("theta\\[", data$parameter)) > 0  
  
  
  if(ggplot.type == "barplot") {  
    data = arrange(data, median)  
    data$max = max(data$median, na.rm = TRUE)
    data$split = add_split_col(data, nb_parameters_per_plot)
    data_split = plyr:::splitter_d(data, .(split))  
    
    para.name = unlist(strsplit(as.character(data[1, "parameter"]), "\\["))[1]
    
    OUT = lapply(data_split, function(dx){
      p = ggplot(dx, aes(x = reorder(parameter, median), y = median)) + geom_bar(stat = "identity")
      p = p + geom_text(data = dx, aes(x = reorder(parameter, median), y = median/2, label = groups), angle = 90, color = "white")
      p = p + ggtitle(paste(para.name, "\n alpha = ", dx[1, "alpha"], "; alpha correction :", dx[1, "alpha.correction"]))
      p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, data[1,"max"]) + ylab("")
      return(p)
    })
    
    OUT = list(OUT)
    names(OUT) = para.name
  }
  
  
  # return results
  out = list(
    
  )
  
  return(out)
}

