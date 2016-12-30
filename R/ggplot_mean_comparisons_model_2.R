ggplot_mean_comparisons_model_2 = function(
  out_mean_comparisons_model_2,
  ggplot.type,
  nb_parameters_per_plot = 10
){
  
  # 1. Error message
  if( attributes(out_mean_comparisons_model_2)$PPBstats.object != "mean_comparisons_model_2" ) { stop("data must come from mean_comparisons and model_2") }
  
  if( !is.element(ggplot.type, c("biplot-alpha-beta", "barplot")) ) { stop("ggplot.type must be barplot or biplot-alpha-beta with output from model_2") }
  
  data_Mpvalue = out_mean_comparisons_model_2$Mpvalue
  data = out_mean_comparisons_model_2$mean.comparisons
  
  if(ggplot.type == "barplot") {  
    data = arrange(data, median)  
    data$max = max(data$median, na.rm = TRUE)
    data$split = add_split_col(data, nb_parameters_per_plot)
    data_split = plyr:::splitter_d(data, .(split))  
    
    para.name = unlist(strsplit(as.character(data[1, "parameter"]), "\\["))[1]
    
    out = lapply(data_split, function(dx){
      p = ggplot(dx, aes(x = reorder(parameter, median), y = median)) + geom_bar(stat = "identity")
      p = p + geom_text(data = dx, aes(x = reorder(parameter, median), y = median/2, label = groups), angle = 90, color = "white")
      p = p + ggtitle(paste(para.name, "\n alpha = ", dx[1, "alpha"], "; alpha correction :", dx[1, "alpha.correction"]))
      p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, data[1,"max"]) + ylab("")
      return(p)
    })
    out = list(out)
    names(out) = para.name
  }
  
  
  # 2.5. biplot-alpha-beta ----------
  if(ggplot.type == "biplot-alpha-beta"){
    
    a = data
    test_a = unlist(strsplit(as.character(a[1,"parameter"]), "\\["))[1]
    if( test_a != "alpha" ){ stop("With ggplot.type = \"biplot-alpha-beta\", data must come from get.mean.comparisons with paramater = \"alpha\".") }
    a$germplasm = gsub("alpha", "", a$parameter)
    colnames(a)[which(colnames(a) == "parameter")] = "parameter_a"
    colnames(a)[which(colnames(a) == "median")] = "alpha_i"
    
    b = data_2$mean.comparisons
    test_b = unlist(strsplit(as.character(b[1,"parameter"]), "\\["))[1]
    if( test_b != "beta" ){ stop("With ggplot.type = \"biplot-alpha-beta\", data_2 must come from get.mean.comparisons with paramater = \"beta\".") }
    b$germplasm = gsub("beta", "", b$parameter)
    colnames(b)[which(colnames(b) == "parameter")] = "parameter_b"
    colnames(b)[which(colnames(b) == "median")] = "beta_i"
    
    
    ab = join(a, b, "germplasm")
    ab=ab[which(!is.na(ab$sensibilite) & !is.na(ab$alpha_i)),]
    ab$germplasm = gsub("\\[", "", ab$germplasm)
    ab$germplasm = gsub("\\]", "", ab$germplasm)
    
    if(is.null(nb_parameters_per_plot)){nb_parameters_per_plot = nrow(ab)}
    if(nb_parameters_per_plot > nrow(ab)){nb_parameters_per_plot = nrow(ab)}
    if(nb_parameters_per_plot < nrow(ab)){
      ab$split = rep(c(1:ceiling(nrow(ab)/nb_parameters_per_plot)), floor(nrow(ab)/(ceiling(nrow(ab)/nb_parameters_per_plot))))[1:nrow(ab)]
    }else{
      ab$split = rep(1,nrow(ab))
    }
    
    xlim = c(floor(min(ab$effet_genetique)),ceiling(max(ab$alpha_i)))
    ylim = c(min(ab$sensibilite),max(ab$beta_i))
    
    d_ab = plyr:::splitter_d(ab, .(split))
    
    out = lapply(d_ab,function(y){
      p = ggplot(y, aes(x = effet_genetique, y = sensibilite, label = germplasm)) + coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)
      p = p + geom_text() + geom_hline(yintercept = 0)
    })
  }
  
  # return results
  return(out)
}

