ggplot_mean_comparisons_GxE = function(
  data,
  nb_parameters_per_plot = 10
  ){
  
  # 1. Error message
  if( attributes(data)$PPBstats.object != "mean_comparisons_GxE" ) { stop("data must come from mean_comparisons and GxE") }
  
  variable = data$variable
  
  data_ggplot_LSDbarplot_germplasm = data$data_ggplot_LSDbarplot_germplasm
  data_ggplot_LSDbarplot_location = data$data_ggplot_LSDbarplot_location
  data_ggplot_LSDbarplot_year = data$data_ggplot_LSDbarplot_year
  
  ggplot_LSDbarplot = function(d_LSD, fac, variable){
    p = ggplot(data = d_LSD, aes(x = trt, y = means, label = M)) + geom_bar(stat = "identity")
    p = p + geom_text(aes(y = ymean), angle = 90, color = "white")
    p = p + theme(legend.position = "none", axis.text.x = element_text(angle = 90))
    p = p + xlab(fac) + ylab(variable)
    return(p)
  }
  
  # 2. Germplasm
  ggplot_LSDbarplot_germplasm = ggplot_LSDbarplot(data_ggplot_LSDbarplot_germplasm, "germplasm", variable)
  
  # 3. Location
  ggplot_LSDbarplot_location = ggplot_LSDbarplot(data_ggplot_LSDbarplot_location, "location", variable)
  
  # 4. Year
  ggplot_LSDbarplot_year = ggplot_LSDbarplot(data_ggplot_LSDbarplot_year, "year", variable)
  
  # 5. return results
  out = list(
    "germplasm" = ggplot_LSDbarplot_germplasm,
    "location" = ggplot_LSDbarplot_location,
    "year" = ggplot_LSDbarplot_year
  )
  
  return(out)
}

