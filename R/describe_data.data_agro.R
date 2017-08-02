describe_data.data_agro = function(
  x,
  vec_variables,
  nb_parameters_per_plot = 5
){
  data = x
  
  # 1. Error message ----------  
  check_data_vec_variables(data, vec_variables)
  
  # 2. Description all variables ----------
  summary_all = summary(data)
  
  # 3. Description for each variable ----------
  fun = function(variable, data){
    
    colnames(data)[which(colnames(data) == variable)] = "variable"
    
    dtmp = droplevels(na.omit(data[,c("germplasm", "location", "year", "variable")]))
    dtmp$variable = as.numeric(dtmp$variable)
    xlim = c(min(dtmp$variable, na.rm = TRUE), max(dtmp$variable, na.rm = TRUE))
    ylim = c(0,max(dtmp$variable, na.rm = TRUE))
    
    # 2.1. Presence/absence for each germplasm, location and year
    m = as.data.frame(with(dtmp, table(germplasm, location, year)))
    m$Freq = as.factor(m$Freq)
    colnames(m)[4] = "nb_measures"
    
    p = ggplot(m, aes(x = germplasm, y = location))
    p = p + geom_raster(aes(fill = nb_measures)) + facet_grid(year ~ .)
    nb_NA = round(length(which(m$nb_measures == 0)) / ( length(which(m$nb_measures == 0)) + length(which(m$nb_measures != 0)) ), 2) * 100
    p = p + ggtitle(
      paste("Presence absence repartition for ", variable, sep = ""),
      paste("(",  nb_NA, "% of 0)", sep = "")
    ) + theme(axis.text.x=element_text(angle=90))
    out.presence.absence = p
    
    # 2.2. Histogram and boxplot
    out_all_hist = ggplot(dtmp, aes(variable)) + geom_histogram() + ggtitle(variable)
    
    # per germplasm
    dtmp_g =  split_data_for_ggplot(dtmp, "germplasm", nb_parameters_per_plot)
    
    fun_g = function(x, xlim, variable_name){
      ggplot(x, aes(variable)) + geom_histogram() + facet_grid(germplasm ~ .) + ggtitle(variable_name)  + coord_cartesian(xlim = xlim)
    }
    out_g_hist = lapply(dtmp_g, function(x){fun_g(x, xlim, variable)})
    
    fun_g = function(x, ylim){
      plot = ggplot(x, aes(x = germplasm, y = variable)) + geom_boxplot() + ggtitle(variable)  + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + coord_cartesian(ylim = ylim)
      outliers = boxplot(x$variable, x$germplasm, plot = FALSE)$out
      names(outliers) = x$germplasm[which(x$variable %in% outliers)]
      return(list("plot" = plot, "outliers" = outliers))
    }
    out_g_box = lapply(dtmp_g, fun_g, xlim)
    
    # per location
    dtmp_l = split_data_for_ggplot(dtmp, "location", nb_parameters_per_plot)
    
    fun_l = function(x, xlim){
      ggplot(x, aes(variable)) + geom_histogram() + facet_grid(location ~ .) + ggtitle(variable)  + coord_cartesian(xlim = xlim)
    }
    out_l_hist = lapply(dtmp_l, fun_l, xlim)
    
    fun_l = function(x, ylim){
      plot = ggplot(x, aes(x = location, y = variable)) + geom_boxplot() + ggtitle(variable)  + coord_cartesian(ylim = ylim)
      outliers = boxplot(x$variable, x$location, plot = FALSE)$out
      names(outliers) = x$location[which(x$variable %in% outliers)]
      return(list("plot" = plot, "outliers" = outliers))
    }
    
    out_l_box = lapply(dtmp_l, fun_l, xlim)
    
    # per year
    dtmp_y = split_data_for_ggplot(dtmp, "year", nb_parameters_per_plot)
    
    fun_y = function(x, xlim){
      ggplot(x, aes(variable)) + geom_histogram() + facet_grid(year ~ .) + ggtitle(variable)  + coord_cartesian(xlim = xlim)
    }
    out_y_hist = lapply(dtmp_y, fun_y, xlim)
    
    fun_y = function(x, ylim){
      plot = ggplot(x, aes(x = year, y = variable)) + geom_boxplot() + ggtitle(variable) + coord_cartesian(ylim = ylim)
      outliers = boxplot(x$variable, x$year, plot = FALSE)$out
      names(outliers) = x$year[which(x$variable %in% outliers)]
      return(list("plot" = plot, "outliers" = outliers))
    }
    out_y_box = lapply(dtmp_y, fun_y, xlim)
    
    # interaction ----------
    dtmp$int = paste(dtmp$germplasm,dtmp$location,dtmp$year,sep=":")
    A = unlist(lapply(unique(dtmp$int), function(x){mean(na.omit(dtmp[dtmp$int %in% x,"variable"]))}))
    ylim = c(min(A),max(A))
    dtmp_int = split_data_for_ggplot(dtmp, "germplasm", nb_parameters_per_plot)
    
    fun_int = function(x, xlim){
      p_gxe = ggplot(data = x, aes(x = location, y = variable, colour = germplasm, group = germplasm))
      #  p_gxe = p_gxe + stat_summary(fun.y= mean, geom = "point")
      p_gxe = p_gxe + stat_summary(fun.y = mean, geom = "line", aes(linetype = germplasm), size = 0.65) # + scale_linetype_manual(values=rep(c("solid", "dotted"), 6))
      
      #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      #cbbPalette <- c("#000000", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
      
      #p_gxe = p_gxe + scale_color_manual(values=rep(cbbPalette, each = 2))
      
      p_gxe = p_gxe + theme(axis.text.x=element_text(angle=90))
      p_gxe = p_gxe + ylab(variable)
      # p2_GxE + ggtitle("") + xlab("") + ylab("") + theme(legend.title=element_blank())
      
      out_gxe = p_gxe + facet_grid(year ~ .) + coord_cartesian(ylim = xlim)
      return(out_gxe)
    }
    out_y_int = lapply(dtmp_int, fun_int, ylim)
    
    
    OUT = list("presence.absence" = out.presence.absence, 
               "histogram" = list(
                 "all" = out_all_hist, 
                 "germplasm" = out_g_hist, 
                 "location" = out_l_hist, 
                 "year" = out_y_hist
               ),
               "boxplot" = list(
                 "germplasm" = out_g_box, 
                 "location" = out_l_box, 
                 "year" = out_y_box
               ),
               "interaction" = out_y_int
    )
  }
  
  out_each_variable = lapply(vec_variables, fun, data)
  names(out_each_variable) = vec_variables
  
  OUT = list("all_variables" = summary_all,
             "each_variable" = out_each_variable
  )
  
  return(OUT)
}
