#' Plot agro object from format_data_PPBstats()
#'
#' @description
#' \code{plot.data_agro} gets ggplot to describe the data
#' 
#' @param data The data frame. It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "..."), with "..." the variables.
#' 
#' @param plot_type the type of plot you wish. It can be :
#' \itemize{
#'  \item "pam" for presence abscence matrix that represent the combinaison of germplasm x location
#'  \item "histogramm"
#'  \item "barplot", where sd error are displayed
#'  \item "boxplot"
#'  \item "interaction"
#'  \item "biplot"
#'  \item "radar"
#'  \item "raster"
#' }
#' 
#' @param x_axis factor displayed on the x.axis of a plot
#' 
#' @param in_col factor displayed in color of a plot
#' 
#' @param vec_variables vector of variables to describe
#' 
#' @param nb_parameters_per_plot_x_axis the number of parameters per plot on x_axis arguments
#' 
#' @param nb_parameters_per_plot_in_col the number of parameters per plot for in_col arguments
#' 
#' @param labels_on factor to display for plot_type = "biplot"
#' 
#' @param labels_size size of the label for plot_type = "biplot" and "radar"
#' 
#' @return 
#' \itemize{
#'  \item For plot_type "histogramm", "barplot", "boxplot" or "interaction",
#'  the function returns a list with ggplot objects for each variable of vec_variables.
#'  \item For plot_type "biplot",
#'  the function returns a list with ggplot objects for each pairs of variables of vec_variables. 
#'  \item For plot_type "radar",
#'  the function returns a list with ggplot objects with all variables of vec_variables. 
#'  \item For plot_type "raster"
#'  the function returns a list with ggplot objects with all variables of vec_variables. 
#' }
#' Each list is divided in several lists according to values 
#' of nb_parameters_per_plot_x_axis and nb_parameters_per_plot_in_col.
#' 
#' @author Pierre Riviere
#' 
plot.data_agro = function(
  data,
  plot_type = c("pam", "histogramm", "barplot", "boxplot", "interaction", "biplot", "radar", "raster"),
  x_axis = NULL,
  in_col = NULL,
  vec_variables = NULL,
  nb_parameters_per_plot_x_axis = 5,
  nb_parameters_per_plot_in_col = 5,
  labels_on = NULL,
  labels_size = 4
){
  # 1. Error message ----------  
  mess = "plot_type must be \"pam\", \"histogramm\", \"barplot\", \"boxplot\", \"interaction\", \"biplot\", \"radar\" or \"raster\"."
  if(length(plot_type) != 1) { stop(mess) }
  if(!is.element(plot_type, c("pam", "histogramm", "barplot", "boxplot", "interaction", "biplot", "radar", "raster"))) { 
    stop(mess) 
  }
  
  if(is.null(vec_variables)){ stop("You must settle vec_variables") }
  
  check_arg = function(x, vec_x) { 
    for(i in x) { 
      if(!is.element(i, vec_x)) { 
        stop("Regarding ", substitute(x),", ", i," is not in data") 
      } 
    } 
  }
  
  if(!is.null(x_axis)){ check_arg(x_axis, colnames(data)) }
  if(!is.null(in_col)){ check_arg(in_col, colnames(data)) }
  check_arg(vec_variables, colnames(data))
  if(!is.null(labels_on)){ check_arg(labels_on, colnames(data)) }

  if( plot_type == "pam" & (!is.null(x_axis) | !is.null(in_col)) ){ 
    warning("Note than with plot_type == pam, x_axis and in_col are not used.")
  }
  if( plot_type == "histogramm" & !is.null(x_axis) ){ 
    warning("Note than with plot_type == histogramm, x_axis can not be NULL.")
  }
  if( plot_type == "barplot" & is.null(x_axis) ){ 
    stop("With plot_type == barplot, x_axis can not be NULL.")
  }
  if( plot_type == "boxplot" & is.null(x_axis) ){ 
    stop("With plot_type == boxplot, x_axis can not be NULL.")
  }
  if( plot_type == "interaction" & (is.null(x_axis) | is.null(in_col)) ){ 
    stop("With plot_type == interaction, x_axis and in_col can not be NULL.")
  }
  if( plot_type == "biplot" & !is.null(x_axis) ){ 
    warning("Note than with plot_type == biplot, x_axis is not used can not be NULL.")
  }
  if( plot_type == "biplot" & length(vec_variables) < 2 ){ 
    stop("With plot_type == biplot, vec_variables must have at least 2 elements.")
  }
  if( plot_type == "biplot" & is.null(labels_on) ){ 
    stop("With plot_type == biplot, labels_on can not be NULL.")
  }
  if( plot_type == "biplot" & length(labels_on) != 1 ){ 
    stop("labels_on must be of length one..")
  }
  if( plot_type == "radar" & length(vec_variables) < 2 ){ 
    stop("With plot_type == radar, vec_variables must have at least 2 elements.")
  }
  if( plot_type == "radar" & is.null(in_col) ){ 
    stop("With plot_type == radar, in_col must not be NULL.")
  }
  if( plot_type == "radar" & !is.null(labels_on) ){ 
    warning("Note that with plot_type == radar, labels_on is not used.")
  }

  if( plot_type == "raster" & !is.null(in_col) ){ 
    warning("Note that with plot_type == raster, in_col is not used.")
  }
  if( plot_type == "raster" & !is.null(labels_on) ){ 
    warning("Note that with plot_type == raster, labels_on is not used.")
  }
  
  # 2. Functions used in the newt steps ----------
  
  # 2.1. Function to run presence abscence matrix ----------
  fun_pam = function(data, vec_variables){
    
    fun_pam_1 = function(variable, data){
      dtmp = droplevels(na.omit(data[,c("germplasm", "location", "year", variable)]))
      dtmp[,variable] = as.numeric(dtmp[,variable])
      
      xlim = c(min(dtmp[,variable], na.rm = TRUE), max(dtmp[,variable], na.rm = TRUE))
      ylim = c(0,max(dtmp[,variable], na.rm = TRUE))
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
      return(p)
    }
    out = lapply(vec_variables, fun_pam_1, data)
    names(out) = vec_variables
    return(out)
  }
  
  
  # 2.2. Function to run histogramm, barplot, boxplot, interaction ----------
  fun_hbbi_1 = function(d, x_axis, in_col, plot_type, variable, ylim){
    
    d$variable = d[,variable]
    
    # histogramm
    if(plot_type == "histogramm") {
      p = ggplot(d, aes( x = variable))
      if( is.null(in_col) ) { 
        p = p + geom_histogram() 
      } else { 
        p = p + geom_histogram(aes(fill = in_col)) 
      }
    }
    
    # barplot
    if(plot_type == "barplot") {	
      if(is.null(in_col)) {	
        mm2 = ddply(d, "x_axis", summarise, mean = mean(variable, na.rm = TRUE), sd = sd(variable, na.rm = TRUE))
        p = ggplot(mm2, aes(x = x_axis, y = mean)) + geom_bar(stat = "identity") 
        limits <- aes(ymax = mean + sd, ymin = mean - sd)
        p = p + geom_errorbar(limits, position = position_dodge(width=0.9), width=0.25)
      } else {
        d$toto = paste(d$in_col, d$x_axis, sep = "azerty")
        mm = ddply(d, "toto", summarise, mean = mean(variable, na.rm = TRUE), sd = sd(variable, na.rm = TRUE)) 
        mm$in_col = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[1]}))
        mm$x_axis = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[2]}))
        
        p = ggplot(mm, aes(x = x_axis, y = mean, fill = in_col))
        p = p + geom_bar(position = "dodge", stat = "identity") 
        limits <- aes(ymax = mean + sd, ymin = mean - sd)
        p = p + geom_errorbar(limits, position = position_dodge(width=0.9), width=0.25)
      }
    }
    
    
    # boxplot
    if(plot_type == "boxplot") {
      p = ggplot(d, aes( x = x_axis, y = variable))
      if( is.null(in_col) ) { 
        p = p + geom_boxplot(position="dodge") 
      } else { 
        p = p + geom_boxplot(aes(fill = in_col)) 
      }
    }
    
    # interaction
    if(plot_type == "interaction") {										
      p = ggplot(d, aes(y = variable, x = factor(x_axis), colour = factor(in_col), group = factor(in_col)))
      p = p + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line")
    }
    
    if(is.element(plot_type, c("barplot", "boxplot", "interaction"))) {
      p = p + xlab("") + ylab(variable) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank())	
      p = p + coord_cartesian(xlim = NULL, ylim)
    }
    
    return(p)
  }
  
  fun_hbbi = function(d, vec_variables,
                      x_axis, nb_parameters_per_plot_x_axis, 
                      in_col, nb_parameters_per_plot_in_col, 
                      plot_type){ 
    
    out = lapply(vec_variables, 
                 function(variable, d, labels_on,
                          x_axis, nb_parameters_per_plot_x_axis,
                          in_col, nb_parameters_per_plot_in_col,
                          plot_type){
                   
                   d = reshape_data_split_x_axis_in_col(d, variable, labels_on,
                                    x_axis, nb_parameters_per_plot_x_axis,
                                    in_col, nb_parameters_per_plot_in_col
                   )
                   ylim = range(unlist(lapply(d, function(x){ range(x[,variable], na.omit = TRUE) } )))
                   out = lapply(d, fun_hbbi_1, x_axis, in_col, plot_type, variable, ylim)
                   return(out)
                 },
                 d, labels_on,
                 x_axis, nb_parameters_per_plot_x_axis,
                 in_col, nb_parameters_per_plot_in_col,
                 plot_type
    )
    names(out) = vec_variables
    return(out)
  }
  
  
  # 2.3. Function to run biplot ----------
  fun_biplot = function(d, vec_variables, labels_on, labels_size,
                        x_axis, nb_parameters_per_plot_x_axis, 
                        in_col, nb_parameters_per_plot_in_col
  ){
    
    d = reshape_data_split_x_axis_in_col(d, vec_variables, labels_on,
                     x_axis, nb_parameters_per_plot_x_axis, 
                     in_col, nb_parameters_per_plot_in_col
    )
    
    ylim = NULL
    for(variable in vec_variables){
      ylim = c(ylim, list(
        range(unlist(lapply(d, function(x){ range(x[,variable], na.omit = TRUE) } )))
      )
      )
    }
    names(ylim) = vec_variables
    
    fun_biplot_1 = function(pair_var, d, in_col, labels_size, ylim){
      fun_biplot_2 = function(d, pair_var, in_col, labels_size, ylim){
        var_ = unlist(strsplit(pair_var, " -azerty- "))
        var1 = var_[1]; d$var1 = d[,var1]
        var2 = var_[2]; d$var2 = d[,var2]
        ylim = range(unlist(ylim[c(var_[1], var_[2])]))
        if(!is.null(in_col)){ 
          dtmp = d[,c("in_col", "var1", "var2", "labels_text")] 
        } else {
          dtmp = d[,c("var1", "var2", "labels_text")] 
        }
        dtmp = na.omit(dtmp)
        if( nrow(dtmp) == 0){
          warning("No biplot is done for ", var1, " and ", var2, " as there are only NA. This can be due to missing data."); 
          p = NULL
        } else {
          p = ggplot(dtmp, aes(x = var1, y = var2, label = labels_text)) 
          if(!is.null(in_col)){
            p = p + geom_text(aes(colour = in_col), size = labels_size)             
          } else {
            p = p + geom_text(size = labels_size) 
          }
          p = p + coord_cartesian(xlim = NULL, ylim = ylim)
          p = p + stat_smooth(method = "lm", se = FALSE)
          p = p  + xlab(var1) + ylab(var2) + theme(axis.text.x = element_text(angle=90, hjust=1), legend.title = element_blank()) 
        }
        return(p)
      }
      p = lapply(d, fun_biplot_2, pair_var, in_col, labels_size, ylim)
      return(p)
    }
    
    pair_var = apply(combn(vec_variables, 2), 2, function(x){paste(x, collapse = " -azerty- ")})
    out = lapply(pair_var, fun_biplot_1, d, in_col, labels_size, ylim)
    names(out) = sub(" -azerty- ", " - ", pair_var)
    return(out)
  }
  
  # 2.4. Function to run radar ----------
  fun_radar = function(d, vec_variables, in_col, labels_size){
    d$group = d[,in_col]
    
    m = data.frame(matrix(levels(d$group), ncol = 1))
    for(variable in vec_variables){
      value = tapply(d[,variable], d$group, mean, na.rm = TRUE)
      # rescale all variables to lie between 0 and 1
      value_ok = value / sum(value, na.rm = TRUE)
      m = cbind.data.frame(m, value_ok)
    }
    colnames(m) = c("group", vec_variables)
    p = ggradar(m, 
                grid.label.size = labels_size, 
                axis.label.size = labels_size,
                group.point.size = labels_size,
                legend.text.size = labels_size*2.5,
                group.line.width= labels_size/4)
    p = p + theme(legend.title = element_blank()) 
    return(p)
  }
  
  # 2.5. Function to run raster representation for factor variables ----------
  fun_raster_1 = function(data, vec_variable){
    vv = vm = vx = NULL
    for(v in vec_variables) { 
      vv = c(vv, as.character(rep(v, nrow(data))))
      vm = c(vm, as.character(data[,v]))
      vx = c(vx, as.character(data$x_axis))
    }
    
    dtmp = cbind.data.frame(
      variable = as.factor(vv),
      value = as.factor(vm),
      x_axis = as.factor(vx)
      )
    
    p = ggplot(dtmp, aes(x = x_axis, y = variable))
    p = p + geom_raster(aes(fill = value))
    p = p + theme(axis.text.x=element_text(angle=90))
    return(p)
  }
  
  fun_raster = function(d, vec_variables,
                        x_axis, nb_parameters_per_plot_x_axis){ 
    d = reshape_data_split_x_axis_in_col(d, vec_variables, labels_on = NULL,
                     x_axis, nb_parameters_per_plot_x_axis,
                     in_col = NULL, nb_parameters_per_plot_in_col = NULL
    )
    out = lapply(d, fun_raster_1, vec_variable)
    return(out)
  }
  
  # 3. Run code ----------
  # 3.1. Presence absence for each germplasm, location and year
  if(plot_type == "pam"){ 
    p_out = fun_pam(data, vec_variables) 
  }
  
  # 3.2. histogramm, barplot, boxplot, interaction ----------
  if( is.element(plot_type, c("histogramm", "barplot", "boxplot", "interaction") )) { 
    p_out = fun_hbbi(data, vec_variables,
                     x_axis, nb_parameters_per_plot_x_axis, 
                     in_col, nb_parameters_per_plot_in_col, 
                     plot_type)  
  }
  
  # 3.3. biplot ----------
  if(plot_type == "biplot") {
    p_out = fun_biplot(data, vec_variables, labels_on, labels_size,
                       x_axis, nb_parameters_per_plot_x_axis, 
                       in_col, nb_parameters_per_plot_in_col)
  }
  
  # 3.4. radar ----------
  if(plot_type == "radar") {
    p_out = fun_radar(data, vec_variables, in_col, labels_size)
  }
  
  # 3.5. raster ----------
  if(plot_type == "raster") {
    p_out = fun_raster(data, vec_variables, x_axis, nb_parameters_per_plot_x_axis)
  } 
   
  # 4. Return results ----------
  return(p_out)
}

