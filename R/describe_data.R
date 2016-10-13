# 0. help ----------
#' Describe the data set in order to choose the appropriate analysis to carry out
#'
#' @description
#' \code{describe_data} describes the data set in order to choose the appropriate analysis to carry out
#' 
#' @param data 
#' 
#' @param vec_variables Vector of variables to describe
#'  
#' @param nb_parameter_per_grid Nomber of parameter on each histogram on the gird
#'  
#' @return The function returns a list with for each environment, the estimated value of the germplasms that were not grown in this environment.
#' 
#' 
#' @return 
#' The function returns a list with, for each variable, a list with :
#' \itemize{
#'  \item A presence.abscence plot
#'  \item Histogram of the variable
#'  \item Histogram of the variable per germplasm
#'  \item Histogram of the variable per location
#'  \item Histogram of the variable per year
#' }
#' 
#' @author Pierre Riviere
#' 
#' 
describe_data = function(
  data,
  vec_variables,
  nb_parameter_per_grid = 5
)
  # let's go !!! ----------
  {
    # 1. Error message ----------  
    mess = "The following column are compulsory : c(\"year\", \"germplasm\", \"location\", \"block\", \"X\", \"Y\"."
    if(!is.element("year", colnames(data))) { stop(mess) }
    if(!is.element("germplasm", colnames(data))) { stop(mess) }
    if(!is.element("location", colnames(data))) { stop(mess) }
    if(!is.element("block", colnames(data))) { stop(mess) }
    if(!is.element("X", colnames(data))) { stop(mess) }
    if(!is.element("Y", colnames(data))) { stop(mess) }
    
    for(variable in vec_variables) { if(!is.element(variable, colnames(data))) { stop(variable," is not in data") } }
    
    
    # 2. Description for each variable ----------
    fun = function(variable, data){

    colnames(data)[which(colnames(data) == variable)] = "variable"
    
      # 2.1. Presence/abscence for each germplasm, location and year
      dtmp = droplevels(na.omit(data[,c("germplasm", "location", "year", "variable")]))
      m = as.data.frame(with(dtmp, table(germplasm, location, year)))
      m$Freq = as.factor(m$Freq)
      colnames(m)[4] = "nb_measures"

      p = ggplot(m, aes(x = germplasm, y = location))
      p = p + geom_raster(aes(fill = nb_measures)) + facet_grid(year ~ .)
      p = p + ggtitle(paste(gettext("Presence / abscence reparition for"), variable))
      
      out.presence.abscence = p

      # 2.2. Histogrammes 
      out_all = ggplot(dtmp, aes(variable)) + geom_histogram()
      
      # per germplasm
      ns = unique(dtmp$germplasm)
      s = rep(c(1:length(ns)), each = nb_parameter_per_grid)[1:length(ns)]
      names(s) = ns
      dtmp$split_germplasm = s[dtmp$germplasm]
      dtmp_g =  plyr:::splitter_d(dtmp, .(split_germplasm))
      out_g = lapply(dtmp_g, function(x){ggplot(x, aes(variable)) + geom_histogram() + facet_grid(germplasm ~ .)})

      # per location
      ns = unique(dtmp$location)
      s = rep(c(1:length(ns)), each = nb_parameter_per_grid)[1:length(ns)]
      names(s) = ns
      dtmp$split_location = s[dtmp$location]
      dtmp_l =  plyr:::splitter_d(dtmp, .(split_location))
      out_l = lapply(dtmp_l, function(x){ggplot(x, aes(variable)) + geom_histogram() + facet_grid(location ~ .)})
      
      # per year
      ns = unique(dtmp$year)
      s = rep(c(1:length(ns)), each = nb_parameter_per_grid)[1:length(ns)]
      names(s) = ns
      dtmp$split_year = s[dtmp$year]
      dtmp_y =  plyr:::splitter_d(dtmp, .(split_year))
      out_y = lapply(dtmp_g, function(x){ggplot(x, aes(variable)) + geom_histogram() + facet_grid(year ~ .)})
      
      OUT = list("presence.abscence" = out.presence.abscence, 
                 "histogram_all" = out_all, 
                 "histogram_per_germplasm" = out_g, 
                 "histogram_per_location" = out_l, 
                 "histogram_per_year" = out_y)
    }
    
    OUT = lapply(vec_variables, fun, data)
    names(OUT) = vec_variables
    
return(OUT)
    }
