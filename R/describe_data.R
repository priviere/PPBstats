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
    check_data_vec_variables(data, vec_variables)
    
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
      out_all = ggplot(dtmp, aes(variable)) + geom_histogram() + ggtitle(variable)
      
      # per germplasm
      dtmp_g =  split_data_for_ggplot(dtmp, "germplasm", nb_parameter_per_grid)
      out_g = lapply(dtmp_g, function(x){ggplot(x, aes(variable)) + geom_histogram() + facet_grid(germplasm ~ .)+ ggtitle(variable) })

      # per location
      dtmp_l = split_data_for_ggplot(dtmp, "location", nb_parameter_per_grid)
      out_l = lapply(dtmp_l, function(x){ggplot(x, aes(variable)) + geom_histogram() + facet_grid(location ~ .) + ggtitle(variable) })
      
      # per year
      dtmp_y = split_data_for_ggplot(dtmp, "year", nb_parameter_per_grid)
      out_y = lapply(dtmp_y, function(x){ggplot(x, aes(variable)) + geom_histogram() + facet_grid(year ~ .) + ggtitle(variable) })
      
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
