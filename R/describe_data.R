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
#' @return 
#' The function returns a list with, 
#' \itemize{
#'  \item A summary of the whole data set
#'  \item for each variable, a list with :
#'   \itemize{
#'   \item A presence.abscence plot
#'   \item A list with histogram for
#'    \itemize{
#'     \item germplasm
#'     \item location
#'     \item year
#'     }
#'   \item A list with boxplot, containg a list with plot and outliers, for
#'    \itemize{
#'     \item germplasm
#'     \item location
#'     \item year
#'     }
#'   }
#' }
#' 
#' @author Pierre Riviere
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
    
    # 2. Description all variables ----------
    summary_all = summary(data)
    
    # 3. Description for each variable ----------
    fun = function(variable, data){

    colnames(data)[which(colnames(data) == variable)] = "variable"
    
      # 2.1. Presence/abscence for each germplasm, location and year
      dtmp = droplevels(na.omit(data[,c("germplasm", "location", "year", "variable")]))
      m = as.data.frame(with(dtmp, table(germplasm, location, year)))
      m$Freq = as.factor(m$Freq)
      colnames(m)[4] = "nb_measures"

      p = ggplot(m, aes(x = germplasm, y = location))
      p = p + geom_raster(aes(fill = nb_measures)) + facet_grid(year ~ .)
      nb_NA = round(length(which(m$nb_measures == 0)) / ( length(which(m$nb_measures == 0)) + length(which(m$nb_measures != 0)) ), 2) * 100
      p = p + ggtitle(
        paste("Presence abscence repartition for ", variable, sep = ""),
        paste("(",  nb_NA, "% of 0)", sep = "")
      )
      out.presence.abscence = p

      # 2.2. Histogram and boxplot
      out_all_hist = ggplot(dtmp, aes(variable)) + geom_histogram() + ggtitle(variable)

      # per germplasm
      dtmp_g =  split_data_for_ggplot(dtmp, "germplasm", nb_parameter_per_grid)
      out_g_hist = lapply(dtmp_g, function(x){ggplot(x, aes(variable)) + geom_histogram() + facet_grid(germplasm ~ .)+ ggtitle(variable) })
      out_g_box = lapply(dtmp_g, function(x){
        plot = ggplot(x, aes(x = germplasm, y = variable)) + geom_boxplot() + ggtitle(variable) 
        outliers = boxplot(x$variable, x$germplasm, plot = FALSE)$out
        names(outliers) = x$germplasm[which(x$variable %in% outliers)]
        return(list("plot" = plot, "outliers" = outliers))
        })

      # per location
      dtmp_l = split_data_for_ggplot(dtmp, "location", nb_parameter_per_grid)
      out_l_hist = lapply(dtmp_l, function(x){ggplot(x, aes(variable)) + geom_histogram() + facet_grid(location ~ .) + ggtitle(variable) })
      out_l_box = lapply(dtmp_l, function(x){
        plot = ggplot(x, aes(x = location, y = variable)) + geom_boxplot() + ggtitle(variable) 
        outliers = boxplot(x$variable, x$location, plot = FALSE)$out
        names(outliers) = x$location[which(x$variable %in% outliers)]
        return(list("plot" = plot, "outliers" = outliers))
      })
      
      # per year
      dtmp_y = split_data_for_ggplot(dtmp, "year", nb_parameter_per_grid)
      out_y_hist = lapply(dtmp_y, function(x){ggplot(x, aes(variable)) + geom_histogram() + facet_grid(year ~ .) + ggtitle(variable) })
      out_y_box = lapply(dtmp_y, function(x){
        plot = ggplot(x, aes(x = year, y = variable)) + geom_boxplot() + ggtitle(variable) 
        outliers = boxplot(x$variable, x$year, plot = FALSE)$out
        names(outliers) = x$year[which(x$variable %in% outliers)]
        return(list("plot" = plot, "outliers" = outliers))
      })
      
      # interaction ----------
      
      p_gxe = ggplot(data = data, aes(x = location, y = variable, colour = germplasm, group = germplasm))
      #  p_gxe = p_gxe + stat_summary(fun.y= mean, geom = "point")
      p_gxe = p_gxe + stat_summary(fun.y = mean, geom = "line", aes(linetype = germplasm), size = 1) # + scale_linetype_manual(values=rep(c("solid", "dotted"), 6))
      
      #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      #cbbPalette <- c("#000000", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
      
      #p_gxe = p_gxe + scale_color_manual(values=rep(cbbPalette, each = 2))
      
      p_gxe = p_gxe + theme(axis.text.x=element_text(angle=90))
      p_gxe = p_gxe + ylab(variable)
      # p2_GxE + ggtitle("") + xlab("") + ylab("") + theme(legend.title=element_blank())
      
      out__gxe = p_gxe + facet_grid(year ~ .)
      
      
      
      OUT = list("presence.abscence" = out.presence.abscence, 
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
                 "interaction" = out__gxe
                 )
    }
    
    out_each_variable = lapply(vec_variables, fun, data)
    names(out_each_variable) = vec_variables
    
    OUT = list("all_variables" = summary_all,
               "each_variable" = out_each_variable
               )
    
    return(OUT)
    }
