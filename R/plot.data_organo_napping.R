#' Plot napping object from format_data_PPBstats()
#'
#' @description
#' \code{plot.data_napping} gets ggplot to describe the data
#' 
#' @param data The data frame coming from format_data_PPBstats() with type = "data_organo_napping"
#' 
#' @param ... other parameters are the same as plot.data_agro.
#' 
#' @details
#' See \code{\link{plot.data_agro}}
#' 
#' @return 
#' See \code{\link{plot.data_agro}}
#' 
#' @author Pierre Riviere
#' 
plot.data_organo_napping = function(  data,
                                      data_version = NULL,
                                      plot_type = c("pam", "histogramm", "barplot", "boxplot", "interaction", "biplot", "radar", "raster", "map"),
                                      x_axis = NULL,
                                      in_col = NULL,
                                      vec_variables = NULL,
                                      nb_parameters_per_plot_x_axis = 5,
                                      nb_parameters_per_plot_in_col = 5,
                                      labels_on = NULL,
                                      labels_size = 4,
                                      pie_size = 0.2){
  
  out = plot.data_agro(
    data$data,
    data_version = data_version,
    plot_type = plot_type,
    x_axis = x_axis,
    in_col = in_col,
    vec_variables = vec_variables,
    nb_parameters_per_plot_x_axis = nb_parameters_per_plot_x_axis,
    nb_parameters_per_plot_in_col = nb_parameters_per_plot_in_col,
    labels_on = labels_on,
    labels_size = labels_size,
    pie_size = pie_size
  )  
  return(out)
}


