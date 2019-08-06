#' Plot hedonic object from \code{\link{format_data_PPBstats.data_organo_hedonic}}
#'
#' @description
#' \code{plot.data_hedonic} gets ggplot to describe the data
#' 
#' @param x The data frame coming from \code{\link{format_data_PPBstats.data_organo_hedonic}}
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
#'  \item "map"
#' }
#' 
#' @param x_axis factor displayed on the x.axis of a plot. 
#' "date_julian" can be choosen: it will  display julian day for a given variable automatically calculated from format_data_PPBstats(). 
#' This is possible only for plot_type = "histogramm", "barplot", "boxplot" and "interaction".
#' @param in_col factor displayed in color of a plot
#' 
#' @param vec_variables vector of variables to display
#' 
#' @param nb_parameters_per_plot_x_axis the number of parameters per plot on x_axis arguments
#' 
#' @param nb_parameters_per_plot_in_col the number of parameters per plot for in_col arguments
#' 
#' @param labels_on factor to display for plot_type = "biplot"
#' 
#' @param labels_size size of the label for plot_type = "biplot" and "radar"
#' 
#' @param pie_size when plot_type = "map" and vec_variables is not NULL, size of the pie 
#' 
#' @param zoom zoom of the map, see ?get_map for more details
#' 
#' @param ... further arguments passed to or from other methods
#' 
#' @return 
#' \itemize{
#'  \item For plot_type "histogramm", "barplot", "boxplot" or "interaction",
#'  the function returns a list with ggplot objects for each variable of vec_variables.
#'  \item For plot_type "biplot",
#'  the function returns a list with ggplot objects for each pairs of variables of vec_variables. 
#'  \item For plot_type "radar" and "raster,
#'  the function returns a list with ggplot objects with all variables of vec_variables. 
#'  \item For plot_type "map", it returns a map with location 
#'  if vec_variables = NULL and labels_on = "location".
#'  If vec_variables is not NULL, it displays pie on map.
#' }
#' Each list is divided in several lists according to values 
#' of nb_parameters_per_plot_x_axis and nb_parameters_per_plot_in_col except for plot_type = "map".
#' 
#' @author Pierre Riviere
#'
#' @details 
#' See the book for more details : https://priviere.github.io/PPBstats_book/hedonic.html#describe-the-data-2
#'
#' @seealso 
#' \code{\link{format_data_PPBstats.data_organo_hedonic}}
#' 
#' @export
#'
plot.data_organo_hedonic = function(  x,
                                      plot_type = "boxplot",
                                      x_axis = NULL,
                                      in_col = NULL,
                                      vec_variables = NULL,
                                      nb_parameters_per_plot_x_axis = 5,
                                      nb_parameters_per_plot_in_col = 5,
                                      labels_on = NULL,
                                      labels_size = 4,
                                      pie_size = 0.2, zoom = 6, ...){
  
  match.arg(plot_type, c("pam", "histogramm", "barplot", "boxplot", "interaction", "biplot", "radar", "raster", "map"), several.ok = FALSE)
  
  p_out = plot_descriptive_data(
    x$data$data_sample,
    plot_type,
    x_axis,
    in_col,
    vec_variables,
    nb_parameters_per_plot_x_axis,
    nb_parameters_per_plot_in_col,
    labels_on,
    labels_size,
    pie_size,
    zoom
  )
  return(p_out)
}


