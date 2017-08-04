#' Get ggplot to describe the data
#'
#' @description
#' \code{describe_data} gets ggplot to describe the data
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
#' }
#' Each list is divided in several lists according to values 
#' of nb_parameters_per_plot_x_axis and nb_parameters_per_plot_in_col.
#' 
#' 
#' @author Pierre Riviere
#' 
describe_data <- function(x, ...) UseMethod("describe_data")

describe_data.default <- function(x, ...) {
  ## No method found
  mess = paste(substitute(x),
               "must come from PPBstats::format_data_PPBstats()."
  )
  stop(mess)
}
