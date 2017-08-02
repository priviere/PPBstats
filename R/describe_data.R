#' Describe the data set in order to choose the appropriate analysis to carry out
#'
#' @description
#' \code{describe_data} describes the data set in order to choose the appropriate analysis to carry out
#' 
#' @param data  The data frame. It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "..."), with "..." the variables.
#' 
#' @param vec_variables Vector of variables to describe
#'  
#' @param nb_parameters_per_plot Number of parameter on each histogram on the gird
#' 
#' @return 
#' The function returns a list with, 
#' 
#' \itemize{
#'  \item A summary of the whole data set
#'  \item for each variable, a list with :
#'   \itemize{
#'   \item A presence.absence plot  that represents the presence/absence matrix of GxE combinaisons. 
#'   \item A list with histogram for
#'    \itemize{
#'     \item germplasm
#'     \item location
#'     \item year
#'     }
#'    For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#'   \item A list with boxplot, containg a list with plot and outliers, for
#'    \itemize{
#'     \item germplasm
#'     \item location
#'     \item year
#'     }
#'    For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#'    \item interaction
#'    For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#'   }
#' }
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
