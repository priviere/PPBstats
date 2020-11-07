#' Run interface to use the package
#'
#' @description
#' \code{PPBstats_interface} runs an interface to use the package
#' 
#' @param interface Type of interface : "network", "agro", "organo" or "mol"
#' 
#' @details 
#' The code was taken from Dean Attali's example
#' https://www.r-bloggers.com/supplementing-your-r-package-with-a-shiny-app-2/
#' 
#' @return 
#' The function returns a windows with the interface
#' 
#' @author 
#' Pierre Riviere
#' 
#' @import shiny
#' @import shinydashboard
#' 
#' @export
#' 
PPBstats_interface <- function(interface) {
  # locate all the shiny app that exist
  valid_interface <- list.files(system.file("shiny_interfaces", package = "PPBstats"))
  
  # if an invalid example is given, throw an error
  if (missing(interface) || !nzchar(interface) ||
      !interface %in% valid_interface) {
    stop(
      "Please run `PPBstats_interface() with a valid interface app : ",
      paste(valid_interface, collapse = "', '")
    )
  }
  
  # find and launch the app
  appDir <- system.file("shiny_interfaces", interface, package = "PPBstats")
  shiny::runApp(appDir, display.mode = "normal")
}
