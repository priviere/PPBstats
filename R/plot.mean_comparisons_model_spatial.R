#' Get ggplot from mean_comparisons_model_spatial
#'
#' @description
#' \code{ggplot_x} returns ggplot from \code{\link{x}}
#' 
#' @param out_x outputs from \code{\link{x}}
#' 
#' @details See \code{\link{get_ggplot}}
#' 
#' @return See \code{\link{get_ggplot}}
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{get_ggplot}}, 
#' \item \code{\link{x}}
#' }
#'
plot.mean_comparisons_model_spatial <- function(
  x,
  nb_parameters_per_plot = 8
){
  
  # 1. Error message ----------
  
  variable = x$spatial$info$variable
  
  data_ggplot_LSDbarplot_germplasm = x$data_ggplot_LSDbarplot_germplasm
  
  # 2. Functions used in the function ----------
  
  ggplot_LSDbarplot = function(d_LSD, fac, variable, nb_parameters_per_plot){
    
    d_LSD = arrange(d_LSD, means) 
    d_LSD$max = max(d_LSD$means, na.rm = TRUE)
    d_LSD$split = add_split_col(d_LSD, nb_parameters_per_plot)
    d_LSD_split = plyr:::splitter_d(d_LSD, .(split))  
    
    out = lapply(d_LSD_split, function(dx){
      p = ggplot(dx, aes(x = reorder(parameter, means), y = means)) + geom_bar(stat = "identity")
      p = p + geom_text(aes(x = reorder(parameter, means), y = means/2, label = groups), angle = 90, color = "white")
      p = p + ggtitle(paste(fac, "\n alpha = ", dx[1, "alpha"], "; alpha correction :", dx[1, "alpha.correction"]))
      p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + coord_cartesian(ylim = c(0, dx[1,"max"])) + ylab(variable)
      return(p)
    })
    
    return(out)
  }
  
  # 3. Germplasm ----------
  if( !is.null(data_ggplot_LSDbarplot_germplasm) ){ 
    ggplot_LSDbarplot_germplasm = ggplot_LSDbarplot(data_ggplot_LSDbarplot_germplasm, "germplasm", variable, nb_parameters_per_plot) 
  } else {
    ggplot_LSDbarplot_germplasm = NULL
  }
  
  # 6. return results ----------
  out = list(
    "germplasm" = ggplot_LSDbarplot_germplasm
  )
  
  return(out)
}

