#' Get ggplot to visualize output from \code{\link{mean_comparisons.check_model_spatial}}
#'
#' @description
#' \code{plot.mean_comparisons_model_spatial} returns ggplot to visualize outputs from \code{\link{mean_comparisons.check_model_spatial}}
#'
#' @param x Output from \code{\link{mean_comparisons.check_model_spatial}} 
#' 
#' @param nb_parameters_per_plot number of parameter per plot to display
#'
#' @param ... further arguments passed to or from other methods
#' 
#' @details
#' S3 method.
#' See example in the book: https://priviere.github.io/PPBstats_book/family-1.html#spatial-analysis
#' 
#' @return 
#' A list with barplot.
#' For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#' Letters are displayed on each bar. Parameters that do not share the same letters are different regarding type I error (alpha) and alpha correction. 
#' The error I (alpha) and the alpha correction are displayed in the title. 
#' \itemize{
#'  \item germplasm : mean comparison for germplasm
#'  }
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{mean_comparisons.check_model_spatial}}
#' 
#' @export
#' 
#' @import dplyr
#' @import plyr
#' @import ggplot2
#' 
plot.mean_comparisons_model_spatial <- function(
  x,
  nb_parameters_per_plot = 8, ...
){
  
  # 1. Error message ----------
  
  variable = x$spatial$info$variable
  
  data_ggplot_LSDbarplot_germplasm = x$data_ggplot_LSDbarplot_germplasm
  
  # 2. Functions used in the function ----------
  
  ggplot_LSDbarplot = function(d_LSD, fac, variable, nb_parameters_per_plot){
    parameter = means = NULL # to avoid no visible binding for global variable
    
    d_LSD = dplyr::arrange(d_LSD, means) 
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
  
  # 4. Germplasm BLUPs with confidence intervalle
  pre = x$blups_prediction
  p_blup = ggplot(pre, aes(y = predicted.values, x = germplasm)) + geom_point() 
  p_blup = p_blup + geom_errorbar(aes(ymin = lower, ymax = upper)) + coord_flip()
  
  # 5. return results ----------
  out = list(
    "germplasm_blup" = p_blup,
    "germplasm_barplot" = ggplot_LSDbarplot_germplasm
  )
  
  return(out)
}

