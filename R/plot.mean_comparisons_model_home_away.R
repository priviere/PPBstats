#' Get ggplot to visualize output from \code{\link{mean_comparisons.check_model_home_away}}
#'
#' @description
#' \code{plot.mean_comparisons_model_home_away} returns ggplot to visualize outputs from \code{\link{mean_comparisons.check_model_home_away}}
#'
#' @param x Output from \code{\link{mean_comparisons.check_model_home_away}} 
#' 
#' @param nb_parameters_per_plot number of parameter per plot to display
#' 
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' See in the book for mo arere details \href{https://priviere.github.io/PPBstats_book/intro-agro.html#section-freq}{here}
#' 
#' @return 
#' A list with barplot.
#' For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#' Letters are displayed on each bar. Parameters that do not share the same letters are different regarding type I error (alpha) and alpha correction. 
#' The error I (alpha) and the alpha correction are displayed in the title. 
#' \itemize{
#'  \item version:germplasm : mean comparison for home away for each germplasm and year
#'  \item germplasm : mean comparison for germplasm
#'  \item location : mean comparison for location
#'  \item year : mean comparison for year
#'  \item version : mean comparison for home and away
#'  }
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{mean_comparisons.check_model_home_away}}
#' 
#' @export
#' 
 plot.mean_comparisons_model_home_away <- function(
  x,
  nb_parameters_per_plot = 8, ...
){

  d = x[["data_ggplot_LSDbarplot_version:germplasm"]]
  d = dplyr::arrange(d, means) 
  d$max = max(d$means, na.rm = TRUE)
  d$split = add_split_col(d, nb_parameters_per_plot)
  d_split = plyr:::splitter_d(d, .(split))  
  
  STARS = unique(d[,c("parameter", "stars")])
  y = tapply(d$means, d$parameter, mean, na.rm = TRUE)
  y = y + (max(y) * 0.2)
  STARS$means = y[STARS$parameter]
  
  fac = "version:germplasm"
  variable = x$info$variable
  
  out_fac_inter = lapply(d_split, function(dx, fac, STARS, variable){
    p = ggplot(dx, aes(x = parameter, y = means))
    p = p + geom_bar(aes(fill = version), position = "dodge", stat = "identity")
    p = p + geom_text(data = STARS, aes(label = stars))
    p = p + ggtitle(paste(fac, "\n alpha = ", dx[1, "alpha"], "; alpha correction :", dx[1, "alpha.correction"]))
    p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + coord_cartesian(ylim = c(0, dx[1,"max"])) + ylab(variable)
    return(p)
  }, fac, STARS, variable)
  names(out_fac_inter) = fac
    
  out_fac_single = plot_mean_comparisons_freq_anova(x, variable = x$info$variable, nb_parameters_per_plot)
  
  out = c(out_fac_inter, out_fac_single)
  return(out)
}

 