#' Return a "discrimitiveness vs representativeness" ggplot based on PCA object
#' 
#' @description
#' \code{ggplot_discrimitiveness_vs_representativeness} returns a "discrimitiveness vs representativeness" ggplot based on PCA object
#' 
#' @param res.pca output from \code{\link{biplot_data.check_model_GxE}}
#'
#' @details
#' It is a list of three elements.
#' The two first elements where the plots are drawn under the same construction.
#' A red circle indicates the position of the average location, which is defined by the average Dim1 and Dim2 scores across all locations.
#' An arrow points this average 'virtual' location.
#' A red line passes through the biplot origin and the average location (known as the Average-Tester Axis (ATA)).
#' 
#' \itemize{
#'  \item discrimitiveness : assess discrimitiveness of each location.
#'  The score represents the discrimination : the higher the vector, the more discriminating the location.
#'      
#'  \item representativeness : assess representativeness of each location.
#'  The score represents the representativeness : the higher the score, the less representative the location.
#'      
#'  \item discrimitiveness_vs_representativeness :  represent discrimitiveness versus representativeness.
#'  The location combining better score (i. e.discrimination and representativeness) are the ones that could be used to test germplasms as they are more representative of all the locations.
#'  This has to be done severals year to get robust results.
#'  The highest the score, the more disciminative and representative the location.
#'  }
#'   
#' @return 
#' A ggplot object.
#' 
#' @author Pierre Riviere
#' 
#' @seealso
#' \code{\link{plot.PPBstats}}
#' \code{\link{plot.biplot_GxE}}
#' 
#' @import ggplot2
#' @import dplyr
#' 
#' @export
#' 
ggplot_discrimitiveness_vs_representativeness = function(res.pca){
  color = score = score_discrimitiveness = score_representativeness = label = NULL  # to avoid no visible binding for global variable
  
  p = get_biplot(res.pca)
  
  var = dplyr::filter(p$data, color == "darkgreen")
  xymean = data.frame(x1 = 0, y1 = 0, x2 = mean(var$x), y2 = mean(var$y))
  p = p + geom_point(data = xymean, aes(x = x2, y = y2), shape = 21, size = 5, color = "red", fill = "white", stroke = 2, alpha = 0.7, inherit.aes = FALSE) # add mean location
  p = p + geom_segment(data = xymean, aes(x = x1, y = y1, xend = x2, yend = y2), arrow=arrow(length=unit(0.4,"cm")), color = "red", inherit.aes = FALSE)
  p = p + geom_abline(intercept = 0, slope = xymean$y2 / xymean$x2, color = "red") # add line that passes through the biplot origin and the average location
  
  p_common = p
  
  # discrimitiveness ----------
  d = data.frame(x1 = 0, y1 = 0, x2 = var$x, y2 = var$y)
  
  # distance of each location from the plot origin
  d$score = NA
  for(i in 1:nrow(d)){
    x1 = d[i,"x1"]
    y1 = d[i,"y1"]
    x2 = d[i,"x2"]
    y2 = d[i,"y2"]
    px = x2-x1
    py = y2-y1
    d[i,"score"] = round(px*px + py*py, 2)
  }
  
  colnames(var)[2:3] = c("x2", "y2")
  a_disc = plyr::join(var, d, by = "x2")[c("label", "x1", "y1", "x2", "y2", "score")]
  vec_disc = as.character(paste("Ranking of locations: \n", paste(a_disc$label, collapse = " > "), sep = ""))
  
  p = p_common + geom_segment(data = d, aes(x = x1, y = y1, xend = x2, yend = y2, color = score), linetype = 2, size = 1, inherit.aes = FALSE)
  p = p + scale_colour_gradient(low = "green", high = "red")
  p_discri = p + ggtitle("Discrimitiveness", vec_disc)
  
  # representativeness ----------
  per_line = data.frame()
  for(i in 1:nrow(var)) {
    x1 = 0
    x2 = xymean$x2
    y1 = 0
    y2 = xymean$y2
    x3 = var$x[i]
    y3 = var$y[i]
    
    obj = get_perpendicular_segment(x1, y1, x2, y2, x3, y3)
    
    per_line = rbind.data.frame(per_line, obj)
  }
  colnames(per_line) = c("x1", "y1", "x2", "y2")
  per_line$score = round((per_line$x1 - per_line$x2)^2 + (per_line$y1 - per_line$y2)^2, 2)
  
  colnames(var)[2:3] = c("x1", "y1")
  a_repr = plyr::join(var, per_line, by = "x1")[c("label", "x1", "y1", "x2", "y2", "score")]
  vec_rank = as.character(paste("Ranking of locations: \n", paste(a_repr$label, collapse = " > "), sep = ""))
  
  p = p_common + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = score), data = per_line, linetype = 2, size = 1, inherit.aes = FALSE)
  p = p + scale_colour_gradient(low = "green", high = "red")
  p_repre = p + ggtitle("Representativeness", vec_rank)

  
  # discrimitiveness vs representativeness
  a_disc = a_disc[,c("label", "score")]
  colnames(a_disc)[2] = "score_discrimitiveness"
  a_repr = a_repr[,c("label", "score")]
  colnames(a_repr)[2] = "score_representativeness"
  d = plyr::join(a_disc, a_repr, by = "label")
  d$score = d$score_discrimitiveness * d$score_representativeness
  p_disc_vs_repr = ggplot(d, aes(x = score_discrimitiveness, y = score_representativeness, label = label, color = score)) + geom_text() 
  p_disc_vs_repr = p_disc_vs_repr + scale_colour_gradient(low = "green", high = "red")
  
  # return results ----------
  p = list(
    "discrimitiveness" = p_discri, 
    "representativeness" = p_repre,
    "discrimitiveness_vs_representativeness" = p_disc_vs_repr
    )

  return(p)
}

