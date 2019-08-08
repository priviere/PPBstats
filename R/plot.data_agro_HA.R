#' Plot agro object from format_data_PPBstats.data_agro_HA()
#'
#' @description
#' \code{plot.data_agro_HA} gets ggplot to describe the data
#' 
#' @param x The data frame. It should come from \code{\link{format_data_PPBstats.data_agro_HA}}
#' 
#' @param plot_type the type of plot you wish. It can be :
#' \itemize{
#'  \item "barplot", where sd error are displayed
#'  \item "boxplot"
#' }
#' 
#' @param vec_variables vector of variables to display
#' 
#' @return 
#' The function returns a list with ggplot objects for each variable of vec_variables divided into three elements:
#' \itemize{
#'  \item home_away_merged, i.e. a single plot with version for all germplasm merged
#'  \item home_away_merged_per_germplasm, i.e. a single plot with version for each germplasm
#'  \item home_away_per_germplasm, i.e. a list of plots for each germplasm with all version separated
#' }
#' 
#' When argument mean_comparison is not NULL, it returns a plot with stars of significant differences.
#' 
#' @author Pierre Riviere
#' 
#' @details 
#' S3 method.
#' See the book for more details \href{https://priviere.github.io/PPBstats_book/home-away.html}{here}.
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{format_data_PPBstats}}
#'  \item \code{\link{format_data_PPBstats.data_agro}}
#' }
#' 
#' @export
#' 
plot.data_agro_HA = function(
  x,
  plot_type = "boxplot",
  vec_variables = NULL,
  ...
){
  match.arg(plot_type, c("barplot", "boxplot"), several.ok = FALSE)
  
  x$group_bis =  paste("sown at", x$location, ", coming from", x$group)

  fun_var = function(variable, d, plot_type){
    colnames(d)[which(colnames(d) == variable)] = "variable"
    
    # single plot with version for all germplasm merged
    p = ggplot(d, aes(x = version, y = variable))
    p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
    if( plot_type == "barplot"){ p = p + geom_bar(stat = "identity", position = "dodge") }
    if( plot_type == "boxplot"){ p = p + geom_boxplot(position = "dodge") }
    p1 = p
    
    # single plot with version for each germplasm
    p = ggplot(d, aes(x = germplasm, y = variable))
    p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
    if( plot_type == "barplot"){
      p = p + geom_bar(aes(fill = version), stat = "identity", position = "dodge")
    }
    if( plot_type == "boxplot"){
      p = p + geom_boxplot(aes(fill = version), position = "dodge")
    }
    p2 = p
    
    # list of plots for each germplasm with all version separated
    colnames(d)[which(colnames(d) == "germplasm")] = "factor_to_split"
    dd = plyr:::splitter_d(d, .(factor_to_split))
    out = lapply(dd, function(x){
      p = ggplot(x, aes(x = group_bis, y = variable))
      p = p + ggtitle(x[1, "factor_to_split"]) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      p = p + xlab("")
      if( plot_type == "barplot"){
        p = p + geom_bar(aes(fill = version), stat = "identity", position = "dodge")
      }
      if( plot_type == "boxplot"){
        p = p + geom_boxplot(aes(fill = version), position = "dodge")
      }
      return(p)
    })
   
    out = list("home_away_merged" = p1,
               "home_away_merged_per_germplasm" = p2,
               "home_away_per_germplasm" = out)
    
    return(out)
  }
  
  out = lapply(vec_variables, 
               fun_var, 
               x, 
               plot_type
               )
  names(out) = vec_variables
  
  return(out)
}


