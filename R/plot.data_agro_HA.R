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
#' @param f_grid facet grid to add, possible value is "year"
#' 
#' @param ... further arguments passed to or from other methods
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
#' }
#' 
#' @import plyr
#' 
#' @export
#' 
plot.data_agro_HA = function(
  x,
  plot_type = "boxplot",
  vec_variables = NULL,
  f_grid = NULL,
  ...
){
  match.arg(plot_type, c("barplot", "boxplot"), several.ok = FALSE)
  match.arg(f_grid, "year", several.ok = FALSE)
  origin_bis = factor_to_split = germplasm = NULL # to avoid no visible binding for global variable
  
  x$origin_bis =  paste("sown at", x$location, ", coming from", x$origin)

  fun_var = function(variable, d, plot_type){
    colnames(d)[which(colnames(d) == variable)] = "variable"
    
    # single plot with version for all germplasm merged
    # Be careful with facet_grid: 
    # cf https://stackoverflow.com/questions/46279720/using-dodge-position-in-ggplot-changing-column-values
    
    if(!is.null(f_grid)){
      colnames(d)[which(colnames(d) == f_grid)] = "f_grid"
      
      if( plot_type == "barplot"){ 
        d$toto = paste(d$version, d$f_grid, sep = "azerty")
        mm = ddply(d, "toto", summarise, mean = mean(variable, na.rm = TRUE), sd = sd(variable, na.rm = TRUE))
        mm$version = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[1]}))
        mm$f_grid = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[2]}))
      }
      
      if( plot_type == "barplot"){ 
        p = ggplot(mm, aes(x = version, y = mean))
        p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
        p = p + geom_bar(stat = "identity", position = "dodge") + facet_grid(.~f_grid) 
        # limits <- aes(ymax = mean + sd, ymin = mean - sd)
        # p = p + geom_errorbar(limits, position = position_dodge(width=0.9), width=0.25)
      }
      
      if( plot_type == "boxplot"){ 
        p = ggplot(d, aes(x = version, y = variable))
        p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
        p = p + geom_boxplot(position = "dodge") + facet_grid(.~f_grid) 
      }
      
    } else {
      p = ggplot(d, aes(x = version, y = variable))
      p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
      if( plot_type == "barplot"){ p = p + geom_bar(stat = "identity", position = "dodge") }
      if( plot_type == "boxplot"){ p = p + geom_boxplot(position = "dodge") }
    }
    
    p1 = p
    
    
    # single plot with version for each germplasm
    if(!is.null(f_grid)){
      colnames(d)[which(colnames(d) == f_grid)] = "f_grid"
      
      if( plot_type == "barplot"){ 
        d$toto = paste(d$germplasm, d$version, d$f_grid, sep = "azerty")
        mm = ddply(d, "toto", summarise, mean = mean(variable, na.rm = TRUE), sd = sd(variable, na.rm = TRUE))
        mm$germplasm = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[1]}))
        mm$version = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[2]}))
        mm$f_grid = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[3]}))
      }
      
      if( plot_type == "barplot"){ 
        p = ggplot(mm, aes(x = germplasm, y = mean, fill = version))
        p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
        p = p + geom_bar(stat = "identity", position = "dodge") + facet_grid(.~f_grid) 
        # limits <- aes(ymax = mean + sd, ymin = mean - sd)
        # p = p + geom_errorbar(limits, position = position_dodge(width=0.9), width=0.25)
      }
      
      if( plot_type == "boxplot"){ 
        p = ggplot(d, aes(x = germplasm, y = variable, fill = version))
        p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
        p = p + geom_boxplot(position = "dodge") + facet_grid(.~f_grid) 
      }
      
    } else {
      p = ggplot(d, aes(x = germplasm, y = variable))
      p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
      if( plot_type == "barplot"){ p = p + geom_bar(aes(fill = version), stat = "identity", position = "dodge") }
      if( plot_type == "boxplot"){ p = p + geom_boxplot(aes(fill = version), position = "dodge") }
    }
    
    p2 = p
    
    
    # list of plots for each germplasm with all version separated
    colnames(d)[which(colnames(d) == "germplasm")] = "factor_to_split"
    dd = plyr:::splitter_d(d, .(factor_to_split))
    
    
    out = lapply(dd, function(x, f_grid){
      
      if(!is.null(f_grid)){
        colnames(x)[which(colnames(x) == f_grid)] = "f_grid"
        
        if( plot_type == "barplot"){ 
          x$toto = paste(x$origin_bis, x$origin, x$version, x$f_grid, sep = "azerty")
          mm = ddply(x, "toto", summarise, mean = mean(variable, na.rm = TRUE), sd = sd(variable, na.rm = TRUE))
          mm$origin_bis = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[1]}))
          mm$origin = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[2]}))
          mm$version = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[3]}))
          mm$f_grid = as.factor(sapply(mm$toto, function(x){unlist(strsplit(x, "azerty"))[4]}))
        }
        
        if( plot_type == "barplot"){ 
          p = ggplot(mm, aes(x = origin_bis, y = mean))
          p = p + ggtitle(x[1, "factor_to_split"]) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
          p1 = p + geom_bar(aes(fill = version), stat = "identity", position = "dodge") + facet_grid(.~f_grid) 
          p2 = p + geom_bar(aes(fill = origin), stat = "identity", position = "dodge") + facet_grid(.~f_grid) 
          # limits <- aes(ymax = mean + sd, ymin = mean - sd)
          # p = p + geom_errorbar(limits, position = position_dodge(width=0.9), width=0.25)
        }
        
        if( plot_type == "boxplot"){ 
          p = ggplot(d, aes(x = origin_bis, y = variable))
          p = p + ggtitle(x[1, "factor_to_split"]) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
          p1 = p + geom_boxplot(aes(fill = version), position = "dodge") + facet_grid(.~f_grid) 
          p2 = p + geom_boxplot(aes(fill = origin), position = "dodge") + facet_grid(.~f_grid) 
        }
        
      } else {
        p = ggplot(x, aes(x = origin_bis, y = variable))
        p = p + ggtitle(x[1, "factor_to_split"]) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")
        if( plot_type == "barplot"){
          p1 = p + geom_bar(aes(fill = version), stat = "identity", position = "dodge")
          p2 = p + geom_bar(aes(fill = origin), stat = "identity", position = "dodge")
        }
        if( plot_type == "boxplot"){
          p1 = p + geom_boxplot(aes(fill = version), position = "dodge")
          p2 = p + geom_boxplot(aes(fill = origin), position = "dodge")
        }
      }
      
      p_out = list("version" = p1, "origin" = p2)
      return(p_out)
    }, f_grid)
    
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


