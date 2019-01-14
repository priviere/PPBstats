#' Get mean comparisons from \code{\link{check_model.fit_model_home_away}} object
#'
#' @description
#' \code{mean_comparisons} performs mean comparisons from object coming from \code{\link{check_model.fit_model_home_away}}
#'
#' @param x outputs from \code{\link{check_model.fit_model_home_away}}
#' 
#' @param alpha level of type one error. 0.05 (5\%) by default
#' 
#' @param p.adj
#' NULL for no adjustement of the type one error. 
#' p.adj can be "tukey".
#' 
#' @param ... further arguments passed to or from other methods
#' 
#' @details
#' S3 method.
#' Mean comparisons based on LSmeans.
#' See in the book for mo arere details \href{https://priviere.github.io/PPBstats_book/intro-agro.html#section-freq}{here}
#' 
#' @return 
#'  A list of four elements : 
#'   \itemize{
#'    \item info : a list with variable and home_away analysis
#'    \item data_ggplot_LSDbarplot_version:germplasm
#'    \item data_ggplot_LSDbarplot_germplasm
#'    \item data_ggplot_LSDbarplot_location
#'    \item data_ggplot_LSDbarplot_year
#'   }
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{plot.PPBstats}}
#'  \item \code{\link{plot.mean_comparisons_model_home_away}}
#' }
#' 
#' @export
#' 
#' @import lsmeans
#' 
mean_comparisons.check_model_home_away <- function(
  x, 
  alpha = 0.05,
  p.adj = "none",
  ...
){
  # get info
  model = x$model_home_away$ANOVA$model
  variable = x$model_home_away$info$variable
  
  # run LSmeans
  if(length(grep("year", attr(model$terms,"term.labels"))) == 0){
    LSmeans = list("location" = lsmeans::lsmeans(model,"location"),
                   "germplasm" = lsmeans::lsmeans(model,"germplasm"),
                   "version" = lsmeans::lsmeans(model,"version"),
                   # "block:location" = lsmeans::lsmeans(model, pairwise~block|location),
                   "version:germplasm" = lsmeans::lsmeans(model, pairwise~version|germplasm)
    )
    fac_single = c("location", "germplasm") 
    
  } else {
    LSmeans = list("location" = lsmeans::lsmeans(model,"location"),
                   "germplasm" = lsmeans::lsmeans(model,"germplasm"),
                   "year" = lsmeans::lsmeans(model,"year"),
                   "version" = lsmeans::lsmeans(model,"version"),
                   # "block:location" = lsmeans::lsmeans(model, pairwise~block|location:year),
                   "location:year" = lsmeans::lsmeans(model, pairwise~year|location),
                   "version:germplasm" = lsmeans::lsmeans(model, pairwise~version|germplasm),
                   "version:germplasm:year" = lsmeans::lsmeans(model, pairwise~version|year:germplasm)
    )
    fac_single = c("location", "germplasm", "year")
  }
  
  # mean comparisons
  # ftg = grep(":", names(LSmeans))
  fac_inter = "version:germplasm" # names(LSmeans)[ftg]

  out_fac_inter = lapply(LSmeans[fac_inter], function(x){
    x = LSmeans[fac_inter][[1]]
    out_pairs = pairs(x, alpha = alpha, Letters = letters, adjust = p.adj)
    out_pairs = as.data.frame(summary(x$lsmeans))
    
    out_stars = as.data.frame(summary(x$contrasts))
    stars = sapply(out_stars$p.value, function(pvalue){
      if(is.null(pvalue)) { stars = " "} else {
        if(pvalue < 0.001) { stars = "***" }
        if(pvalue > 0.001 & pvalue < 0.05) { stars = "**" }
        if(pvalue > 0.05 & pvalue < 0.01) { stars = "*" }
        if(pvalue > 0.01) { stars = "." }
      }
      return(stars)
    })
    names(stars) = out_stars[,2]
    
    out_pairs$stars = stars[out_pairs[,2]]
    
    out_pairs = data.frame("parameter" = out_pairs[,2], 
                           "version" = out_pairs[,1],
                           "means" = out_pairs[,"lsmean"],
                           "stars" = out_pairs[,"stars"],
                           "alpha" = alpha,
                           alpha.correction = p.adj)
    return(out_pairs)
  } 
  )
  names(out_fac_inter) = paste("data_ggplot_LSDbarplot_", fac_inter, sep = "")
  
  
  out_fac_single = lapply(LSmeans[fac_single], function(x, alpha, p.adj) {
    out_cld = lsmeans::cld(x, alpha = alpha, Letters = letters, adjust = p.adj)
    out_cld = data.frame("parameter" = out_cld[,1], 
                         "means" = out_cld[,2], 
                         "groups" = out_cld[,7],
                         "alpha" = alpha,
                         alpha.correction = p.adj)
    return(out_cld)
    }, alpha = alpha, p.adj = p.adj
  )
  names(out_fac_single) = paste("data_ggplot_LSDbarplot_", fac_single, sep = "")
  
  out = c(list("info" = x$model_home_away$info), c(out_fac_inter, out_fac_single))
  class(out) <- c("PPBstats", "mean_comparisons_model_home_away")
  return(out)
}
