#' Get mean comparisons from \code{\link{check_model.fit_model_local_foreign}} object
#'
#' @description
#' \code{mean_comparisons} performs mean comparisons from object coming from \code{\link{check_model.fit_model_local_foreign}}
#'
#' @param x outputs from \code{\link{check_model.fit_model_local_foreign}}
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
#' Mean comparisons based on LSmeans with the R package emmeans.
#' See in the book for mo arere details \href{https://priviere.github.io/PPBstats_book/intro-agro.html#section-freq}{here}
#'
#' @return
#'  A list of four elements :
#'   \itemize{
#'    \item info : a list with variable and local_foreign analysis
#'    \item data_ggplot_LSDbarplot_version:location
#'    \item data_ggplot_LSDbarplot_germplasm
#'    \item data_ggplot_LSDbarplot_location
#'    \item data_ggplot_LSDbarplot_year
#'   }
#'
#' @author Pierre Riviere and Baptiste Rouger
#'
#' @seealso
#' \itemize{
#'  \item \code{\link{mean_comparisons}}
#'  \item \code{\link{plot.PPBstats}}
#'  \item \code{\link{plot.mean_comparisons_model_local_foreign}}
#' }
#'
#' @export
#'
#' @import emmeans
#'
mean_comparisons.check_model_local_foreign <- function(
  x,
  alpha = 0.05,
  p.adj = "none",
  ...
){
  # get info
  model = x$model_local_foreign$ANOVA$model
  variable = x$model_local_foreign$info$variable

  # run LSmeans
  if(length(grep("year", attr(model$terms,"term.labels"))) == 0){
    LSmeans = list("location" = emmeans::emmeans(model,"location"),
                   "germplasm" = emmeans::emmeans(model,"germplasm"),
                   "version" = emmeans::emmeans(model,"version"),
                   # "block:location" = emmeans::emmeans(model, pairwise~block|location),
                   "version:location" = emmeans::emmeans(model, pairwise~version|location)
    )
    fac_single = c("location", "germplasm")

  } else {
    LSmeans = list("location" = emmeans::emmeans(model,"location"),
                   "germplasm" = emmeans::emmeans(model,"germplasm"),
                   "year" = emmeans::emmeans(model,"year"),
                   "version" = emmeans::emmeans(model,"version"),
                   # "block:location" = emmeans::emmeans(model, pairwise~block|location:year),
                   "location:year" = emmeans::emmeans(model, pairwise~year|location),
                   "version:location" = emmeans::emmeans(model, pairwise~version|location),
                   "version:location:year" = emmeans::emmeans(model, pairwise~version|year:location)
    )
    fac_single = c("location", "germplasm", "year")
  }

  # mean comparisons
  # ftg = grep(":", names(LSmeans))
  fac_inter = "version:location" # names(LSmeans)[ftg]

  out_fac_inter = lapply(LSmeans[fac_inter], function(x){
    out_pairs = as.data.frame(summary(x$emmeans))

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
                           "means" = out_pairs[,"emmean"],
                           "stars" = out_pairs[,"stars"],
                           "alpha" = alpha,
                           alpha.correction = p.adj)
    return(out_pairs)
  }
  )
  names(out_fac_inter) = paste("data_ggplot_LSDbarplot_", fac_inter, sep = "")


  out_fac_single = lapply(LSmeans[fac_single], function(x, alpha, p.adj) {
    out_cld = emmeans::CLD(x, alpha = alpha, Letters = letters, adjust = p.adj)
    out_cld = data.frame("parameter" = out_cld[,1],
                         "means" = out_cld[,2],
                         "groups" = out_cld[,7],
                         "alpha" = alpha,
                         alpha.correction = p.adj)
    return(out_cld)
    }, alpha = alpha, p.adj = p.adj
  )
  names(out_fac_single) = paste("data_ggplot_LSDbarplot_", fac_single, sep = "")

  out = c(list("info" = x$model_local_foreign$info), c(out_fac_inter, out_fac_single))
  class(out) <- c("PPBstats", "mean_comparisons_model_local_foreign")
  return(out)
}
