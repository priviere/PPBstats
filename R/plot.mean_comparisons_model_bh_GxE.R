#' Get ggplot to visualize output from \code{\link{mean_comparisons.check_model_bh_GxE}}
#'
#' @description
#' \code{plot.mean_comparisons_model_bh_GxE} returns ggplot to visualize outputs from \code{\link{mean_comparisons.check_model_bh_GxE}}
#'
#' @param x Output from \code{\link{mean_comparisons.check_model_bh_GxE}} for a given parameter
#'
#' @param y Output from \code{\link{mean_comparisons.check_model_bh_GxE}} for a given parameter
#'
#' @param plot_type "biplot-alpha-beta" or "barplot"
#'
#' @param nb_parameters_per_plot number of parameter per plot to display
#'
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' y can not be NULL for plot_type = "biplot-alpha-beta".
#' See example in the book: https://priviere.github.io/PPBstats_book/family-2.html#model-2
#'
#' @return
#' A list with ggplot object depending on plot_type.
#' There are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#' \itemize{
#'  \item barplot :
#'  Letters are displayed on each bar.
#'  Parameters that do not share the same letters are different regarding type I error (alpha) and alpha correction.
#'  The error I (alpha) and the alpha correction are displayed in the title.
#'  alpha = Imp means that no differences were possible to find.
#'  \item biplot-alpha-beta : display the biplot with alpha_i on the x axis and beta_i on the y axis if
#'  x and y are coming respectively from \code{\link{mean_comparisons.check_model_bh_GxE}} for alpha and beta.
#'  }
#'
#' @author Pierre Riviere
#'
#' @seealso \code{\link{mean_comparisons.check_model_bh_GxE}}
#'
#' @export
#'
#' @import dplyr
#' @import plyr
#' @import ggplot2
#'
plot.mean_comparisons_model_bh_GxE <- function(
  x,
  y = NULL,
  plot_type = c("biplot-alpha-beta", "barplot"),
  nb_parameters_per_plot = 8, ...
) {
  parameter = alpha_i = beta_i = germplasm = NULL # to avoid no visible binding for global variable

  # 1. Error message
  plot_type <- match.arg(plot_type, several.ok = FALSE)

  if( is.null(y) & plot_type == "biplot-alpha-beta" ) {
    stop("With plot_type = \"biplot-alpha-beta\", y can not be NULL.")
  }

  if (!is.null(y) && !inherits(y, "mean_comparisons_model_bh_GxE")) {
    stop(substitute(y), "must come from PPBstats::check_model from model_bh_GxE.")
  }

  data_Mpvalue = x$Mpvalue
  data = x$mean.comparisons

  if(plot_type == "barplot") {
    data = dplyr::arrange(data, median)
    data$max = max(data$median, na.rm = TRUE)
    data$split = add_split_col(data, nb_parameters_per_plot)
    data_split = plyr:::splitter_d(data, .(split))

    para.name = unlist(strsplit(as.character(data[1, "parameter"]), "\\["))[1]

    out = lapply(data_split, function(dx){
      p = ggplot(dx, aes(x = reorder(parameter, median), y = median)) + geom_bar(stat = "identity")
      p = p + geom_text(aes(x = reorder(parameter, median), y = median/2, label = groups), angle = 90, color = "white")
      p = p + ggtitle(paste(para.name, "\n alpha = ", dx[1, "alpha"], "; alpha correction :", dx[1, "alpha.correction"]))
      p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylab("") + coord_cartesian(ylim = c(0, dx[1,"max"]))
      return(p)
    })
    out = list(out)
    names(out) = para.name
  }


  # 2.5. biplot-alpha-beta ----------
  if(plot_type == "biplot-alpha-beta"){

    a = data
    test_a = unlist(strsplit(as.character(a[1,"parameter"]), "\\["))[1]
    if( test_a != "alpha" ){ stop("With plot_type = \"biplot-alpha-beta\", data must come from get.mean.comparisons with paramater = \"alpha\".") }
    a$germplasm = gsub("alpha", "", a$parameter)
    colnames(a)[which(colnames(a) == "parameter")] = "parameter_a"
    colnames(a)[which(colnames(a) == "median")] = "alpha_i"

    b = y$mean.comparisons
    test_b = unlist(strsplit(as.character(b[1,"parameter"]), "\\["))[1]
    if( test_b != "beta" ){ stop("With plot_type = \"biplot-alpha-beta\", data_2 must come from get.mean.comparisons with paramater = \"beta\".") }
    b$germplasm = gsub("beta", "", b$parameter)
    colnames(b)[which(colnames(b) == "parameter")] = "parameter_b"
    colnames(b)[which(colnames(b) == "median")] = "beta_i"


    ab = plyr::join(a, b, "germplasm")
    ab <- ab[!duplicated(as.list(ab))]
    ab=ab[which(!is.na(ab$beta_i) & !is.na(ab$alpha_i)),]
    ab$germplasm = gsub("\\[", "", ab$germplasm)
    ab$germplasm = gsub("\\]", "", ab$germplasm)

    spl = add_split_col(ab, nb_parameters_per_plot)
    ab$split  = sample(spl,size = length(spl), replace=F)
    d_ab = plyr:::splitter_d(ab, .(split))

    xlim = range(ab$alpha_i)
    ylim = range(ab$beta_i)

    out = lapply(d_ab,function(y){
      p = ggplot(y, aes(x = alpha_i, y = beta_i, label = germplasm)) + coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)
      p = p + geom_text() + geom_hline(yintercept = 0)
    })
  }

  # return results
  return(out)
}
