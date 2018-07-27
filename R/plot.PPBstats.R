#' Get ggplot to visualize output within the PPBstats package
#'
#' @name plot.PPBstats
#' 
#' @description
#' \code{plot.PPBstats} returns ggplot to visualize outputs from several functions
#'
#' @param x Outputs from 
#' \itemize{
#'  \item \code{\link{format_data_PPBstats}}
#'  \item \code{\link{check_model}} 
#'  \item \code{\link{mean_comparisons}} 
#'  \item \code{\link{parameter_groups}} 
#'  \item \code{\link{biplot_data}} 
#'  \item \code{\link{cross_validation_model_bh_GxE}}
#' }
#' 
#' @param ... See details: parameters are specific to different classes
#' 
#' @details
#' For more details see:
#' \itemize{
#'  \item \code{\link{plot.biplot_GxE}}
#'  \item \code{\link{plot.biplot_hedonic}}
#'  \item \code{\link{plot.biplot_napping}}
#'  \item \code{\link{plot.check_model_bh_GxE}}
#'  \item \code{\link{plot.check_model_bh_intra_location}}
#'  \item \code{\link{plot.check_model_bh_variance_intra}}
#'  \item \code{\link{plot.check_model_hedonic}}
#'  \item \code{\link{plot.check_model_napping}}
#'  \item \code{\link{plot.check_model_spatial}}
#'  \item \code{\link{plot.cross_validation_model_bh_GxE}}
#'  \item \code{\link{plot.data_agro}}
#'  \item \code{\link{plot.data_network}}
#'  \item \code{\link{plot.data_organo_hedonic}}
#'  \item \code{\link{plot.data_organo_napping}}
#'  \item \code{\link{plot.mean_comparisons_model_bh_GxE}}
#'  \item \code{\link{plot.mean_comparisons_model_bh_intra_location}}
#'  \item \code{\link{plot.mean_comparisons_model_GxE}}
#'  \item \code{\link{plot.mean_comparisons_model_hedonic}}
#'  \item \code{\link{plot.mean_comparisons_model_spatial}}
#'  \item \code{\link{plot.mean_comparisons_predict_the_past_model_bh_GxE}}
#'  \item \code{\link{plot.parameter_groups}}
#' }
#'
#' @return 
#' It returns list of ggplot object
#'   
#' @author Pierre Riviere
#' 
NULL