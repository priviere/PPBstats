# 0. help ----------
#' Get ggplot objects to visualize output from the analysis
#'
#' @description
#' \code{get.ggplot} returns ggplot objects to visualize outputs from the analysis
#'
#' @param data The data to plot. It can come from \code{get.mean.comparison}, \code{get.parameter.groups}, \code{predict.the.past}, \code{MC}$data_env_with_no_controls or \code{analye.outputs$model1.data_env_whose_param_did_not_converge}.
#' 
#' @param data_2 Outputs from \code{get.mean.comparisons} from model 2.For ggplot.type = "biplot-alpha-beta"
#' 
#' @param data_version data set with the following columns: "year", "location", "germplasm", "group", "version". The group refers to an id that contains two different versions. For example for group 1, there is version 1 and 2. See data(data_version) for an example.
#'
#' @param ggplot.type The type of plot you wish:
#' \itemize{
#'  \item from \code{get.mean.comparison}
#'  \itemize{
#'    \item from model 1 (\code{MC}) :  "barplot", "interaction", "score"
#'    \item from model 2 (\code{FWH}): "barplot", "biplot-alpha-beta"
#'    }
#'      
#'  \item from \code{get.parameter.groups} only from model 2 (\code{FWH}): "PCA"
#'  
#'  \item from \code{predict.the.past}$predict.the.past : "barplot", "interaction"
#'  
#'  \item from \code{MC}$data_env_with_no_controls : "barplot", "interaction"
#'  
#'  \item from \code{analye.outputs$model1.data_env_whose_param_did_not_converge} : "barplot", "interaction"
#'  
#'  }
#' 
#' @param nb_parameters_per_plot The number of parameters per plot to facilitate the visualization
#' 
#' @details
#' \itemize{
#' \item From \code{get.mean.comparison} and from model 1 (\code{MC}) there are one plot per environment.
#' For ggplot.type = "interaction" and ggplot.type = "score", nb_parameters_per_plot is the number of entries (mu_ij).
#' This is meaningful if you have data for several years.
#' 
#' ggplot.type = "score" display a plot with a score according to which group the entry was allocated.
#' An high score means that the entry was in a group with an high mean.
#' A low score means that the entry was in a group with an low mean.

#' \item From \code{get.parameter.groups} from model 2 (\code{FWH}) "PCA" display the PCA and the groups of parameters.
#' 
#' On each plot, the alpha value and the alpha correction are displayed.
#' alpha = Imp means that no differences were possible to find.
#' For ggplot.type = "interaction" and ggplot.type = "score", it is display under the form: alpha | alpha correction
#' 
#' \item ggplot.type = "biplot-alpha-beta" display the biplot with alpha_i on the x axis and beta_i on the y axis.
#' 
#' \item When using data_version, and ggplot.type = "barplot"; the pvalue is computed based on the MCMC.
#' For data that did not converge or without environments, it is a \code{t.test} which is perform.
#' }
#' 
#' @return 
#' The function returns a list of ggplot objects
#'  
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{get.mean.comparisons}}, \code{\link{get.parameter.groups}}
#' 
#' 
get.ggplot = function(
  data,
  data_2 = NULL,
  data_version = NULL,
  ggplot.type = "interaction",
  nb_parameters_per_plot = 8
)
  # let's go !!! ----------
{
  # 1. error messages ----------
  mess = "data must come from functions check_model, mean_comparisons, parameter_groups"
  
  if (is.null(attributes(data)$PPBstats.object)) { stop(mess) }
  
  if (!is.element(attributes(data)$PPBstats.object, c("check_model_model_1", "check_model_model_2", "check_model_GxE", "mean_comparisons_GxE", "mean_comparisons_model_1", "mean_comparisons_model_2", "mean_comparisons_predict_the_past_model_2", "parameter_groups", "cross_validation_model_2"))) { stop(mess) }
  
  if( !is.element(ggplot.type, c("barplot", "biplot-alpha-beta", "interaction", "score"))) { stop("ggplot.type must be either \"barplot\", \"biplot-alpha-beta\", \"interaction\", \"score\".") }
  
  if( !is.null(data_version) ){
    mess = "The following column are compulsory in data_version : c(\"year\", \"germplasm\", \"location\", \"group\", \"version\"."
    if(!is.element("year", colnames(data_version))) { stop(mess) }
    if(!is.element("germplasm", colnames(data_version))) { stop(mess) }
    if(!is.element("location", colnames(data_version))) { stop(mess) }
    if(!is.element("group", colnames(data_version))) { stop(mess) }
    if(!is.element("version", colnames(data_version))) { stop(mess) }
    
    # delete version where there are not v1 AND v2
    vec_group = unique(data_version$group)
    for(gp in vec_group){
      d_tmp = droplevels(filter(data_version, group == gp))
      if(nlevels(d_tmp$version) != 2 ){ stop("There must be 2 levels per group in data_version. This is not the case for group ", gp) }
    }
  }
  
  # 2. Run functions ----------
  if( attributes(data)$PPBstats.object == "check_model_model_1" ) { out = ggplot_check_model_model_1(data, nb_parameters_per_plot) }

  if( attributes(data)$PPBstats.object == "check_model_model_2" ) { out = ggplot_check_model_model_2(data, nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "check_model_GxE" ) { out = ggplot_check_model_GxE(data, nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "mean_comparisons_GxE" ) { out = ggplot_mean_comparisons_model_GxE(data, nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "mean_comparisons_model_1" ) { out = ggplot_mean_comparisons_model_1(data, data_version, ggplot.type, nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "mean_comparisons_model_2" ) { out = ggplot_mean_comparisons_model_2(data, data_2, ggplot.type, nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "mean_comparisons_predict_the_past_model_2" ) { out = ggplot_mean_comparisons_predict_the_past_model_2(data, data_version, ggplot.type, nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "biplot_GxE" ) { out = ggplot_biplot_GxE(data) }
  
  if( attributes(data)$PPBstats.object == "parameter_groups" ) { out = ggplot_parameter_groups(data) }

  if( attributes(data)$PPBstats.object == "cross_validation_model_2" ) { out = ggplot_cross_validation_model_2(data) }
  
  return(out)
}

