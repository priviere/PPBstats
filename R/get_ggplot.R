# 0. help ----------
#' Get ggplot objects to visualize output from several functions
#'
#' @description
#' \code{get_ggplot} returns ggplot objects to visualize outputs from several functions
#'
#' @param data Outputs from 
#' \itemize{
#'  \item \code{check_model} or
#'  \item \code{mean_comparisons} or
#'  \item \code{parameter_groups} or
#'  \item \code{biplot_GxE} or
#'  \item \code{cross_validation_model_2}
#' }
#' 
#' @param data_2 Outputs from \code{mean_comparisons} from \code{check_model} from \code{model_2}.
#' This argument is used only when argument data comes from \code{mean_comparisons} from \code{check_model} from \code{model_2}.
#' 
#' @param data_version data set with the following columns: "year", "germplasm", "location", "group", "version". The group refers to an id that contains two different versions. For example for group 1, there is version 1 and 2. See data(data_version) for an example.
#' This argument is used only for \code{mean_comparisons} from 
#' \itemize{
#'  \item \code{check_model} from \code{model_1}
#'  \item \code{check_model} from \code{predict_the_past_model_2}
#' }
#'
#' @param ggplot.type The type of plot you wish.
#' \itemize{
#'  \item "barplot" for mean_comparisons from 
#'  \itemize{
#'   \item check_model from GxE, model_1, model_2
#'   \item predict_the_past_model_2
#'   }
#'  \item "interaction.plot" for mean_comparisons from
#'  \itemize{
#'   \item check_model from model_1
#'   \item predict_the_past_model_2
#'   }
#'  \item "score" for mean_comparisons from
#'  \itemize{
#'   \item check_model from model_1
#'   \item predict_the_past_model_2
#'   }
#'  \item "biplot-alpha-beta" for mean_comparisons from check_model from model_2
#'  }
#' 
#' There are automatic plots generated for several functions:
#' \itemize{
#'  \item \code{biplot_GxE}
#'  \item \code{check_model}
#'  \item \code{parameter_groups}
#'  \item \code{cross_validation_model_2}
#'  }
#' See details for more information.
#' 
#' @param nb_parameters_per_plot The number of parameters per plot to facilitate the visualization
#' 
#' @details
#' to do
#' 
#' @return 
#' \itemize{
#' 
#' 
#'  \item check_model
#'   \itemize{
#'   
#'    \item from GxE :
#'     \itemize{
#'      \item residuals
#'       \itemize{
#'        \item histogramm
#'        \item  qqplot
#'       }
#'      \item variability_repartition
#'      \item variance_intra_germplasm
#'      \item pca_composante_variance
#'     }
#'     
#'    \item from model_1 :
#'     \itemize{
#'      \item sigma_j_gamma
#'      \item mu_ij
#'      \item beta_jk
#'      \item sigma_j
#'      \item epsilon_ijk
#'     }
#'    
#'    \item from model_2 :
#'     \itemize{
#'      \item alpha_i
#'      \item beta_i
#'      \item theta_j
#'      \item epsilon_ij
#'     }
#'    
#'   }
#' 
#' 
#'  \item mean_comparisons
#'   \itemize{
#'    \item from GxE
#'     \itemize{
#'      \item germplasm
#'      \item location
#'      \item year
#'     }
#'    
#'    \item from model_1, a list with ggplot object
#'    
#'    \item from model_2, a list with ggplot object
#'    
#'    \item predict_the_past_model_2, a list with ggplot object
#'     
#'   }
#'  
#'  
#'  \item parameter_groups
#'   \itemize{
#'    \item pca
#'     \itemize{
#'      \item variation_dim
#'      \item ind
#'      \item var
#'     }
#'    \item clust
#'     \itemize{
#'      \item nb_k
#'      \item pca
#'     }
#'   }
#'   
#'   
#'  \item biplot_GxE
#'   \itemize{
#'    \item ecovalence
#'    \item biplot
#'     \itemize{
#'      \item which_won_where
#'      \item mean_vs_stability
#'      \item discrimitiveness_vs_representativeness
#'     }
#'   }
#'   
#'   
#'  \item cross_validation_model_2
#'   \itemize{
#'    \item plot
#'    \item anova
#'   }
#'   
#' }
#'  
#' @author Pierre Riviere
#' 
#' @seealso
#' \itemize{
#'  \item \code{check_model}
#'  \item \code{mean_comparisons}
#'  \item \code{parameter_groups}
#'  \item \code{biplot_GxE}
#'  \item \code{cross_validation_model_2}
#' }
#' 
get_ggplot = function(
  data,
  data_2 = NULL,
  data_version = NULL,
  ggplot.type = "interaction",
  nb_parameters_per_plot = 8
)
  # let's go !!! ----------
{
  # 1. error messages ----------
  mess = "data must come from functions check_model, mean_comparisons, parameter_groups, biplot_GxE, cross_validation_model_2"
  
  if (is.null(attributes(data)$PPBstats.object)) { stop(mess) }
  
  if (!is.element(attributes(data)$PPBstats.object, c("check_model_model_1", "check_model_model_2", "check_model_GxE", "mean_comparisons_GxE", "mean_comparisons_model_1", "mean_comparisons_model_2", "mean_comparisons_predict_the_past_model_2", "biplot_GxE", "parameter_groups", "cross_validation_model_2"))) { stop(mess) }
  
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
  
  if( attributes(data)$PPBstats.object == "mean_comparisons_GxE" ) { out = ggplot_mean_comparisons_GxE(data, ggplot.type, nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "mean_comparisons_model_1" ) { out = ggplot_mean_comparisons_model_1(data, data_version, ggplot.type, nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "mean_comparisons_model_2" ) { out = ggplot_mean_comparisons_model_2(data, data_2, ggplot.type, nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "mean_comparisons_predict_the_past_model_2" ) { out = ggplot_mean_comparisons_predict_the_past_model_2(data, data_version, ggplot.type, nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "biplot_GxE" ) { out = ggplot_biplot_GxE(data) }
  
  if( attributes(data)$PPBstats.object == "parameter_groups" ) { out = ggplot_parameter_groups(data) }

  if( attributes(data)$PPBstats.object == "cross_validation_model_2" ) { out = ggplot_cross_validation_model_2(data) }
  
  return(out)
}

