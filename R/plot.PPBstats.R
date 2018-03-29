#' Get ggplot to visualize output
#'
#' @description
#' \code{plot.PPBstats} returns ggplot to visualize outputs from several functions
#'
#' @param data Outputs from 
#' \itemize{
#'  \item \code{\link{check_model}} or
#'  \item \code{\link{mean_comparisons}} or
#'  \item \code{\link{parameter_groups}} or
#'  \item \code{\link{biplot_GxE}} or
#'  \item \code{\link{cross_validation_model_bh_GxE}}
#' }
#' 
#' @param data_2 Outputs from \code{\link{mean_comparisons}} from \code{\link{check_model}} from \code{\link{model_bh_GxE}}.
#' This argument is used only when argument data comes from \code{\link{mean_comparisons}} from \code{\link{check_model}} from \code{\link{model_bh_GxE}}.
#' 
#' @param data_version data set with the following columns: "year", "germplasm", "location", "group", "version". The group refers to an id that contains two different versions. For example for group 1, there is version 1 and 2. See data(data_version) for an example.
#' This argument is used only for \code{\link{mean_comparisons}} from 
#' \itemize{
#'  \item \code{\link{check_model}} from \code{\link{model_bh_intra-location}}
#'  \item \code{\link{check_model}} from \code{\link{predict_the_past_model_bh_GxE}}
#' }
#'
#' @param plot_type The type of plot you wish.
#' \itemize{
#'  \item "barplot" for \code{\link{mean_comparisons}} from 
#'  \itemize{
#'   \item \code{\link{check_model}} from \code{\link{model_GxE}}, \code{\link{model_bh_intra-location}}, \code{\link{model_bh_GxE}}
#'   \item \code{\link{predict_the_past_model_bh_GxE}}
#'   }
#'  \item "interaction.plot" for \code{\link{mean_comparisons}} from
#'  \itemize{
#'   \item \code{\link{check_model}} from \code{\link{model_bh_intra-location}}
#'   \item \code{\link{predict_the_past_model_bh_GxE}}
#'   }
#'  \item "score" for \code{\link{mean_comparisons}} from
#'  \itemize{
#'   \item \code{\link{check_model}} from \code{\link{model_bh_intra-location}}
#'   \item \code{\link{predict_the_past_model_bh_GxE}}
#'   }
#'  \item "biplot-alpha-beta" for \code{\link{mean_comparisons}} from \code{\link{check_model}} from \code{\link{model_bh_GxE}}
#'  }
#' 
#' There are automatic plots generated for several functions:
#' \itemize{
#'  \item \code{\link{biplot_GxE}}
#'  \item \code{\link{check_model}}
#'  \item \code{\link{parameter_groups}}
#'  \item \code{\link{cross_validation_model_bh_GxE}}
#'  }
#' See details for more information.
#' 
#' @param nb_parameters_per_plot The number of parameters per plot to facilitate the visualization
#' 
#' @param ind_to_highlight For parmater_group, individuals to highlight
#' 
#' @details
#' Below are some details on the construction of the plots :
#' 
#' \itemize{
#'  \item \code{\link{check_model}} from \code{\link{model_GxE}} :
#'   \itemize{
#'    \item variance_intra_germplasm represents the repartition of the residuals for each germplasm.
#'    This has to been seen with caution:
#'    \itemize{
#'     \item If germplasm have no intra-germplasm variance (i.e. pure line or hybrides) then the distribution of each germplasm represent only the micro-environmental variation.
#'     \item If germplasm have intra-germplasm variance (i.e. population such as landraces for example) then the distribution of each germplasm represent the micro-environmental variation plus the intra-germplasm variance.
#'     With the hypothesis than the micro-environmental variation is equaly distributed on all the individuals (i.e. all the plants), the distribution of each germplasm represent the intra-germplasm variance.
#'    }
#'  }
#'  
#'  \item \code{\link{check_model}} from \code{\link{model_bh_intra-location}} and \code{\link{model_bh_GxE}}
#'  \itemize{
#'   \item mcmc_not_converge_traceplot_density : If you wish exhaustive information, look at \code{ggmcmc::ggmcmc} with \code{ggmcmc(out_model$MCMC)}. 
#'   But be careful with the size of your MCMC output which are often too big to be performed in R.
#'  }
#'  
#'  \item \code{\link{biplot_GxE}}
#'   \itemize{
#'    \item which_won_where : plot to assess which germplasm win in which location.
#'    A polygon is drawn connecting germplasm which are located furthest away from the biplot origin such that all other entries are contained within the polygon;
#'    Perpendicular lines are drawn to each side of the polygon and start from biplot origin.
#'    A sector is the triangle area formed by two perpendicular lines.
#'    The germplasms which have the largest value in a sector "win" in the location present in that sector.
#'    The information is summarized in the legend of the plot.
#'    
#'    \item mean_vs_stability.
#'    It is a list of two elements where the plots are drawn under the same construction.
#'    A red circle indicates the position of the average location, which is defined by the average Dim1 and Dim2 scores across all locations.
#'    An arrow points this average 'virtual' location.
#'    A red line passes through the biplot origin and the average location (known as the Average-Tester Axis (ATA)).
#'    A red line is set perpendicular the ATA.
#'    Dashed perpendicular lines to the ATA are drawn and passed through each germplasm.
#'    
#'    \itemize{
#'     \item mean_performance : assess mean performance score of each germplasm. 
#'     The mean performance score is the rank of germplasm along the ATA.
#'     The arrow points the greatest value according to their mean performance across all locations.
#'     An high score, represents an high mean performance.
#'     A low score reresents an low mean performance.
#'     Score are displayed into the legend.
#'      
#'     \item stability_performance : assess stability performance score of each germplasm.
#'     The stability performance score of the germplasms are represented by the projection from the germplasm to the ATA.
#'     An high score, represents an high interaction and therfore lower stability.
#'     A low score reresents an low interaction and therefore an high stability.
#'     Score are displayed into the legend.
#'    }
#'    
#'    \item discrimitiveness_vs_representativeness.
#'    It is a list of two elements where the plots are drawn under the same construction.
#'    A red circle indicates the position of the average location, which is defined by the average Dim1 and Dim2 scores across all locations.
#'    An arrow points this average 'virtual' location.
#'    A red line passes through the biplot origin and the average location (known as the Average-Tester Axis (ATA)).
#'    
#'    
#'     \itemize{
#'      \item discrimitiveness : assess discrimitiveness of each location.
#'      The score represents the discrimination : the higher the vector, the more discriminating the location.
#'      
#'      \item representativeness : assess representativeness of each location.
#'      The score represents the representativeness : the higher the score, the less representative the location.
#'      
#'      \item discrimitiveness_vs_representativeness :  represent discrimitiveness versus representativeness.
#'      The location combining better score (i. e.discrimination and representativeness) are the ones that could be used to test germplasms as they are more representative of all the locations.
#'      This has to be done severals year to get robust results.
#'      The highest the score, the more disciminative and representative the location.
#'     } 
#'    }
#'   
#'  \item \code{\link{cross_validation_model_bh_GxE}}
#'   \itemize{
#'    \item plot : plot estimated.value = f(observed.value). 
#'    The probability mean = 0 is coming from a \code{t.test} performed with the null hypothesis H0: the bias of estimated values in relation to real values = 0.
#'   }
#'   
#' }
#'  
#' 
#' @return 
#' \itemize{
#' 
#' 
#'  \item \code{\link{check_model}}
#'   \itemize{
#'   
#'    \item from \code{\link{model_GxE}} :
#'     \itemize{
#'      \item residuals
#'       \itemize{
#'        \item histogram : histogram with the distribution of the residuals
#'        \item qqplot
#'       }
#'      \item variability_repartition : pie with repartition of SumSq for each factor
#'      \item variance_intra_germplasm : repartition of the residuals for each germplasm (see Details for more information)
#'      \item pca_composante_variance : variance caught by each dimension of the PCA run on the interaction matrix
#'     }
#'     
#'    \item from \code{\link{model_spatial}} :
#'     \itemize{
#'      \item residuals
#'       \itemize{
#'        \item histogram : histogram with the distribution of the residuals
#'        \item qqplot
#'       }
#'      \item variability_repartition : pie with repartition of variance for each factor
#'     }
#'     
#'    \item from \code{\link{model_bh_intra-location}} :
#'     \itemize{
#'      \item sigma_j_gamma : mean of each sigma_j displayed on the inverse Gamma distribution. 
#'      The first graph represent all the sigma_j, the other graph represent \code{nb_parameters_per_plot} sigma_j per graph.
#'      \item mu_ij : distribution of each mu_ij in a list with as many elements as environment. 
#'      For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} mu_ij per graph.
#'      \item beta_jk : distribution of each beta_jk in a list with as many elements as environment. 
#'      For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} beta_jk per graph.
#'      \item sigma_j : distribution of each sigma_j. 
#'      There are as many graph as needed with \code{nb_parameters_per_plot} sigma_j per graph.
#'      \item epsilon_ijk : standardised residuals distribution.
#'      If the model went well it should be between -2 and 2.
#'      \item mcmc_not_converge_traceplot_density : a list with the plots of trace and density to check the convergence of the two MCMC only for chains that are not converging thanks to the Gelman-Rubin test. 
#'      If all the chains converge, it is NULL.
#'     }
#'    
#'    \item from \code{\link{model_bh_GxE}} :
#'     \itemize{
#'      \item alpha_i : distribution of each alpha_i. 
#'      There are as many graph as needed with \code{nb_parameters_per_plot} alpha_i per graph.
#'      \item beta_i : distribution of each beta_i. 
#'      There are as many graph as needed with \code{nb_parameters_per_plot} beta_i per graph.
#'      \item theta_j : distribution of each theta_j. 
#'      There are as many graph as needed with \code{nb_parameters_per_plot} theta_j per graph.
#'      \item epsilon_ij : standardised residuals distribution.  
#'      If the model went well it should be between -2 and 2.
#'      \item mcmc_not_converge_traceplot_density : a list with the plots of trace and density to check the convergence of the two MCMC only for chains that are not converging thanks to the Gelman-Rubin test. 
#'      If all the chains converge, it is NULL.
#'     }
#'   }
#' 
#' 
#'  \item \code{\link{mean_comparisons}}
#'   \itemize{
#'   
#'    \item from \code{\link{model_GxE}}, a list with barplot.
#'    For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#'    Letters are displayed on each bar. Parameters that do not share the same letters are different regarding type I error (alpha) and alpha correction. 
#'    The error I (alpha) and the alpha correction are displayed in the title. 
#'     \itemize{
#'      \item germplasm : mean comparison for germplasm
#'      \item location : mean comparison for location
#'      \item year : mean comparison for year
#'     }
#'    
#'    \item from \code{\link{model_spatial}}, a list with barplot.
#'    For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#'    Letters are displayed on each bar. Parameters that do not share the same letters are different regarding type I error (alpha) and alpha correction. 
#'    The error I (alpha) and the alpha correction are displayed in the title. 
#'     \itemize{
#'      \item germplasm : mean comparison for germplasm
#'     }
#'    
#'    \item from \code{\link{model_bh_intra-location}}, a list with ggplot object depending on plot_type.
#'    For each plot_type, it is a list of three elements being lists with as many elements as environment. 
#'        For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#'     \itemize{
#'      \item barplot : 
#'       \itemize{
#'        \item data_mean_comparisons : only environments where all MCMC converge are represented.
#'        Letters are displayed on each bar. Parameters that do not share the same letters are different regarding type I error (alpha) and alpha correction. 
#'        The error I (alpha) and the alpha correction are displayed in the title. 
#'        alpha = Imp means that no differences were possible to find.
#'        
#'        \item data_env_with_no_controls : only environments where there were no controls are represented.
#'        \item data_env_whose_param_did_not_converge : only environments where MCMC did not converge are represented.
#'       }
#'       Note that when using data_version, the pvalue is computed based on the MCMC.
#'       For data that did not converge or without environments, it is a \code{t.test} which is perform.
#'       
#'      \item interaction : 
#'       \itemize{
#'        \item data_mean_comparisons : only environments where all MCMC converge are represented.
#'        The error I (alpha) and the alpha correction are displayed in the x.axis under the form "alpha | alpha correction".
#'        alpha = Imp means that no differences were possible to find.
#'        \item data_env_with_no_controls : only environments where there were no controls are represented.
#'        \item data_env_whose_param_did_not_converge : only environments where MCMC did not converge are represented.
#'       }
#'      \item score : The score is set according to which group the entry was allocated.
#'        An high score means that the entry was in a group with an high mean.
#'        A low score means that the entry was in a group with an low mean.
#'        In the legend, the score goes from 1 (first group) to the number of groups of significativity.
#'        The error I (alpha) and the alpha correction are displayed in the x.axis under the form "alpha | alpha correction".
#'        alpha = Imp means that no differences were possible to find.
#'     }
#'    
#'    
#'    \item from \code{\link{model_bh_GxE}}, a list with ggplot object depending on plot_type.
#'    There are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#'     \itemize{
#'      \item barplot : 
#'        Letters are displayed on each bar. 
#'        Parameters that do not share the same letters are different regarding type I error (alpha) and alpha correction. 
#'        The error I (alpha) and the alpha correction are displayed in the title. 
#'        alpha = Imp means that no differences were possible to find.
#'      \item biplot-alpha-beta : display the biplot with alpha_i on the x axis and beta_i on the y axis.
#'     }
#'    
#'    
#'    \item \code{\link{predict_the_past_model_bh_GxE}}, a list with ggplot object depending on plot_type. 
#'    It is the same outputs than for model_bh_intra-location
#'     
#'   }
#'  
#'  
#'  \item \code{\link{parameter_groups}}
#'   \itemize{
#'    \item pca : a list with three elements on the PCA on the group of parameters :
#'     \itemize{
#'      \item composante_variance : variance caught by each dimension of the PCA
#'      \item ind : graph of individuals
#'      \item var : graph of variables
#'     }
#'    \item clust : output from \code{factextra::fviz_nbclust()}. See \code{?factoextra::fviz_nbclust} for more details, 
#'    a list of number of cluster + 1 element
#'   }
#'   
#'   
#'  \item \code{\link{biplot_GxE}}
#'   \itemize{
#'    \item ecovalence
#'    \item interaction which display the interaction matrix
#'    \item biplot, a list of four elements :
#'     \itemize{
#'      \item simple_biplot : the biplot of location and germplasm
#'      \item which_won_where : plot to assess which germplasm win in which location (see Details for more information). Done only if gxe_analysis is GGE in \code{\link{model_GxE}}.
#'      \item mean_vs_stability : a list of two elements. Done only if gxe_analysis is GGE in \code{\link{model_GxE}}.
#'       \itemize{
#'        \item mean : assess mean of each germplasm (see Details for more information).
#'        \item stability : assess stability of each germplasm (see Details for more information).
#'       }
#'      \item discrimitiveness_vs_representativeness : a list of two elements. Done only if gxe_analysis is GGE in \code{\link{model_GxE}}.
#'       \itemize{
#'        \item discrimitiveness : assess discrimitiveness of each location (see Details for more information).
#'        \item representativeness : assess representativeness of each location (see Details for more information).
#'       }
#'      \item discrimitiveness_vs_representativeness :represents discrimitiveness versus representativeness (see Details for more information). 
#'     }
#'   }
#'   
#'   
#'  \item \code{\link{cross_validation_model_bh_GxE}}
#'   \itemize{
#'    \item plot : plot estimated.value = f(observed.value). 
#'    The probability mean = 0 is display (see Details for more information).
#'    \item regression : output of the model observed.value = a x estimated.value + b
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
#'  \item \code{cross_validation_model_bh_GxE}
#' }
#' 
plot.PPBstats <- function(
  x, y = NULL,
  data_version = NULL,
  plot_type = c("interaction", "barplot", "biplot-alpha-beta", "score", 
                "network", "map", "pam", "histogramm", "boxplot","biplot", "radar", "raster"),
  ...
) {
  ## common tasks
  plot_type <- match.arg(plot_type, several.ok = FALSE)
  
  if( !is.null(data_version) ){
    mandatory_dvcols <- c("year", "germplasm", "location", "group", "version")

    ## check compulsory columns
    if (!all(mandatory_dvcols %in% colnames(data_version))) {
      stop(
        paste("The following columns are compulsory in data_version:",
              paste(mandatory_dvcols, collapse = ", "))
      )
    }
    
    ## check factor
    if (!all(idx <- vapply(data_version[, mandatory_dvcols], is.factor, TRUE))) {
      stop(
        paste("The following column(s) must be factor in data_version:",
              paste(mandatory_dvcols[!idx], collapse = ", "))
      )
    }
    
    vec_group = unique(data_version$group)

  }
  
  ## perform model-specific tasks
  out <- try(NextMethod("plot"), silent = TRUE)
  
  ## Method not found
  if (inherits(out, "try-error")) {
    if (grepl("NextMethod", out)) {
      ## Method not found
      ## This message needs updating whenever new models are included in PPBstats
      ## TODO: have a list of supported models somewhere, and use it to compose
      ## the error message automatically
      mess = paste(substitute(x),
                   "must come from functions PPBstats::check_model(),",
                   "PPBstats::mean_comparisons(), PPBstats::parameter_groups(),", 
                   "PPBstats::biplot_GxE() or PPBstats::cross_validation_model_bh_GxE()")
    } else {
      ## some error within the more specific plot method
      mess = out
    }
    stop(mess)
  }
  
  return(out)
}
