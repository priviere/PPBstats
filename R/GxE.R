# 0. help ----------
#' Run GxE model on a data set
#'
#' @description
#' \code{GxE} runs AMMI or GGE model on a data set and gives back results on germplasms, locations, years and GxE or G+GxE interactions effects
#' 
#' @param data data with at least the following columns: year, location, germplasm, block, X, Y
#' 
#' @param vec_variables vectors of variables to analyse
#' 
#' @param gxe_analysis the analysis to carry out: "AMMI" or "GGE"
#' 
#' @details See the vignette for more details on the models
#' 
#' @return 
#' The function returns two lists with
#' \itemize{
#'  \item for each variable a list with
#'   \itemize{
#'    \item "descriptive" being a list with
#'    \itemize{
#'     \item germplasm containing a boxplot for each germplasm
#'     \item location containing a boxplot for each location
#'     \item interaction containing an interaction plot
#'     }
#'    
#'    \item "ANOVA" being a list with 
#'     \itemize{
#'      \item "anova_model"
#'      \item "residuals" being a list with 
#'       \itemize{
#'        \item "distribution"
#'        \item "QQplot"
#'       }
#'      \item "variability_repartition" a pie chart with variability repartition
#'      \item "location_effects" being a list with
#'      \itemize{
#'       \item a vector with location effects
#'       \item a barplot with groups based on LSD interval
#'      }
#'      \item "germplasm_effects"
#'      \itemize{
#'       \item a vector with germplasm effects
#'       \item a vector with intra germplasm variance estimation
#'       \item a barplot with groups based on LSD interval
#'       \item a boxplot with intra germplasm variance estimation per germplasm
#'      }
#'      \item "interaction_matrix" the interaction matrix on which is based the PCA
#'     }
#'     
#'    \item PCA being a list with 
#'     \itemize{
#'      \item "ecovalence"
#'      \item "biplot" a list with
#'      \itemize{
#'       \item "variation_dim"
#'       \item "which_won_where"
#'       \item "mean_vs_stability" a list with two elements: "mean" and "stability"
#'       \item "discrimitiveness_vs_representativeness"
#'      }
#'    }
#'  }
#'   
#'  \item a post GxE analysis with
#'   \itemize{
#'    \item "barplot_variation_repartition"
#'    \item "PCA_G_effect"
#'    \item "PCA_intraG_effect"
#'    \item "PCA_E_effect"
#'   }
#'  }
#' 
#' @author 
#' Pierre Riviere
#' 
GxE = function(
  data, 
  variable,
  gxe_analysis = NULL
)

# Lets' go ----------
  {
    # 1. Error messages ----------
    check_data_vec_variables(data, variable)
    if( is.null(gxe_analysis) ) { stop("You ust set gxe_analysis: AMMI or GGE") }
    if(!is.element(gxe_analysis, c("AMMI", "GGE"))) { stop("gxe_analysis must be either \"AMMI\" or \"GGE\".") }
    
    # 2. Set up data set ----------
      colnames(data)[which(colnames(data) == variable)] = "variable"
      data = data[c("location", "germplasm", "year", "block", "variable")]
      data = droplevels(na.omit(data))
      
      data$block_in_env = factor(paste(data$block, data$location, data$year, sep = ";")) # hierarchise block within location and year
      data$YxE = factor(paste(data$year, data$location, sep = ":"))
      data$YxG = factor(paste(data$year, data$germplasm, sep = ":"))
      
    
    # 3. ANOVA ----------
    
    # options(contrasts = c("contr.treatment", "contr.poly")) default options
    options(contrasts = c("contr.sum", "contr.sum")) # to get sum of parameters = 0
    
    if(nlevels(data$year) > 1) { # depends on the years available in the data set
      model = lm(variable ~ germplasm*location + block_in_env + year + YxG + YxE, data = data)
    } else {
      model = lm(variable ~ germplasm*location + block_in_env, data = data)
    }
    options(contrasts = c("contr.treatment", "contr.poly")) # Come back to default options

    anova_model = anova(model)
    
    # 3.1. Get effects ----------
    
    c = model$coefficients
    
    # 3.2. germplam effects ----------
    coef_germ = c[grep("germplasm", names(c))]
    todel = grep(":", names(coef_germ))
    if(length(todel) > 0) { coef_germ = coef_germ[-todel] }
    coef_germ = c(coef_germ, - sum(coef_germ))
    names(coef_germ) = model$xlevels$germplasm
    
    out_germplasm = list(
      "effects" = coef_germ,
      "intra_variance" = var_intra
      )
    
    # 3.3. location effect ----------
    coef_env = c[grep("location", names(c))]
    todel = grep(":", names(coef_env))
    if(length(todel) > 0) { coef_env = coef_env[-todel] }
    coef_env = c(coef_env, - sum(coef_env))
    names(coef_env) = model$xlevels$location
    
    out_location = list(
      "effects" = coef_env,
      )
    
    # 3.3 interaction matrix ----------
    data_interaction = GxE_build_interaction_matrix(data, gxe_analysis)
    
    # 3.4. Return ANOVA results ----------
    out_anova = list(
      "anova_model" = anova_model,
      "germplasm_effects" = out_germplasm,
      "location_effects" = out_location,
      "interaction_matrix" = data_interaction
    )
    
    
    # 4. PCA on interaction matrix ----------
    out_pca = PCA(data_interaction, scale.unit = TRUE, graph = FALSE)

    # 5. Return results ----------
    out = list(
      "ANOVA" = out_anova,
      "PCA" = out_pca
    )
    
    message(gxe_analysis, " model done for ", variable)
    
    return(out)
  }
