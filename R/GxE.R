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
#' @param gxe_analysis "AMMI" or "GGE"
#' 
#' @details See the vignette for more details on the models
#' 
#' @return 
#' The function returns two lists with
#' \itemize{
#'  \item for each variable a list with
#'   \itemize{
#'    \item "model" being a list with 
#'     \itemize{
#'      \item "anova_model"
#'      \item "residuals" being a list with 
#'       \itemize{
#'        \item "distribution"
#'        \item "Residuals_vs_fitted"
#'        \item "QQplot"
#'        \item "standardized_residuals_vs_fitted"
#'       }
#'      \item "variability_repartition"
#'      \item "interaction_matrix"
#'      \item "location_effects"
#'      \item "germplasm_effects"
#'      \item "intra_germplasm_variance"
#'     }
#'    
#'    \item germplasm being a list with 
#'      \itemize{
#'       \item "boxplot"
#'       \item "barplot_LSD_significant_group"
#'       \item "variance_intra"
#'      }
#'    
#'    \item location being a list with 
#'     \itemize{
#'       \item "boxplot"
#'       \item "barplot_LSD_significant_group"
#'     }
#'     
#'    \item GxE being a list with 
#'     \itemize{
#'      \item "interaction_plot"
#'      \item "ecovalence"
#'      \item PCA a list with
#'      \itemize{
#'       \item "variation_dim"
#'       \item "which_won_where"
#'       \item "mean_vs_stability"
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
  vec_variables,
  gxe_analysis = NULL
)

# Lets' go ----------
  {
    
    # Error messages ----------
    check_data_vec_variables(data, vec_variables)
    if( is.null(gxe_analysis) ) { stop("You ust set gxe_analysis: AMMI or GGE") }
    if(!is.element(gxe_analysis, c("AMMI", "GGE"))) { stop("gxe_analysis must be either \"AMMI\" or \"GGE\".") }
    
    # 1. write the ammi function to apply to vec_variables ----------
  fun_gxe = function(variable, data, gxe_analysis) 
    # go! ----------
    {
    # 1.1. Set up data set ----------
      colnames(data)[which(colnames(data) == variable)] = "variable"
      data = data[c("location", "germplasm", "year", "block", "variable")]
      data = droplevels(na.omit(data))
    
    # 1.2. GxE model which depends on the years available in the data set ----------
    
    # 1.2.1. Run the model ----------
    data$block_in_env = factor(paste(data$block, data$location, sep = ";")) # hierarchise block within environemnt
    data$YxE = factor(paste(data$year, data$location, sep = ";"))
    data$YxG = factor(paste(data$year, data$germplasm, sep = ";"))
    
    # options(contrasts = c("contr.treatment", "contr.poly")) default options
    options(contrasts = c("contr.sum", "contr.sum")) # to get sum of parameters = 0
    
    if(nlevels(data$year) > 1) { 
      model = lm(variable ~ germplasm*location + block_in_env + year + YxG + YxE, data = data)
    } else {
      model = lm(variable ~ germplasm*location + block_in_env, data = data)
    }
    options(contrasts = c("contr.treatment", "contr.poly")) # Come back to default options

    anova_model = anova(model)
    
    # Test for homogeneity of variances
    #ft = fligner.test(variable ~ interaction(germplasm,location), data=data)
    #print(ft)

    # 1.2.2. Check residuals (qqplot, Skewness & Kurtosis tests) ----------
    outRes = gverifResidualsnormality(model)
    
    # 1.2.3. repartition of variability among factors
    pie = gpieplot(anova_model, variable)
    
    # 1.2.4. Get effects ----------
    outinter = build_interaction_matrix(model, data, gxe_analysis)
    data_interaction = outinter$data_interaction
    vec_E = outinter$vec_E
    vec_G = outinter$vec_G
    vec_var_G_intra = outinter$vec_var_G_intra

    out_model = list(
      "anova_model" = anova_model,
      "residuals" = list(
        "distribution" = outRes$plotRes0,
        "Residuals_vs_fitted" = outRes$plotRes1,
        "QQplot" = outRes$plotRes2,
        "standardized_residuals_vs_fitted" = outRes$plotRes3
      ),
      "variability_repartition" = pie,
      "interaction_matrix" = data_interaction,
      "location_effects" = vec_E,
      "germplasm_effects" = vec_G,
      "intra_germplasm_variance" = vec_var_G_intra
    )
    
    
    # 1.3. Study germplam effects ----------
    p1_G = gboxplot(data, "germplasm", variable) # germplasm boxplot
    p2_G = gLSDplot(model, "germplasm", variable, "bonferroni") # germplasm significant groups
    p3_G = gResidualplot(model, variable) # residuals fonction of the germplasm (i.e. intra germplasm variability)
    
    out_germplasm = list(
      "boxplot" = p1_G,
      "barplot_LSD_significant_group" = p2_G,
      "variance_intra" = p3_G
    )
    
    # 1.4. Study location effect ----------
    p1_E = gboxplot(data, "location", variable) # location boxplot
    p2_E = gLSDplot(model, "location", variable, "bonferroni") # location significant groups
    
    out_location = list(
      "boxplot" = p1_E,
      "barplot_LSD_significant_group" = p2_E
    )
    
    # 1.5. Study interaction effects ----------
    
    # 1.5.1. Interaction plot ----------
    p1_GxE = ggplot(data = data, aes(x = location, y = variable, colour = germplasm, group = germplasm))
    #  p1_GxE = p1_GxE + stat_summary(fun.y= mean, geom = "point")
    p1_GxE = p1_GxE + stat_summary(fun.y = mean, geom = "line", aes(linetype = germplasm), size = 1) # + scale_linetype_manual(values=rep(c("solid", "dotted"), 6))
    
    #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    #cbbPalette <- c("#000000", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
    
    #p1_GxE = p1_GxE + scale_color_manual(values=rep(cbbPalette, each = 2))
    
    #p1_GxE = p1_GxE + labs(title = paste("Graphique d'interaction pour la variable :" ,variable,"en fonction du germplasme et de l'environnement"))
    p1_GxE = p1_GxE + theme(axis.text.x=element_text(size=15,angle=90), plot.title = element_text(lineheight=.8, face="bold"))
    # p2_GxE + ggtitle("") + xlab("") + ylab("") + theme(legend.title=element_blank())
    
    if(nlevels(data$year) > 1) { 
      p1_GxE = p1_GxE + facet_grid(year ~ .)
    }
    
    # 1.5.2. PCA on interaction matrix ----------
    pca = PCA(data_interaction, scale.unit = TRUE, graph = FALSE)

    # 1.5.3. Ecovalence
    ecovalence = ecovalence(data_interaction,variable)
    
    out_GxE = list(
      "interaction_plot" = p1_GxE,
      "ecovalence" = ecovalence,
      "PCA" = list(
        "variation_dim" = fviz_eig(pca),
        "which_won_where" = ggplot_which_won_where(pca),
        "mean_vs_stability" = ggplot_mean_vs_stability(pca),
        "discrimitiveness_vs_representativeness" = ggplot_discrimitiveness_vs_representativeness(pca)
      )
    )
    
    # 1.6. Return results ----------
    out = list(
      "model" = out_model,
      "germplasm" = out_germplasm,
      "location" = out_location,
      "GxE" = out_GxE
    )
    
    message(gxe_analysis, " model done for ", variable)
    
    return(out)
  }
  
  # 2. write function post ammi analysis regrouping all ammi analysis from different variables ----------
  fun_post_gxe = function(out_gxe)
    # go! ----------
    {
  # 2.1. barplot_variation_repartition
    dtmp = data.frame()
    for(i in 1:length(out_ammi)){
      d = out_ammi[[i]]$model$variability_repartition$data
      d = cbind.data.frame(names(out_ammi)[i], d)
      dtmp = rbind.data.frame(dtmp, d)
    }
    colnames(dtmp)[1] = "var"
    p = ggplot(dtmp, aes(x = var, y = percentage_Sum_sq)) + geom_bar(stat = "identity", aes(fill = factor))
    p = p + ylab("pourcentage de variation") + theme(axis.text.x = element_text(angle = 90))
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    barplot_variation_repartition = p + scale_fill_manual(values = cbbPalette)
    
    # 2.2. PCA on effects
    
    # 2.2.1. Get data sets
    n_G = n_E = n_varG = NULL
    for(i in 1:length(out_ammi)){
      n_G = c(n_G, names(out_ammi[[i]]$model$germplasm_effects))
      n_E = c(n_E, names(out_ammi[[i]]$model$location_effects))
      n_varG = c(n_varG, names(out_ammi[[i]]$model$intra_germplasm_variance))
    }
    

    n_G = unique(n_G)
    n_E = unique(n_E)
    n_varG = unique(n_varG)
    
    df_G = matrix(NA, ncol = length(out_ammi), nrow = length(n_G))
    colnames(df_G) = names(out_ammi)
    rownames(df_G) = n_G
      
    df_E = matrix(NA, ncol = length(out_ammi), nrow = length(n_E))
    colnames(df_E) = names(out_ammi)
    rownames(df_E) = n_E
    
    df_varG = matrix(NA, ncol = length(out_ammi), nrow = length(n_varG))
    colnames(df_varG) = names(out_ammi)
    rownames(df_varG) = n_varG
    
    for(i in 1:length(out_ammi)){
      g = out_ammi[[i]]$model$germplasm_effects
      df_G[names(g),i] = g
      e = out_ammi[[i]]$model$location_effects
      df_E[names(e),i] = e
      vg = out_ammi[[i]]$model$intra_germplasm_variance
      df_varG[names(vg),i] = vg
    }
    
    # 2.2.2. Run PCA
    PCA_G = PCA(df_G, scale.unit = TRUE, graph = FALSE)
    out_PCA_G = dographPCA(PCA_G)

    PCA_E = PCA(df_E, scale.unit = TRUE, graph = FALSE)
    out_PCA_E = dographPCA(PCA_E)

    PCA_varG = PCA(df_varG, scale.unit = TRUE, graph = FALSE)
    out_PCA_varG = dographPCA(PCA_varG)
    
    PCA_G_effect = list(
      "variation_dim" = out_PCA_G$screeplot,
      "CP1-CP2" = list("ind" = out_PCA_G$ind1_2, "var" = out_PCA_G$var1_2),
      "CP2-CP3" = list("ind" = out_PCA_G$ind2_3, "var" = out_PCA_G$var2_3)
    )
    
    PCA_intraG_effect = list(
      "variation_dim" = out_PCA_varG$screeplot,
      "CP1-CP2" = list("ind" = out_PCA_varG$ind1_2, "var" = out_PCA_varG$var1_2),
      "CP2-CP3" = list("ind" = out_PCA_varG$ind2_3, "var" = out_PCA_varG$var2_3)
    )
    
    PCA_E_effect = list(
      "variation_dim" = out_PCA_E$screeplot,
      "CP1-CP2" = list("ind" = out_PCA_E$ind1_2, "var" = out_PCA_E$var1_2),
      "CP2-CP3" = list("ind" = out_PCA_E$ind2_3, "var" = out_PCA_E$var2_3)
    )
    
    out = list(
      "barplot_variation_repartition" = barplot_variation_repartition,
      "PCA_G_effect" = PCA_G_effect,
      "PCA_intraG_effect" = PCA_intraG_effect,
      "PCA_E_effect" = PCA_E_effect
    )

  }
    
  # 3. Apply AMMI and Post AMMI to vec_variables ----------
  message("I. Run ", gxe_analysis, " model on each variable")
  out_gxe = lapply(vec_variables, fun_gxe, data, gxe_analysis)
  names(out_gxe) = vec_variables
  
  message("\nII. Post ", gxe_analysis," analysis on all outputs")
  #out_post_gxe = fun_post_gxe(out_gxe)
  out_post_gxe = NULL
  
  OUT = list(out_gxe, out_post_gxe)
  names(OUT) = c(gxe_analysis, paste("Post_", gxe_analysis, sep = ""))
  
  return(OUT)
}
