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
#'        \item "Residuals_vs_fitted"
#'        \item "QQplot"
#'        \item "standardized_residuals_vs_fitted"
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
      
      data$block_in_env = factor(paste(data$block, data$location, sep = ";")) # hierarchise block within environemnt
      data$YxE = factor(paste(data$year, data$location, sep = ":"))
      data$YxG = factor(paste(data$year, data$germplasm, sep = ":"))
      
      # 2.Descriptive analysis ----------
      
      # 2.1. germplam ----------
      out_descriptive_germplasm = list("boxplot" = gboxplot(data, "germplasm", variable))
      
      # 2.2. location ----------
      out_descriptive_location = list("boxplot" = gboxplot(data, "location", variable))
      
      # 2.3. interaction ----------
      
      p_gxe = ggplot(data = data, aes(x = location, y = variable, colour = germplasm, group = germplasm))
      #  p_gxe = p_gxe + stat_summary(fun.y= mean, geom = "point")
      p_gxe = p_gxe + stat_summary(fun.y = mean, geom = "line", aes(linetype = germplasm), size = 1) # + scale_linetype_manual(values=rep(c("solid", "dotted"), 6))
      
      #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      #cbbPalette <- c("#000000", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
      
      #p_gxe = p_gxe + scale_color_manual(values=rep(cbbPalette, each = 2))
      
      #p_gxe = p_gxe + labs(title = paste("Graphique d'interaction pour la variable :" ,variable,"en fonction du germplasme et de l'environnement"))
      p_gxe = p_gxe + theme(axis.text.x=element_text(size=15,angle=90), plot.title = element_text(lineheight=.8, face="bold"))
      # p2_GxE + ggtitle("") + xlab("") + ylab("") + theme(legend.title=element_blank())
      
      if(nlevels(data$year) > 1) { 
        p_gxe = p_gxe + facet_grid(year ~ .)
      }
      
      out_descriptive_interaction = list("interaction-plot" = p_gxe)
      
      out_descriptive = list(
        "germplasm" = out_descriptive_germplasm, 
        "location" = out_descriptive_location, 
        "interaction" = out_descriptive_interaction
        )
    
    # 3. GxE model: ANOVA + PCA ----------
    
    # 3.1. ANOVA ----------
    
    # options(contrasts = c("contr.treatment", "contr.poly")) default options
    options(contrasts = c("contr.sum", "contr.sum")) # to get sum of parameters = 0
    
    if(nlevels(data$year) > 1) { # depends on the years available in the data set
      model = lm(variable ~ germplasm*location + block_in_env + year + YxG + YxE, data = data)
    } else {
      model = lm(variable ~ germplasm*location + block_in_env, data = data)
    }
    options(contrasts = c("contr.treatment", "contr.poly")) # Come back to default options

    anova_model = anova(model)
    
    # Test for homogeneity of variances
    #ft = fligner.test(variable ~ interaction(germplasm,location), data=data)
    #print(ft)

    # 3.1.1. Check residuals (qqplot, Skewness & Kurtosis tests) ----------
    outRes = gverifResidualsnormality(model)
    
    # 3.1.2. repartition of variability among factors ----------
    total_Sum_Sq = sum(anova_model$"Sum Sq")
    Sum_sq = anova_model$"Sum Sq"
    pvalue = anova_model$"Pr(>F)"
    percentage_Sum_sq = Sum_sq/total_Sum_Sq*100
    factor = rownames(anova_model)
      
    d_pie = cbind.data.frame(factor, pvalue, Sum_sq, percentage_Sum_sq)
      
    p_pie = ggplot(data = d_pie, aes(x = "", y = percentage_Sum_sq, fill = factor)) 
    p_pie = p_pie + ggtitle(paste("Distribution de la variance totale pour \n", variable))
    p_pie = p_pie + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
    #pie = pie + geom_text(data=DFtemp, aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = paste("  ",round(valuep*100), "%")))
    p_pie = p_pie + ylab("") + xlab("")
    
    # 3.1.3. Get effects ----------
    data_interaction = GxE_build_interaction_matrix(data, gxe_analysis)
    
    c = model$coefficients
    
    # 3.1.5. germplam effects ----------
    
    # germplasm
    coef_germ = c[grep("germplasm", names(c))]
    todel = grep(":", names(coef_germ))
    if(length(todel) > 0) { coef_germ = coef_germ[-todel] }
    coef_germ = c(coef_germ, - sum(coef_germ))
    names(coef_germ) = model$xlevels$germplasm
    
    # variance intra germplasm
    var_intra = tapply(model$residuals, model$model$germplasm, var, na.rm = TRUE)
    
    out_germplasm = list(
      "effects" = coef_germ,
      "intra_variance" = var_intra,
      "barplot_LSD_significant_group" = gLSDplot(model, "germplasm", variable, "bonferroni"),
      "boxplot_variance_intra" = gResidualplot(model, variable)
    )
    
    # 3.1.6. location effect ----------
    # location
    coef_env = c[grep("location", names(c))]
    todel = grep(":", names(coef_env))
    if(length(todel) > 0) { coef_env = coef_env[-todel] }
    coef_env = c(coef_env, - sum(coef_env))
    names(coef_env) = model$xlevels$location
    
    out_location = list(
      "effects" = coef_env,
      "barplot_LSD_significant_group" = gLSDplot(model, "location", variable, "bonferroni")
      )
    
    # 3.1.7. Return ANOVA results ----------
    out_anova = list(
      "anova_model" = anova_model,
      "residuals" = list(
        "distribution" = outRes$plotRes0,
        "Residuals_vs_fitted" = outRes$plotRes1,
        "QQplot" = outRes$plotRes2,
        "standardized_residuals_vs_fitted" = outRes$plotRes3
      ),
      "variability_repartition" = p_pie,
      "location_effects" = out_location,
      "germplasm_effects" = out_germplasm,
      "interaction_matrix" = data_interaction
    )
    
    
    # 3.2. PCA on interaction matrix ----------
    pca = PCA(data_interaction, scale.unit = TRUE, graph = FALSE)

    # 3.2.1. Ecovalence ----------
    m_eco = data_interaction^2
    
    d_eco = data.frame(
      germplasm = rep(rownames(m_eco), times = ncol(m_eco)), 
      location = rep(colnames(m_eco), each = nrow(m_eco)),
      variable = as.vector(m_eco)
      )
    
    p_eco = ggplot(data = d_eco, aes(x = location, y = germplasm, fill = variable)) + geom_raster()
    p_eco = p_eco + scale_fill_gradient(low = "green", high = "red") 
    p_eco = p_eco + ggtitle("Wrick ecovalence", variable)
    
    # 3.2.2. Biplots ----------
    variation_dim = fviz_eig(pca)
    which_won_where = ggplot_which_won_where(pca)
    mean_vs_stability = ggplot_mean_vs_stability(pca)
    discrimitiveness_vs_representativeness = ggplot_discrimitiveness_vs_representativeness(pca)
    
    # 3.2.3. Return PCA results ----------
    out_pca = list(
      "ecovalence" = p_eco,
      "biplot" = list(
        "variation_dim" = variation_dim,
        "which_won_where" = which_won_where,
        "mean_vs_stability" = mean_vs_stability,
        "discrimitiveness_vs_representativeness" = discrimitiveness_vs_representativeness
      )
    )
    
    # 1.4. Return results ----------
    out = list(
      "descriptive" = out_descriptive,
      "ANOVA" = out_anova,
      "PCA" = out_pca
    )
    
    message(gxe_analysis, " model done for ", variable)
    
    return(out)
  }
  
  # 2. write function post ammi analysis regrouping all ammi analysis from different variables ----------
  fun_post_gxe = function(out_ammi)
    # go! ----------
    {
  # 2.1. barplot_variation_repartition ---------- 
    dtmp = data.frame()
    for(i in 1:length(out_ammi)){
      d = out_ammi[[i]]$ANOVA$variability_repartition$data
      d = cbind.data.frame(names(out_ammi)[i], d)
      dtmp = rbind.data.frame(dtmp, d)
    }
    colnames(dtmp)[1] = "var"
    p = ggplot(dtmp, aes(x = var, y = percentage_Sum_sq)) + geom_bar(stat = "identity", aes(fill = factor))
    p = p + ylab("pourcentage de variation") + theme(axis.text.x = element_text(angle = 90))
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    barplot_variation_repartition = p + scale_fill_manual(values = cbbPalette)
    
    # 2.2. PCA on effects -----------
    
    # 2.2.1. Get data sets ----------
    n_G = n_E = n_varG = NULL
    for(i in 1:length(out_ammi)){
      n_G = c(n_G, names(out_ammi[[i]]$ANOVA$germplasm_effects$effects))
      n_varG = c(n_varG, names(out_ammi[[i]]$ANOVA$germplasm_effects$intra_variance))
      n_E = c(n_E, names(out_ammi[[i]]$ANOVA$location_effects$effects))
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
      g = out_ammi[[i]]$ANOVA$germplasm_effects$effects
      df_G[names(g),i] = g
      vg = out_ammi[[i]]$ANOVA$germplasm_effects$intra_variance
      df_varG[names(vg),i] = vg
      e = out_ammi[[i]]$ANOVA$location_effects$effects
      df_E[names(e),i] = e
    }
    
    # 2.2.2. Run PCA ----------
    PCA_G = PCA(df_G, scale.unit = TRUE, graph = FALSE)
    PCA_intraG = PCA(df_varG, scale.unit = TRUE, graph = FALSE)
    PCA_E = PCA(df_E, scale.unit = TRUE, graph = FALSE)

    PCA_G = list(
      "variation_dim" = fviz_eig(PCA_G),
      "ind" = fviz_pca_ind(PCA_G),
      "var" = fviz_pca_ind(PCA_G)
    )
    
    PCA_intraG = list(
      "variation_dim" = fviz_eig(PCA_intraG),
      "ind" = fviz_pca_ind(PCA_intraG),
      "var" = fviz_pca_ind(PCA_intraG)
    )
    
    PCA_E = list(
      "variation_dim" = fviz_eig(PCA_E),
      "ind" = fviz_pca_ind(PCA_E),
      "var" = fviz_pca_ind(PCA_E)
    )
    
    out = list(
      "barplot_variation_repartition" = barplot_variation_repartition,
      "PCA_G_effect" = PCA_G,
      "PCA_intraG_effect" = PCA_intraG,
      "PCA_E_effect" = PCA_E
    )
  }
  

  # 3. Apply analysis and Post analysis to vec_variables ----------
  message("I. Run ", gxe_analysis, " model on each variable")
  out_gxe = lapply(vec_variables, fun_gxe, data, gxe_analysis)
  names(out_gxe) = vec_variables
  
  message("\nII. Post ", gxe_analysis," analysis on all outputs")
  #out_post_gxe = fun_post_gxe(out_gxe)
  out_post_gxe = fun_post_gxe(out_gxe)
  
  OUT = list(out_gxe, out_post_gxe)
  names(OUT) = c(gxe_analysis, paste("Post_", gxe_analysis, sep = ""))
  
  return(OUT)
}
