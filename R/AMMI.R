# 0. help ----------
#' Run AMMI model on a data set
#'
#' @description
#' \code{AMMI} runs AMMI model on a data set and gives back results on germplasms, locations, years and GxE interactions effects
#' 
#' @param data data with at least the following columns: year, location, germplasm, block, X, Y
#' 
#' @param vec_variables vectors of variables to analyse
#' 
#' @details See the vignette for more details on the AMMI model
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
#'      \item "PCA" being a list with 
#'       \itemize{
#'        \item "variation_dim"
#'        \item "CP1-CP2" being a list with 
#'         \itemize{
#'          \item "ind"
#'          \item "var"
#'          }
#'        \item "CP2-CP3" being a list with 
#'         \itemize{
#'          \item "ind"
#'          \item "var"
#'         }
#'       }
#'     }
#'   }
#'   
#'  \item a post AMMI analysis with
#'   \itemize{
#'    \item "barplot_variation_repartition"
#'    \item "PCA_G_effect"
#'    \item "PCA_intraG_effect"
#'    \item "PCA_E_effect"
#'   }
#'  }
#' 
#' @author 
#' Jonathan Locqueville, Maxime Garnault and Pierre Riviere
#' 
AMMI = function(
  data, 
  vec_variables
)
# Lets' go ----------
  {
    
    # Error messages ----------
    
  # 1. write the ammi function to apply to vec_variables ----------
  fun_ammi = function(variable, data) 
    # go! ----------
    {
    # 1.1. Set up data set ----------
      colnames(data)[which(colnames(data) == variable)] = "variable"
      data = data[c("location", "germplasm", "year", "block", "variable")]
      data$variable = as.numeric(as.character(data$variable))
    
    # 1.2. AMMI model which depends on the years available in the data set ----------
    
    # 1.2.1. Run the model ----------
    data$block_in_env = factor(paste(data$block, data$location, sep = ";")) # hierarchise block within environemnt
    data$YxE = factor(paste(data$year, data$location, sep = ";"))
    data$YxG = factor(paste(data$year, data$germplasm, sep = ";"))
    
    # options(contrasts = c("contr.treatment", "contr.poly")) default options
    options(contrasts = c("contr.sum", "contr.sum")) # to get sum of parameters = 0
    
    if(nlevels(data$year) > 1) { 
      model = lm(variable ~ germplasm*location + block_in_env + year + YxG + YxE, data = data)
    } else {
      model = lm(variable ~ germplasm*location + bloc_in_env, data = data)
    }
    options(contrasts = c("contr.treatment", "contr.poly")) # Come back to default options
    
    anova_model = anova(model)
    
    # 1.2.2. Check residuals (qqplot, Skewness & Kurtosis tests) ----------
    outRes = gverifResidualsnormality(model)
    
    # 1.2.3. repartition of variability among factors
    pie = gpieplot(anova_model, variable)
    
    # 1.2.4. Get effects ----------
    outinter = build_interaction_matrix(model)
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
    p1_GxE = p1_GxE + stat_summary(fun.y = mean, geom = "line", aes(linetype = germplasm), size = 1) + scale_linetype_manual(values=rep(c("solid", "dotted"), 6))
    
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    cbbPalette <- c("#000000", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
    
    p1_GxE = p1_GxE + scale_color_manual(values=rep(cbbPalette, each = 2))
    
    p1_GxE = p1_GxE + labs(title = paste("Graphique d'interaction pour la variable :" ,variable,"en fonction du germplasme et de l'environnement"))
    p1_GxE = p1_GxE + theme(axis.text.x=element_text(size=15,angle=90), plot.title = element_text(lineheight=.8, face="bold"))
    # p2_GxE + ggtitle("") + xlab("") + ylab("") + theme(legend.title=element_blank())
    
    if(nlevels(data$year) > 1) { 
      p1_GxE = p1_GxE + facet_grid(year ~ .)
    }
    
    # 1.5.2. PCA on interaction matrix ----------
    pca = PCA(data_interaction, scale.unit = TRUE, graph = FALSE)
    outPCA = dographPCA(pca, "germplasm")
    
    outPCA$screeplot = outPCA$screeplot + labs(title = paste("Pourcentage de la variance expliquee par chaque composante principale, ACP sur le facteur germplasme,\n variable etudiee :",variable)) # graphique représentant le pourcentage de variabilité totale pour chaque composante principale
    
    # 1.5.3. Ecovalence
    ecovalence = ecovalence(data_interaction,variable)
    
    out_GxE = list(
      "interaction_plot" = p1_GxE,
      "ecovalence" = ecovalence,
      "PCA" = list(
        "variation_dim" = outPCA$screeplot,
        "CP1-CP2" = list("ind" = outPCA$ind1_2, "var" = outPCA$var1_2),
        "CP2-CP3" = list("ind" = outPCA$ind2_3, "var" = outPCA$var2_3)
      )
    )
    
    # 1.6. Return results ----------
    out = list(
      "model" = out_model,
      "germplasm" = out_germplasm,
      "location" = out_location,
      "GxE" = out_GxE
    )
    
    message("AMMI model done for ",variable)
    
    return(out)
  }
  
  # 2. write function post ammi analysis regrouping all ammi analysis from different variables ----------
  fun_post_ammi = function(out_ammi)
    # go! ----------
    {
    dtmp = data.frame()
    for(i in 1:length(out_ammi)){
      d = out_year_and_env[[i]]$model$variability_repartition$data
      d = cbind.data.frame(names(out_ammi)[i], d)
      dtmp = rbind.data.frame(dtmp, d)
    }
    colnames(dtmp)[c(1, 2)] = c("var", "facteur")
    
    fr = c("année", "bloc", "ferme", "population", "population x ferme", "résiduelle", "anée x ferme", "population x année")
    names(fr) = c("year", "block", "location", "germplasm", "germplasm:location", "residuals", "YxE", "YxG")
    dtmp$facteur = factor(fr[dtmp$facteur])
    
    p = ggplot(dtmp, aes(x = var, y = percentage_Sum_sq)) + geom_bar(stat = "identity", aes(fill = facteur))
    p = p + ylab("pourcentage de variation") + theme(axis.text.x = element_text(angle = 90))
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    barplot_variation_repartition = p + scale_fill_manual(values = cbbPalette)
    
    PCA_G_effect = NULL
    PCA_intraG_effect = NULL
    PCA_E_effect = NULL
    
    out = list(
      "barplot_variation_repartition" = barplot_variation_repartition,
      "PCA_G_effect" = PCA_G_effect,
      "PCA_intraG_effect" = PCA_intraG_effect,
      "PCA_E_effect" = PCA_E_effect
    )

  }
    
  # 3. Apply AMMI and Post AMMI to vec_variables ----------
  out_ammi = lapply(vec_variables, fun_ammi, data)
  names(out_ammi) = vec_variables
  
  out_post_ammi = fun_post_ammi(out_ammi)
  
  OUT = list("AMMI" = out_ammi, "Post_AMMI" = out_post_ammi)
  
  return(OUT)
}
