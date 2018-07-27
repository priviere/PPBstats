#' Run AMMI or GGE model
#'
#' @description
#' \code{model_GxE} runs AMMI or GGE model
#' 
#' @param data The data frame on which the model is run. It should come from \code{\link{format_data_PPBstats.data_agro}}
#' 
#' @param variable variable to analyse
#' 
#' @param gxe_analysis the analysis to carry out: "AMMI" or "GGE"
#' 
#' @details 
#' scaling for interaction.matrix is not useful as the column mean is equal to 0 because of model constraints and all the values are regarding one variable, so it is possible to compare it into the PCA.
#' 
#' More information can be found in the book
#' \itemize{
#'  \item AMMI : https://priviere.github.io/PPBstats_book/family-2.html#ammi
#'  \item GGE : https://priviere.github.io/PPBstats_book/family-2.html#gge
#' }
#' 
#' @return 
#' The function returns a list with three elements :
#' \itemize{
#'  \item info : a list with variable and gxe_analysis
#'  \item ANOVA a list with five elements:
#'   \itemize{
#'    \item model
#'    \item anova_model
#'    \item germplasm_effects a list of two elements:
#'     \itemize{
#'      \item effects
#'      \item intra_variance
#'     }
#'    \item location_effects
#'    \item interaction_matrix
#'   }
#'  
#'  \item PCA : PCA object from FactoMineR
#' }
#' 
#' @author 
#' Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{GxE_build_interaction_matrix}},
#' \item \code{\link{check_model}},
#' \item \code{\link{check_model.fit_model_GxE}}
#' }
#' 
#' @import stats
#' @import FactoMineR
#' @importFrom methods is
#' 
#' @export
#' 
model_GxE = function(
  data, 
  variable,
  gxe_analysis = NULL
)
  {
    # 1. Error messages ----------
    if(!is(data, "data_agro")){ stop(substitute(data), " must be formated with type = \"data_agro\", see PPBstats::format_data_PPBstats().") }
    check_data_vec_variables(data, variable)
    if( is.null(gxe_analysis) ) { stop("You ust set gxe_analysis: AMMI or GGE") }
    if(!is.element(gxe_analysis, c("AMMI", "GGE"))) { stop("gxe_analysis must be either \"AMMI\" or \"GGE\".") }
    
    # 2. Set up data set ----------
      colnames(data)[which(colnames(data) == variable)] = "variable"
      data = data[c("location", "germplasm", "year", "block", "variable")]
      data = droplevels(na.omit(data))
      
    # 3. ANOVA ----------
    # options(contrasts = c("contr.treatment", "contr.poly")) default options
    options(contrasts = c("contr.sum", "contr.sum")) # to get sum of parameters = 0
    
    if(nlevels(data$year) > 1) { # depends on the years available in the data set
      model = stats::lm(variable ~ germplasm*location + block %in% year:location + year + year:germplasm + year:location , data = data)
    } else {
      model = stats::lm(variable ~ germplasm*location + block %in% location, data = data)
    }
    options(contrasts = c("contr.treatment", "contr.poly")) # Come back to default options
    
    anova_model = stats::anova(model)
    
    # 3.1. Get effects ----------
    
    c = model$coefficients
    
    # 3.2. germplam effects ----------
    coef_germ = c[grep("germplasm", names(c))]
    todel = grep(":", names(coef_germ))
    if(length(todel) > 0) { coef_germ = coef_germ[-todel] }
    coef_germ = c(coef_germ, - sum(coef_germ))
    names(coef_germ) = model$xlevels$germplasm

    var_intra = tapply(model$residuals, model$model$germplasm, var, na.rm = TRUE)
    
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
      "effects" = coef_env
      )
    
    # 3.3 interaction matrix ----------
    data_interaction = GxE_build_interaction_matrix(data, gxe_analysis)
    
    # 3.4. Return ANOVA results ----------
    out_anova = list(
      "model" = model,
      "anova_model" = anova_model,
      "germplasm_effects" = out_germplasm,
      "location_effects" = out_location,
      "interaction_matrix" = data_interaction
    )
    
    
    # 4. PCA on interaction matrix ----------
    out_pca = FactoMineR::PCA(data_interaction, scale.unit = TRUE, graph = FALSE)

    # 5. Return results ----------
    out = list(
      "info" = list("variable" = variable, "gxe_analysis" = gxe_analysis),
      "ANOVA" = out_anova,
      "PCA" = out_pca
    )
    
    message(gxe_analysis, " model done for ", variable)
    
    class(out) <- c("PPBstats", "fit_model_GxE")
    return(out)
  }