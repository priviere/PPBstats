#' Run classic anova model
#'
#' @description
#' \code{model_anova} runs classic anova model
#' 
#' @param data The data frame on which the model is run. It should come from \code{\link{format_data_PPBstats.data_agro}}
#' 
#' @param variable variable to analyse
#' 
#' @details 
#' More information can be found in the book regarding \href{https://priviere.github.io/PPBstats_book/family-1.html#classic-anova}{classic anova}.
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
#'   }
#' }
#' 
#' @author 
#' Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}},
#' \item \code{\link{check_model.fit_model_anova}}
#' }
#' 
#' @import stats
#' @importFrom methods is
#' 
#' @export
#' 
model_anova = function(
  data, 
  variable
)
  {
    # 1. Error messages ----------
    if(!is(data, "data_agro")){ stop(substitute(data), " must be formated with type = \"data_agro\", see PPBstats::format_data_PPBstats().") }
    check_data_vec_variables(data, variable)

    # 2. Set up data set ----------
    colnames(data)[which(colnames(data) == variable)] = "variable"
    data = data[c("location", "germplasm", "year", "block", "variable")]
    data = droplevels(na.omit(data))
      
    # 3. ANOVA ----------
    # options(contrasts = c("contr.treatment", "contr.poly")) default options
    options(contrasts = c("contr.sum", "contr.sum")) # to get sum of parameters = 0
    model = stats::lm(variable ~ germplasm + block, data = data)
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
    
    # 3.4. Return ANOVA results ----------
    out_anova = list(
      "model" = model,
      "anova_model" = anova_model,
      "germplasm_effects" = out_germplasm
    )
    
    
    # 4. Return results ----------
    out = list(
      "info" = list("variable" = variable),
      "ANOVA" = out_anova
    )
    
    class(out) <- c("PPBstats", "fit_model_anova")
    return(out)
}
