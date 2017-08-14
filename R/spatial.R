#' Run spatial row and column model
#'
#' @description
#' \code{spatial} runs spatial row and column model based on the SpATS package
#' 
#' @param data The data frame on which the model is run. 
#' It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "..."), with "..." the variables.
#' 
#' @param variable variable to analyse
#' 
#' @param genotype.as.random TRUE of FALSE. TRUE by default.
#' 
#' @details 
#' The model is run with the SpATS function of package SpATS. 
#' See ?SpATS for more information.
#' 
#' @return 
#' The function returns a list with two elements :
#' \itemize{
#'  \item info : a list with variable
#'  \item model : 
#'   \itemize{
#'    \item model : the output from SpATS function
#'    \item summary : summary of the model
#'    \item germplasm_effects : germplasm effects, BLUE or BLUP depending if it is fixed or random effect
#'    \item var_res : variance of the residuals
#'   }
#' }
#' 
#' @author 
#' Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}
#' }
#' 
spatial = function(data, variable, genotype.as.random = TRUE){

  # 1. Error messages, update arg ----------
  if(!is(data, "data_agro")){ stop(substitute(data), " must be formated, see PPBstats::format_data_PPBstats().") }
  check_data_vec_variables(data, variable)
  data_tmp = data
  colnames(data_tmp)[which(colnames(data_tmp) == variable)] = "variable"
  
  # 2. Set up data set ----------
  data_tmp$col = as.numeric(data_tmp$X)
  data_tmp$row = as.numeric(data_tmp$Y)
  data_tmp$col_f = as.factor(data_tmp$col)
  data_tmp$row_f = as.factor(data_tmp$row)
  
  # 3. Run the model ----------
  m = suppressMessages(
    SpATS(
    response = "variable", 
    genotype = "germplasm", 
    genotype.as.random = genotype.as.random,
    spatial = ~ PSANOVA(col, row, nseg = c(nlevels(data_tmp$X), nlevels(data_tmp$Y))),
    random = ~ col_f + row_f, 
    data = data_tmp)
  )
  
  # 4. Get effetcs ----------
  intercept = mean(data$variable, na.rm = TRUE)
  g_effect = m$coeff[is.element(names(m$coeff), levels(d$germplasm))]
  g_effect = sort(g_effect + intercept)
  
  # 5. Residuals variance ----------
  s = summary(m, which = "variances")
  var_res = s$psi[1]
  
  # 6. Return results ----------
  out = list(
    "info" = list("variable" = variable, "data" = data), 
    "model" = list(
      "model" = m,
      "summary" = summary(m),
      "germplasm_effects" = g_effect,
      "var_res" = var_res
    )
    
  )
  
  class(out) <- c("PPBstats", "fit_model_spatial")
  return(out)
}



