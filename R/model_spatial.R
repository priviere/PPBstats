#' Run spatial row and column model
#'
#' @description
#' \code{model_spatial} runs spatial row and column model based on the SpATS package
#' 
#' @param data The data frame on which the model is run. 
#' It should come from \code{\link{format_data_PPBstats.data_agro}}
#' 
#' @param variable variable to analyse
#' 
#' 
#' @details 
#' The model is run with the SpATS function of package SpATS. 
#' See ?SpATS for more information.
#' 
#' More information can be found in the book \href{https://priviere.github.io/PPBstats_book/family-1.html#spatial-analysis}{here}.
#' 
#' @return 
#' The function returns a list with two elements :
#' \itemize{
#'  \item info : a list with variable
#'  \item model : 
#'   \itemize{
#'    \item model: the output from SpATS function
#'    \item summary: summary of the model
#'    \item df_residuals: effective degree of freedom of the residuals
#'    \item MSerror: Mean Square error
#'   }
#' }
#' 
#' @author 
#' Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}
#' \item \code{\link{check_model.fit_model_spatial}}
#' }
#' 
#' @importFrom SpATS SpATS
#' @importFrom SpATS summary.SpATS
#' @importFrom methods is
#' 
#' @export
#' 
model_spatial = function(data, variable){

  # 1. Error messages, update arg ----------
  if(!is(data, "data_agro")){ stop(substitute(data), " must be formated with type = \"data_agro\", see PPBstats::format_data_PPBstats().") }
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
    SpATS::SpATS(
    response = "variable", 
    genotype = "germplasm", 
    genotype.as.random = TRUE,
    spatial = ~ PSANOVA(col, row, nseg = c(nlevels(data_tmp$X), nlevels(data_tmp$Y))),
    random = ~ col_f + row_f, 
    data = data_tmp)
  )
  
  # 4. MSerror for mean comparisons ----------
  s = summary.SpATS(m, which = "dimensions")
  deviance_model = deviance(m)
  df_residual = as.numeric(s$p.table.dim["Residual", "Effective"])
  MSerror = deviance_model / df_residual
  
  # 5. Return results ----------
  out = list(
    "info" = list("variable" = variable, "data" = data), 
    "model" = list(
      "model" = m,
      "summary" = summary(m),
      "df_residual" = df_residual,
      "MSerror" = MSerror
    )
  )
  
  class(out) <- c("PPBstats", "fit_model_spatial")
  return(out)
}

