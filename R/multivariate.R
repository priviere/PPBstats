#' Run multivariate analysis with function from FactoMineR
#'
#' @description
#' \code{mutlivariate} runs multivariate analysis with function from FactoMineR
#' 
#' @param data The data frame. See details.
#' 
#' @param vec_variables vector of variables to use in the analysis
#' 
#' @param FUN function to apply : PCA
#' 
#' @details 
#' This function is useful to settle the right data set and quali.sup
#' 
#' @return 
#' The function returns the FactoMineR object corresponding to FUN
#' 
#' @author Pierre Riviere
#' 
mutlivariate = function(
  data, 
  vec_variables, 
  FUN){
  
  # 1. Error message and update arguments ----------
  if(!is(data, "data_agro")){ stop(substitute(data), " must be formated, see PPBstats::format_data_PPBstats().") }
  check_data_vec_variables(data, vec_variables)
  
  if( !is.element(as.character(substitute(FUN)), c("PCA")) ) { stop("Fun can only be PCA") }
  
  # 2. Run FUN ----------
  df = data[,c("location", "year", "germplasm", "block", "X", "Y", vec_variables)]
  
  # 3. Return results ----------
  out = FUN(df, quali.sup = c(1:6), graph = FALSE)
  return(out)
}
