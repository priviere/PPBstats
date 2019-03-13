#' Run multivariate analysis with function from FactoMineR
#'
#' @description
#' \code{mutlivariate} runs multivariate analysis with function from FactoMineR
#' 
#' @param data The data frame. It should come from \code{\link{format_data_PPBstats.data_agro}}
#' 
#' @param vec_variables vector of variables to use in the analysis
#' 
#' @param FUN function to apply coming from FactoMineR, for example PCA
#' 
#' @details 
#' This function is useful to settle the right data set and quali.sup
#' 
#' More information can be found in the book : https://priviere.github.io/PPBstats_book/family-5.html
#' 
#' @return 
#' The function returns the FactoMineR object corresponding to FUN
#' 
#' @author Pierre Riviere
#' 
#' @importFrom methods is
#' 
#' @export
#' 
mutlivariate = function(
  data, 
  vec_variables, 
  FUN){
  
  # 1. Error message and update arguments ----------
  if(!is(data, "data_agro")){ stop(substitute(data), " must be formated with type = \"data_agro\", see PPBstats::format_data_PPBstats().") }
  check_data_vec_variables(data, vec_variables)
  
  if( !is.element(as.character(substitute(FUN)), c("PCA")) ) { stop("Fun can only be PCA") }
  
  # 2. Run FUN ----------
  df = data[,c("location", "year", "germplasm", "block", "X", "Y", vec_variables)]
  
  # 3. Return results ----------
  out = FUN(df, quali.sup = c(1:6), graph = FALSE)
  return(out)
}

