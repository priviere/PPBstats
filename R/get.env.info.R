#' Get regional farms data and satellite farms data
#'
#' @description
#' \code{get.env.info} look at a data set and separate regional farms and satellite farms in two datasets. This function is used in \code{\link{model_bh_intra_location}}.
#'
#' @param D The data frame with its specific column names used in \code{\link{model_bh_intra_location}}
#'  
#' @param nb_ind The minimal number of individuals per variable
#' 
#' @return The function returns a list with 
#' \itemize{
#' \item "vec_env_with_no_data": a vector with the environments without data for the given variable
#' \item "vec_env_with_no_controls": a vector with the environments with no controls
#' \item "vec_env_with_controls": a vector with the environments with controls
#' \item "vec_env_RF": a vector with the environments as regional farms (i.e. with at least two blocks)
#' \item "vec_env_SF": a vector with the environments as satellite farms (i.e. with one block)
#' \item "D_RF": a data frame with regional farms
#' \item "D_SF": a data frame with satellite farms
#' }
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{model_bh_intra_location}}
#' 
#' @import dplyr
#' 
#' @export
#' 
get.env.info = function(
  D,
  nb_ind
  )
{
  block = entry = NULL # to avoid no visible binding for global variable
  
  # 1. Get informations on environments ----------
  vec_env_all = levels(D$environment)
  Dna = droplevels(D[which(!is.na(D$variable)),]) # Get rid of NA, keep the farm only where there is data
  vec_env_na = levels(Dna$environment)
  vec_env_with_no_data = vec_env_all[!is.element(vec_env_all, vec_env_na)]

  if ( length(vec_env_with_no_data) == 0 ) { vec_env_with_no_data = NULL }

  # 2. Get environment with controls ----------
  
  vec_env_with_controls = NULL

  for (env in vec_env_na) {

    d = droplevels(dplyr::filter(Dna, environment == env))
    
    test_g = length(unique(d$germplasm))
    
    if(test_g != 1) {
      w = with(d, table(germplasm, ID))
      
      G = n_ind = NULL
      for (i in 1:nrow(w)) {
        if( (ncol(w) - length(which(w[i,]==0)) ) > 1) { G = c(G, rownames(w)[i])}
        n_ind = c(n_ind, sum(w[i,]))
      }
      
      test = sum(n_ind) / length(n_ind) # mean number of individual per entry
      if(test >= nb_ind) { test = TRUE } else { test = FALSE}
      
      if(!is.null(G) & test){ vec_env_with_controls = c(vec_env_with_controls, env) }
    }
  }
  
  # 3. Get regional and satellite farms ----------

  if( !is.null(vec_env_with_controls) ) {
    vec_env_with_no_controls = vec_env_na[!is.element(vec_env_na, vec_env_with_controls)] 
    
    # Separate regional farms and satellite farms
    Dc = droplevels(dplyr::filter(Dna, environment %in% vec_env_with_controls))
    
    w = with(Dc, table(environment, block))
    if( ncol(w) == 1 ) { w = cbind(w, matrix(0, ncol = 1, nrow = nrow(w) )) }
    
    # regional farms : at leat two blocks
    vec_RF = rownames(w)[which(w[, 2] != 0)]

    if( length(vec_RF) > 0 ) {
      D_RF = droplevels(dplyr::filter(Dc, environment %in% vec_RF))
      D_RF = dplyr::arrange(D_RF, environment, block, entry)
    } else { vec_RF = D_RF = NULL }
        

    # satellite farms : only one block
    vec_SF = rownames(w)[which(w[,2] == 0)]
    if( length(vec_SF) > 0 ) {
      D_SF = droplevels(dplyr::filter(Dc, environment %in% vec_SF))
      D_SF = dplyr::arrange(D_SF, environment, block, entry)
    } else { vec_SF = D_SF = NULL }
    
  } else { 
    vec_env_with_no_controls = vec_env_na
    vec_RF = D_RF = vec_SF = D_SF = NULL
  }
  
   
  # 4. Return results ----------
  
  out = list(
    vec_env_with_no_data = vec_env_with_no_data,
    vec_env_with_no_controls = vec_env_with_no_controls,
    vec_env_with_controls = vec_env_with_controls,
    vec_env_RF = vec_RF,
    vec_env_SF = vec_SF,
    D_RF = D_RF,
    D_SF = D_SF
  )
  
return(out)
}
