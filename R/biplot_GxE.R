#' Compute ecovalence and format PCA results
#'
#' @description
#' \code{biplot_GxE} computes ecovalence and format PCA results from \code{\link{check_model}} with \code{\link{GxE}}
#' 
#' @param out_check_model_GxE Output from \code{\link{check_model}} with \code{\link{GxE}}
#' 
#' @details
#' The ecovalence is the interaction matrix squared.
#' 
#' @return 
#' The function returns a list with:
#' \itemize{
#' \item info : a list with variable and gxe_analysis
#' \item data_ecovalence : the ecovalence matrix
#' \item pca : the pca object
#' }
#'  
#' @author Pierre Riviere
#'
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}, 
#' \item \code{\link{check_model_GxE}}, 
#' \item \code{\link{get_ggplot}},
#' \item \code{\link{ggplot_biplot_GxE}}
#' }
#'  
#' 
#' 
biplot_GxE = function(out_check_model_GxE){
  
  # 1. Error message ----------
  if( attributes(out_check_model_GxE)$PPBstats.object != "check_model_GxE" ) { stop("data must come from PPBstats::check_model with GxE.") }
  
  data_interaction = out_check_model_GxE$GxE$ANOVA$interaction_matrix
  
  # 2. Ecovalence ----------
#  m_eco = data_interaction^2
  m_eco = as.matrix(data_interaction)
  for (i in 1:nrow(m_eco)){rownames(m_eco)[i] = paste(rownames(m_eco)[i]," (",format(sum(m_eco[i,]^2),scientific=T, digits=3),")",sep="")}
  for (j in 1:ncol(m_eco)){colnames(m_eco)[j] = paste(colnames(m_eco)[j]," (",format(sum(m_eco[,j]^2),scientific=T, digits=3),")",sep="")}
  
  data_ecovalence = data.frame(
    germplasm = rep(rownames(m_eco), times = ncol(m_eco)), 
    location = rep(colnames(m_eco), each = nrow(m_eco)),
    variable = as.vector(m_eco)
  )
  
  # 3. Return results ----------
  out = list(
    "info" = out_check_model_GxE$GxE$info,
    "data_ecovalence" = data_ecovalence,
    "pca" = out_check_model_GxE$GxE$PCA
  )
  
  attributes(out)$PPBstats.object = "biplot_GxE"
  
  return(out)
}



