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
#' \item data_interaction : the interaction matrix
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
  m_eco = data_interaction^2
  data_ecovalence = data.frame(
    germplasm = rep(rownames(m_eco), times = ncol(m_eco)), 
    location = rep(colnames(m_eco), each = nrow(m_eco)),
    variable = as.vector(m_eco)
  )
  
  
  m_inter = as.matrix(data_interaction)
  for (i in 1:nrow(m_inter)){
    rownames(m_inter)[i] = paste(rownames(m_inter)[i]," (",format(sum(m_inter[i,]^2),scientific=T, digits=3),")",sep="")
    }
  for (j in 1:ncol(m_inter)){
    colnames(m_inter)[j] = paste(colnames(m_inter)[j]," (",format(sum(m_inter[,j]^2),scientific=T, digits=3),")",sep="")
    }
  
  data_inter = data.frame(
    germplasm = rep(rownames(m_inter), times = ncol(m_inter)),
    location = rep(colnames(m_inter), each = nrow(m_inter)),
    variable = as.vector(m_inter)
  )
  
  # 3. Return results ----------
  out = list(
    "info" = out_check_model_GxE$GxE$info,
    "data_ecovalence" = data_ecovalence,
    "data_interaction" = data_inter,
    "pca" = out_check_model_GxE$GxE$PCA
  )
  
  attributes(out)$PPBstats.object = "biplot_GxE"
  
  return(out)
}

