#' Run hedonic analysis
#'
#' @description
#' \code{model_hedonic} runs hedonic analysis
#' 
#' @param data  data is a data frame. It should come from \code{\link{format_data_PPBstats.data_organo_hedonic}}
#' 
#' @details
#' An anova is run on "note".
#' A Correspondence Analysis is run with FactoMineR::CA
#' 
#' More information can be found in the book : https://priviere.github.io/PPBstats_book/hedonic.html
#' 
#' @return 
#' It returns a list with three elements:
#' \itemize{
#'  \item model : the result of the anova run on note
#'  \item CA the result of the correspondance analysis run on the data set with the supplementary variables with \code{FactoMineR::CA}.
#'  \item HCPC the result of the correspondane analysis run on the data set with the supplementary variables with \code{FactoMineR::PCA} follow by \code{FactoMineR::HCPC}.
#'  It is a list of three elements:
#'  \itemize{
#'   \item res.pca the results of the PCA
#'   \item res.hcpc the results of the HCPC
#'   \item clust the cluster found with the HCPC
#'  }
#' }
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}
#' \item \code{\link{check_model.fit_model_hedonic}}
#' }
#'
#' @import stats
#' @import FactoMineR
#' @importFrom methods is
#' 
#' @export
#'
model_hedonic = function(
  data
  )
{
  # 0. Error message ----------
  if(!is(data, "data_organo_hedonic")){ 
    stop(substitute(data), " must be formated with type = \"data_organo_hedonic\", see PPBstats::format_data_PPBstats().") 
  }
  
  var_sup = data$var_sup
  descriptors = data$descriptors
  data_sample = data$data$data_sample
  data_sample_mean = data$data$data_sample_mean
  data_juges = data$data$data_juges
  
  # ANOVA ----------
  model = stats::lm(note ~ juges + germplasm, data_sample)
  
  # CA ----------
  quanti.sup_ca = "note"
  quali.sup_ca = c("sample", "germplasm", "location")
  
  for(v in quanti.sup_ca){ # CA does not work if NA on column
    to_delete = which(is.na(data_sample_mean[,v]))
    if( length(to_delete) > 0 ){
      data_sample_mean = data_sample_mean[-to_delete,]
      warning("Rows in column \"", v, "\" has been deleted because of NA.", sep = "")
    }
  }
  
  remove_row_with_no_descriptors = which(apply(data_sample_mean[,is.element(colnames(data_sample_mean), descriptors)], 1 , sum) == 0)
  if( length(remove_row_with_no_descriptors) > 0 ) { 
    data_sample_mean = data_sample_mean[-remove_row_with_no_descriptors,]
    warning("Some rows have been removed because there are no descriptors.")
  } 
  
  out_CA = FactoMineR::CA(data_sample_mean, 
                          quanti.sup = which(is.element(colnames(data_sample_mean), quanti.sup_ca)), 
                          quali.sup  = which(is.element(colnames(data_sample_mean), quali.sup_ca)), 
                          graph = FALSE)
  
  # HCPC ----------
  rownames(data_juges) = data_juges$juges
  data_juges_hcpc = data_juges[,c(2:ncol(data_juges))]
  
  quanti.sup = quali.sup = NULL
  for(v in var_sup){
    if( is.numeric(data_juges[,v]) ) { quanti.sup = c(quanti.sup, v) }
    if( is.factor(data_juges[,v]) ) { quali.sup = c(quali.sup, v) }
  }
  
  id_quanti.sup = which(is.element(colnames(data_juges_hcpc), quanti.sup))
  if( length(id_quanti.sup) == 0 ) { id_quanti.sup = NULL}
  id_quali.sup = which(is.element(colnames(data_juges_hcpc), quali.sup))
  if( length(id_quali.sup) == 0 ) { id_quali.sup = NULL}
  
  res.pca = PCA(data_juges_hcpc, 
                quanti.sup = id_quanti.sup, 
                quali.sup  = id_quali.sup, 
                graph = FALSE)
  res.hcpc = FactoMineR::HCPC(res.pca, cluster = -1, graph = FALSE, consol = 0)
  clust =  res.hcpc$data.clust
  clust$clust = paste("cluster", clust$clust)
  out_HCPC = list("res.pca" = res.pca, "res.hcpc"= res.hcpc, "clust" = clust)
  
  out = list("model" = model, "CA" = out_CA, "HCPC" = out_HCPC)
  class(out) <- c("PPBstats", "fit_model_hedonic")
  return(out)
  }
