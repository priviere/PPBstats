#' Run hedonic test analysis
#'
#' @description
#' \code{hedonic} runs hedonic analysis
#' 
#' @param data
#' 
hedonic = function(
  data
  )
{
  # 0. Error message ----------
  if(!is(data, "data_organo_hedonic")){ 
    stop(substitute(data), " must be formated, see PPBstats::format_data_PPBstats().") 
  }
  
  # ANOVA ----------
  model = lm(note ~ juges + germplasm, data)
  
  # CA ----------
  vec_var_sup = c(
    (which(colnames(data) == "note") + 1) :
      (which(colnames(data) == "sample") - 1)
  )
  
  d_CA_1 = d[,c( (which(colnames(data) == "sample") + 1) : ncol(d))]
  d_CA_2 = d[,vec_var_sup]
  d_CA = cbind.data.frame(d_CA_1, d_CA_2)
  rownames(d_CA) = d$sample
  out_CA = CA(d_CA, ncp = 5, row.sup = NULL, col.sup = c( c(ncol(d_CA_1) + 1) : ncol(d_CA) ) )
}
