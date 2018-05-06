#' Run hedonic analysis
#'
#' @description
#' \code{model_hedonic} runs hedonic analysis
#' 
#' @param data  data is a data frame with the following columns: sample, juges, note, descriptors, germplasm, location. 
#' The descriptors must be separated by ";". Any other column can be added as supplementary variables.
#' 
#' @details
#' An anova is run on "note".
#' A Correspondence Analysis is run with FactoMineR::CA
#' 
#' @return 
#' A list with the anova and the CA object
#' 
model_hedonic = function(
  data
  )
{
  # 0. Error message ----------
  if(!is(data, "data_organo_hedonic")){ 
    stop(substitute(data), " must be formated, see PPBstats::format_data_PPBstats().") 
  }
  
  var_sup = data$var_sup
  data = data$data
  
  # ANOVA ----------
  model = lm(note ~ juges + germplasm, data)
  
  # CA ----------
  quanti.sup = quali.sup = NULL
  for(v in 1:length(var_sup)){
    if( is.numeric(data[,v]) ) { quanti.sup = c(quanti.sup, v)}
    if( is.factor(data[,v]) ) { quali.sup = c(quali.sup, v) }
  }
  for(v in quanti.sup){ # CA does not work if NA on column
    to_delete = which(is.na(data[,v]))
    if( length(to_delete) > 0 ){
      data = data[-to_delete,]
      warning("Rows in column \"", colnames(data)[v], "\" has been deleted because of NA.", sep = "")
    }
  }

  remove_row_with_no_descriptors = which(apply(data[,(length(var_sup)+1):ncol(data)], 1 , sum) == 0)
  if( length(remove_row_with_no_descriptors) > 0 ) { 
    data = data[-remove_row_with_no_descriptors,]
    warning("Some rows have been removed because there are no descriptors.")
  } 
  
  out_CA = CA(data, quanti.sup = quanti.sup, quali.sup  = quali.sup, graph = FALSE)
  out = list("model" = model, "CA" = out_CA)
  class(out) <- c("PPBstats", "fit_model_hedonic")
  return(out)
  }
