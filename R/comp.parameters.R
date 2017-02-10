#' Get parameter comparisons two by two or to a given threshold based on MCMC outputs
#'
#' @description
#' \code{comp.parameters} performs parameter comparisons based on MCMC outputs
#'
#' @param MCMC MCMC data frame.
#'  
#' @param parameter The parameter on which to get the comparisons 
#' 
#' @param type The type of comparisons. 1 for comparison two by two. 2 for comparison to a specific threshold.
#'
#' @param threshold For type = 2, the threshold to which a parameter is different
#' 
#' @details
#' The comparisons is based on the probability of having a common distribution for each pair of parameters (type 1) or to be different from a specific threshold (type 2). More details in the vignette (type vignette ("PPBstats")).
#' 
#' @return The function returns 
#' \itemize{
#' \item for type = 1 : a square matrix with the probability of having a common distribution for each pair of parameter.
#' \item for type = 2 : a vector with the probability of being different from the given threshold.
#'} 
#'
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{mean_comparisons_model_1}}
#' \item \code{\link{mean_comparisons_model_2}}
#' }
#' 
comp.parameters = function(
  MCMC,
  parameter = "mu",
  type = 1,
  threshold = 1
  )
{
# 1. Error message and update arguments ----------
if( !is.data.frame(MCMC) ) { stop("MCMC must be a data frame.") }

if( length(grep(parameter, colnames(MCMC))) == 0 ) { stop(paste(parameter, "is not in the colnames of MCMC.")) }
    
MCMC1 = MCMC[,grep(parameter, colnames(MCMC))]
if( is.null(ncol(MCMC1)) ) { MCMC1 = as.data.frame(matrix(MCMC1, ncol = 1)); colnames(MCMC1) = parameter }

vec_para_median = sort(apply(MCMC1, 2, median))
vec_element = names(vec_para_median)

if( length(vec_element) < 2 & type ==1 ) { warning("With type = 1, MCMC must have at least two parameters."); return(NULL)}

# 2. Comparisons of parameters two by two ----------
if(type == 1) {

	if(length(vec_element)>1) {
		vec_combi = combn(vec_element, m = 2)
		rownames(vec_combi) = c("para1", "para2")
	
		Mpvalue = matrix(0, ncol = length(vec_element), nrow = length(vec_element))
		colnames(Mpvalue) = rownames(Mpvalue) = vec_element
	
		for (i in 1:ncol(vec_combi)) {
				para1 = vec_combi["para1", i]
				para2 = vec_combi["para2", i]
	
				# in theorie dataY2 > dataY1 (because it has been sorted by median)
				dataY1 = MCMC1[,para1]
				dataY2 = MCMC1[,para2]
				dataY3 = dataY1  - dataY2
	
				a = which(dataY3 >= 0)
				pvalue = length(a) / length(dataY3)
				Mpvalue[para1, para2] = pvalue				
				}
	
		} else {Mpvalue = matrix(1, ncol = 1, nrow = 1); colnames(Mpvalue) = rownames(Mpvalue) = vec_element}

	return(Mpvalue)
	}

# 3. Comparison of a parameter to a specific threshold ----------
if(type == 2)  {
	pvalue = NULL

	for (b in 1:length(vec_element)) {
		MCMC = as.vector(MCMC1[,vec_element[b]])
		if(median(MCMC) > threshold) { p = length(which(MCMC < threshold)) } else { p = length(which(MCMC > threshold)) }
		pvalue = c(pvalue, p / length(MCMC))
	}
	names(pvalue) = vec_element

	return("pvalue" = pvalue)
	}
}

