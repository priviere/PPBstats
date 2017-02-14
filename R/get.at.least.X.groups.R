#' Get the value of type one error needed to have X groups.
#'
#' @description
#' \code{get.at.least.X.groups} gets the value of type one error needed to have X groups. X goes from 2 to the maximum number of groups possible.
#' 
#' @param Mpvalue A vector or a matrix with the probabilities for two parameters to be equal. It comes from the function \code{\link{comp.parameters}}
#'  
#' @param MCMC MCMC. It is a data frame. 
#' 
#' @param p.adj NULL for no adjustement of the type one error. "soft.bonf" for a soft bonferonni correction to take into account multiple comparisons (alpha / number of parameters).
#' 
#' @param precision The precision of the type one error with the corresponding groups. The smaller the better, but the smaller the more time consuming due to computing matters
#' 
#' @details
#' This function is helpful to get at least two (or more) groups with a given type one error.
#' 
#' When there is no data, it returns NA. It may depend on the precision. For exemple, if you go from 5 groups to 2 groups, it will be NA for 4 groups and 3 groups because the precision was not small enought.
#' 
#' @return The function returns a vector with the value of type one error for each group
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{mean_comparisons_model_1}}
#' \item \code{\link{mean_comparisons_model_2}}
#' } 
#' 
get.at.least.X.groups = function(
  Mpvalue, 
  MCMC, 
  p.adj = "soft.bonf", 
  precision = 0.0005
  )
{
total_groups = nrow(Mpvalue)
ALP = rep(NA, (total_groups - 1))
names(ALP) = paste(c(2:(length(ALP) + 1)), "_groups", sep = "")

alp = 1 # the highest value of the probability to have X groups
Comparison = get.significant.groups(Mpvalue, MCMC, alpha = alp, p.adj = p.adj)
a = unlist(strsplit(paste(Comparison[, "groups"], collapse = ""), ""))
nb_group = length(unique(a))

if( nb_group == 1) {
  ALP[] = "Imp" 
  go = FALSE
} else {go = TRUE }

while(go) {
	alp = alp - precision
	Comparison = get.significant.groups(Mpvalue, MCMC, alpha = alp, p.adj = p.adj)
	a = unlist(strsplit(paste(Comparison[, "groups"], collapse = ""), ""))
	nb_group = length(unique(a))
	
	if(nb_group == 1) {go = FALSE}

	for(gp in 2:total_groups) { if(nb_group == gp ) { ALP[gp-1] = alp } }	
}

return(ALP)
}
