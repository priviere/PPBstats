#' Check if the Hierarchical Bayesian variance intra model went well 
#'
#' @description
#' \code{check_model.fit_model_bh_variance_intra} computes tests to assess if the model went well. 
#' It is important to run this step before going ahead with the analysis otherwise you may make mistakes in the interpretation of the results.
#' 
#' @param x outputs from \code{\link{model_bh_variance_intra}}
#' 
#' @details
#' S3 method.
#' The different test apply to the model are explained in the book \href{https://priviere.github.io/PPBstats_book/intro-agro.html#section-bayes}{here}.
#' 
#' @return It returns a list with the following elements:
#' 
#' \itemize{
#'  \item MCMC : a data fame resulting from the concatenation of the two MCMC for each parameter
#'  \item MCMC_conv_not_ok : a data fame resulting from the concatenation of the two MCMC for each parameter for environment where  some parameters did not converge for mu and beta
#'  \item data_ggplot a list containing information for ggplot:
#'  \itemize{
#'   \item mu
#'   \item sigma
#'   \item epsilon
#'  }
#' }
#' 
#' @author Gaelle Van Frank and Pierre Riviere
#'
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}
#' \item \code{\link{plot.check_model_bh_variance_intra}}
#' \item \code{\link{mean_comparisons}}
#' \item \code{\link{mean_comparisons.check_model_bh_variance_intra}}
#' }
#'
#' @export
#'
check_model.fit_model_bh_variance_intra = function(
  x
)
{
  # 1. Convergence, update MCMC and data frame with environments where some parameters did not converge ----------
  out.conv = check_convergence(x, model_name = "model_bh_variance_intra")
  MCMC = out.conv$MCMC
  sq_MCMC = out.conv$sq_MCMC
  conv_not_ok = out.conv$conv_not_ok
  
  if( length(conv_not_ok) > 0 ) {
    # mu
    mu_not_ok = conv_not_ok[grep("mu\\[", conv_not_ok)]
    # sigma
    sigma_not_ok = conv_not_ok[grep("sigma\\[", conv_not_ok)]
  
    mcmc_to_delete = c(mu_not_ok, sigma_not_ok)
    for (i in mcmc_to_delete){MCMC = MCMC[,names(MCMC)!=i]}

    data_whose_param_did_not_converge=mcmc_to_delete
   } else {   data_whose_param_did_not_converge = NULL }
  
  
  attributes(data_whose_param_did_not_converge)$PPBstats.object = "data_whose_param_did_not_converge"
  
  # 2. posteriors data frame for ggplot ----------
  
  sq_MCMC$entry_mu = sub("mu\\[", "", sapply(sq_MCMC$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
  env_mu = sub("\\]", "", sub("mu\\[", "", sapply(sq_MCMC$parameter[grep("mu\\[", sq_MCMC$parameter)], function(x){unlist(strsplit(as.character(x), ","))[2]})))
  env_sigma = sub("\\]", "", sub("sigma\\[", "", sq_MCMC$parameter[grep("sigma\\[", sq_MCMC$parameter)]))
  
  sq_MCMC$environment =  c(env_mu, env_sigma)
  sq_MCMC$location = sapply(sq_MCMC$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
  sq_MCMC$year = sapply(sq_MCMC$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
  
  
  # 2.1. mu_ijk caterpillar plot ----------
  if ( length(grep("mu", rownames(sq_MCMC))) > 0  ) {
    data_ggplot_model_bh_variance_intra_mu_ijk = droplevels(sq_MCMC[grep("mu", rownames(sq_MCMC)),])
  } else { data_ggplot_model_bh_variance_intra_mu_ijk = NULL }
  

  # 2.2. sigma caterpillar plot ----------
  if ( length(grep("sigma", rownames(sq_MCMC))) > 0  ) {
    data_ggplot_model_bh_variance_intra_sigma_ij = droplevels(sq_MCMC[grep("sigma", rownames(sq_MCMC)),])
  } else { data_ggplot_model_bh_variance_intra_sigma_j_2 = NULL }

  
  # 3. Return results ----------
  out = list(
    "MCMC" = MCMC,
    "MCMC_conv_not_ok" = mcmc_to_delete,
    "data_ggplot" = list(
      "mu_ijk" = data_ggplot_model_bh_variance_intra_mu_ijk,
      "sigma_ij" = data_ggplot_model_bh_variance_intra_sigma_ij
    )
  )
  
  class(out) <- c("PPBstats", "check_model_bh_variance_intra")
  
  return(out)
}
