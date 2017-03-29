#' Check if the model_variance_intra model went well 
#'
#' @description
#' \code{check_model_variance_intra} computes tests to assess if the model_variance_intra model went well
#' 
#' @param out_model_variance_intra outputs from \code{\link{model_variance_intra}}
#' 
#' @details See \code{\link{check_model}}
#' 
#' @return See \code{\link{check_model}}
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{model_variance_intra}}, 
#' \item \code{\link{check_model}}
#' }
#' 
check_model_model_variance_intra = function(
  out_model_varintra
)
{
  
  # 1. Convergence, update MCMC and data frame with environments where some parameters did not converge ----------
  out.conv = check_convergence(out_model_varintra, model_name = "model_varintra")
  MCMC = out.conv$MCMC
  sq_MCMC = out.conv$sq_MCMC
  conv_not_ok = out.conv$conv_not_ok
  
  if( length(conv_not_ok) > 0 ) {
    
    # mu
    mu_not_ok = conv_not_ok[grep("mu\\[", conv_not_ok)]
    if( length(mu_not_ok) > 0 ) {
      env_not_ok_mu = unique(sub("\\]", "", sub("mu\\[", "", sapply(mu_not_ok, function(x){unlist(strsplit(as.character(x), ","))[2]}))))
    } else { env_not_ok_mu = NULL }
 
    # sigma
    sigma_not_ok = conv_not_ok[grep("sigma\\[", conv_not_ok)]
    if( length(sigma_not_ok) > 0 ) {
      env_not_ok_sigma = unique(sub("\\]", "", sub("sigma\\[", "", sigma_not_ok)))
    } else { env_not_ok_sigma = NULL }
    
    # update data
    env_not_ok = unique(c(env_not_ok_mu, env_not_ok_sigma))
    if( length(env_not_ok) > 0 ) {
      data_env_whose_param_did_not_converge = droplevels(filter(out_model_varintra$data.modelvarIntra, environment %in% env_not_ok))
      data_env_whose_param_did_not_converge = plyr::rename(data_env_whose_param_did_not_converge, replace = c("variable" = "median"))
      data_env_whose_param_did_not_converge$parameter = paste("mu", data_env_whose_param_did_not_converge$parameter, sep = "")
      
      # Update MCMC, delete all environments where at least one parameter do not converge
      message("MCMC are updated, the following environment were deleted : ", paste(env_not_ok, collapse = ", "))
      message("data_env_whose_param_did_not_converge contains the raw data for these environments.")
      m1 = unlist(sapply(paste("sigma\\[", env_not_ok, sep = ""), function(x){grep(x, colnames(MCMC))} ))
      m3 = grep("mu\\[", colnames(MCMC))
      m3 = colnames(MCMC)[m3][unlist(sapply(paste(",", env_not_ok, "]", sep = ""), function(x){grep(x, colnames(MCMC)[m3])} ))]
      m3 = c(1:ncol(MCMC))[is.element(colnames(MCMC), m3)]
      
      mcmc_to_delete = c(m1, m3)
      MCMC = MCMC[,-mcmc_to_delete] 
    } else {   data_env_whose_param_did_not_converge = NULL }
  } else {   
    mcmc_to_delete = NULL
    data_env_whose_param_did_not_converge = NULL 
  }
  
  attributes(data_env_whose_param_did_not_converge)$PPBstats.object = "data_env_whose_param_did_not_converge"
  
  # 2. posteriors data frame for ggplot ----------
  
  sq_MCMC$entry_mu = sub("mu\\[", "", sapply(sq_MCMC$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
  env_mu = sub("\\]", "", sub("mu\\[", "", sapply(sq_MCMC$parameter[grep("mu\\[", sq_MCMC$parameter)], function(x){unlist(strsplit(as.character(x), ","))[2]})))
  env_sigma = sub("\\]", "", sub("sigma\\[", "", sq_MCMC$parameter[grep("sigma\\[", sq_MCMC$parameter)]))
  
  sq_MCMC$environment =  c(env_mu, env_sigma)
  sq_MCMC$location = sapply(sq_MCMC$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
  sq_MCMC$year = sapply(sq_MCMC$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
  
  
  # 2.1. mu_ij caterpillar plot ----------
  if ( length(grep("mu", rownames(sq_MCMC))) > 0  ) {
    data_ggplot_model_varintra_mu_ij = droplevels(sq_MCMC[grep("mu", rownames(sq_MCMC)),])
  } else { data_ggplot_model_varintra_mu_ij = NULL }
  

  # 2.2. sigma caterpillar plot ----------
  if ( length(grep("sigma", rownames(sq_MCMC))) > 0  ) {
    data_ggplot_model_varintra_sigma_ij = droplevels(sq_MCMC[grep("sigma", rownames(sq_MCMC)),])
  } else { data_ggplot_model_varintra_sigma_j_2 = NULL }

  
  # 3. Return results ----------
  data_env_with_no_controls = out_model_varintra$data_env_with_no_controls
  if( !is.null(data_env_with_no_controls) ){
    data_env_with_no_controls$parameter = paste("mu", data_env_with_no_controls$parameter, sep = "")
    data_env_with_no_controls = plyr::rename(data_env_with_no_controls, replace = c("variable" = "median"))
  }
  attributes(data_env_with_no_controls)$PPBstats.object = "data_env_with_no_controls"
  
  out = list(
    "MCMC" = MCMC,
    "MCMC_conv_not_ok" = mcmc_to_delete,
    "data_env_with_no_controls" = data_env_with_no_controls,
    "data_env_whose_param_did_not_converge" = data_env_whose_param_did_not_converge,
    "data_ggplot" = list(
      "mu_ij" = data_ggplot_model_varintra_mu_ij,
      "sigma_ij" = data_ggplot_model_varintra_sigma_ij
    )
  )
  
  attributes(out)$PPBstats.object = "check_model_model_varintra"
  
  return(out)
}
