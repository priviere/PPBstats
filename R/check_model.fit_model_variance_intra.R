check_model.fit_model_variance_intra = function(
  x
)
{
  # 1. Convergence, update MCMC and data frame with environments where some parameters did not converge ----------
  out.conv = check_convergence(x, model_name = "model_variance_intra")
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
    data_ggplot_model_varintra_mu_ijk = droplevels(sq_MCMC[grep("mu", rownames(sq_MCMC)),])
  } else { data_ggplot_model_varintra_mu_ijk = NULL }
  

  # 2.2. sigma caterpillar plot ----------
  if ( length(grep("sigma", rownames(sq_MCMC))) > 0  ) {
    data_ggplot_model_varintra_sigma_ij = droplevels(sq_MCMC[grep("sigma", rownames(sq_MCMC)),])
  } else { data_ggplot_model_varintra_sigma_j_2 = NULL }

  
  # 3. Return results ----------
  out = list(
    "MCMC" = MCMC,
    "MCMC_conv_not_ok" = mcmc_to_delete,
    "data_ggplot" = list(
      "mu_ijk" = data_ggplot_model_varintra_mu_ijk,
      "sigma_ij" = data_ggplot_model_varintra_sigma_ij
    )
  )
  
  class(out) <- c("PPBstats", "check_model_variance_intra")
  
  return(out)
}
