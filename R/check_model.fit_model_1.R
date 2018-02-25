check_model.fit_model_bh_intra_location <- function(
  x
)
  {
  
  # 1. Convergence, update MCMC and data frame with environments where some parameters did not converge ----------
  out.conv = check_convergence(x, model_name = "model_bh_intra_location")
  MCMC = out.conv$MCMC
  sq_MCMC = out.conv$sq_MCMC
  conv_not_ok = out.conv$conv_not_ok
  
  if( length(conv_not_ok) > 0 ) {

    # mu
    mu_not_ok = conv_not_ok[grep("mu\\[", conv_not_ok)]
    if( length(mu_not_ok) > 0 ) {
      env_not_ok_mu = unique(sub("\\]", "", sub("mu\\[", "", sapply(mu_not_ok, function(x){unlist(strsplit(as.character(x), ","))[2]}))))
    } else { env_not_ok_mu = NULL }
    
    # beta
    beta_not_ok = conv_not_ok[grep("beta\\[", conv_not_ok)]
    if( length(beta_not_ok) > 0 ) {
      env_not_ok_beta = unique(sub("\\]", "", sub("beta\\[", "", sapply(beta_not_ok, function(x){unlist(strsplit(as.character(x), ","))[1]}))))
    } else { env_not_ok_beta = NULL }
    
    # sigma
    sigma_not_ok = conv_not_ok[grep("sigma\\[", conv_not_ok)]
    if( length(sigma_not_ok) > 0 ) {
      env_not_ok_sigma = unique(sub("\\]", "", sub("sigma\\[", "", sigma_not_ok)))
    } else { env_not_ok_sigma = NULL }
    
    # update data
    env_not_ok = unique(c(env_not_ok_mu, env_not_ok_beta, env_not_ok_sigma))
    if( length(env_not_ok) > 0 ) {
      data_env_whose_param_did_not_converge = droplevels(filter(x$data.model1, environment %in% env_not_ok))
      data_env_whose_param_did_not_converge = plyr::rename(data_env_whose_param_did_not_converge, replace = c("variable" = "median"))
      data_env_whose_param_did_not_converge$parameter = paste("mu", data_env_whose_param_did_not_converge$parameter, sep = "")
      data_env_whose_param_did_not_converge$location = unlist(lapply(as.character(data_env_whose_param_did_not_converge$environment),function(x){strsplit(x,":")[[1]][1]}))
      data_env_whose_param_did_not_converge$year = unlist(lapply(as.character(data_env_whose_param_did_not_converge$environment),function(x){strsplit(x,":")[[1]][2]}))
      

    # Update MCMC, delete all environments where at least one parameter do not converge
      message("MCMC are updated, the following environment were deleted : ", paste(env_not_ok, collapse = ", "))
      message("data_env_whose_param_did_not_converge contains the raw data for these environments.")
      m1 = unlist(sapply(paste("sigma\\[", env_not_ok, sep = ""), function(x){grep(x, colnames(MCMC))} ))
      m2 = unlist(sapply(paste("beta\\[", env_not_ok, sep = ""), function(x){grep(x, colnames(MCMC))} ))
      m3 = grep("mu\\[", colnames(MCMC))
      m3 = colnames(MCMC)[m3][unlist(sapply(paste(",", env_not_ok, "]", sep = ""), function(x){grep(x, colnames(MCMC)[m3])} ))]
      m3 = c(1:ncol(MCMC))[is.element(colnames(MCMC), m3)]
      
      mcmc_to_delete = c(m1, m2, m3)
      MCMC = MCMC[,-mcmc_to_delete] 
    } else {   data_env_whose_param_did_not_converge = NULL }
  } else {   
    mcmc_to_delete = NULL
    data_env_whose_param_did_not_converge = NULL 
    }
  
  attributes(data_env_whose_param_did_not_converge)$PPBstats.object = "data_env_whose_param_did_not_converge"
  
  # 2. posteriors data frame for ggplot ----------
  
  sq_MCMC$entry_mu = sub("mu\\[", "", sapply(sq_MCMC$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
  
  env_beta = sub("\\]", "", sub("beta\\[", "", sapply(sq_MCMC$parameter[grep("beta\\[", sq_MCMC$parameter)], function(x){unlist(strsplit(as.character(x), ","))[1]})))
  env_mu = sub("\\]", "", sub("mu\\[", "", sapply(sq_MCMC$parameter[grep("mu\\[", sq_MCMC$parameter)], function(x){unlist(strsplit(as.character(x), ","))[2]})))
  env_nu = env_rho = NA
  env_sigma = sub("\\]", "", sub("sigma\\[", "", sq_MCMC$parameter[grep("sigma\\[", sq_MCMC$parameter)]))
  
  sq_MCMC$environment =  c(env_beta, env_mu, env_nu, env_rho, env_sigma)
  sq_MCMC$location = sapply(sq_MCMC$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
  sq_MCMC$year = sapply(sq_MCMC$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
  
  # 2.1. sigma_j distribution ----------   
  if( length(grep("nu", rownames(sq_MCMC))) > 0 & length(grep("rho", rownames(sq_MCMC))) > 0 & length(grep("sigma", rownames(sq_MCMC))) > 0 ) {
    nu = sq_MCMC["nu", "q3"]
    rho = sq_MCMC["rho", "q3"]
    d_sigma_distribution = cbind.data.frame(sigma_distribution = sqrt(1/rgamma(10000, nu, rho)))
    sigma = sq_MCMC[grep("sigma", sq_MCMC$parameter), "q3"]
    names(sigma) = sq_MCMC$parameter[grep("sigma", sq_MCMC$parameter)]
    sigma = sort(sigma)
    d_sigma = cbind.data.frame(sigma = as.factor(names(sigma)), value = sigma)
    data_ggplot_model_bh_intra_location_sigma_j = list(d_sigma_distribution = d_sigma_distribution, d_sigma = d_sigma)
  } else { data_ggplot_model_bh_intra_location_sigma_j = NULL }
  
  # 2.2. mu_ij caterpillar plot ----------
  if ( length(grep("mu", rownames(sq_MCMC))) > 0  ) {
    data_ggplot_model_bh_intra_location_mu_ij = droplevels(sq_MCMC[grep("mu", rownames(sq_MCMC)),])
  } else { data_ggplot_model_bh_intra_location_mu_ij = NULL }
  
  # 2.3. beta_jk caterpillar plot ----------
  if ( length(grep("beta", rownames(sq_MCMC))) > 0  ) {
    data_ggplot_model_bh_intra_location_beta_jk = droplevels(sq_MCMC[grep("beta", rownames(sq_MCMC)),])   
  } else { data_ggplot_model_bh_intra_location_beta_jk = NULL }
  
  # 2.4. sigma_j caterpillar plot ----------
  if ( length(grep("sigma", rownames(sq_MCMC))) > 0  ) {
    data_ggplot_model_bh_intra_location_sigma_j_2 = droplevels(sq_MCMC[grep("sigma", rownames(sq_MCMC)),])
  } else { data_ggplot_model_bh_intra_location_sigma_j_2 = NULL }
  
  # 2.5. standardized epsilon_ijk distribution ----------
  if ( !is.null(x$epsilon)  ) {
    epsilon_ijk = x$epsilon
    sigma_j = sq_MCMC[grep("sigma", sq_MCMC$parameter), "q3"]
    names(sigma_j) = sq_MCMC$parameter[grep("sigma", sq_MCMC$parameter)]
    env = sub("\\]", "", sapply(names(epsilon_ijk), function(x) { sub("epsilon\\[", "", sapply(x, function(x){unlist(strsplit(as.character(x), ","))[2]})) }))
    sigma_j = sigma_j[paste("sigma[", env, "]", sep="")]
    data_ggplot_model_bh_intra_location_epsilon_ijk = cbind.data.frame(x = c(1:length(sigma_j)), std_res = epsilon_ijk / sigma_j)
  } else { data_ggplot_model_bh_intra_location_epsilon_ijk = NULL }
  

  # 3. Return results ----------
  data_env_with_no_controls = x$data_env_with_no_controls
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
      "sigma_j" = data_ggplot_model_bh_intra_location_sigma_j,
      "mu_ij" = data_ggplot_model_bh_intra_location_mu_ij,
      "beta_jk" = data_ggplot_model_bh_intra_location_beta_jk,
      "sigma_j_2" = data_ggplot_model_bh_intra_location_sigma_j_2,
      "epsilon_ijk" = data_ggplot_model_bh_intra_location_epsilon_ijk
    )
  )
  
  class(out) <- c("PPBstats", "check_model_bh_intra_location")
  
  return(out)
}
