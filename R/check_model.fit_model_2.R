check_model.fit_model_2 <- function(
  x
) {
  # Default settings
  model2.presence.absence.matrix = x$model2.presence.absence.matrix

  # 1. Convergence, update MCMC when parameters did not converge ----------
  out.conv = check_convergence(x, model_name = "model2")
  MCMC = out.conv$MCMC
  sq_MCMC = out.conv$sq_MCMC
  conv_not_ok = out.conv$conv_not_ok
  
  if( length(conv_not_ok) > 0 ) {
    
    # alpha
    alpha_not_ok = conv_not_ok[grep("alpha\\[", conv_not_ok)]
    if( length(alpha_not_ok) > 0 ) {
      germ_not_ok_alpha = sub("\\]", "", sub("alpha\\[", "", alpha_not_ok))
    } else { germ_not_ok_alpha = NULL }
    
    # beta
    beta_not_ok = conv_not_ok[grep("beta\\[", conv_not_ok)]
    if( length(beta_not_ok) > 0 ) {
      germ_not_ok_beta = sub("\\]", "", sub("beta\\[", "", beta_not_ok))
    } else { germ_not_ok_beta = NULL }
    
    # theta
    theta_not_ok = conv_not_ok[grep("theta\\[", conv_not_ok)]
    if( length(theta_not_ok) > 0 ) {
      env_not_ok = sub("\\]", "", sub("theta\\[", "", theta_not_ok))
    } else { env_not_ok = NULL }
    
    germ_not_ok = unique(c(germ_not_ok_alpha, germ_not_ok_beta))
    
    mat = x$model2.presence.absence.matrix
    if( !is.null(germ_not_ok) ) { mat = mat[!is.element(rownames(mat), germ_not_ok),] }
    if( !is.null(env_not_ok) ) { mat = mat[,!is.element(colnames(mat), env_not_ok)] }
    model2.presence.absence.matrix = mat
    
    # update MCMC
    mcmc_to_delete = MCMC[,!is.element(colnames(MCMC), conv_not_ok)]
    MCMC = MCMC[,!is.element(colnames(MCMC), conv_not_ok)]
    attributes(MCMC)$model = "model2"
  } else { 
    mcmc_to_delete = NULL
    }
  
  # 2. posteriors data frame for ggplot ----------
  
  # 2.1. alpha_i caterpillar plot distribution ----------
  if ( length(grep("alpha\\[", rownames(sq_MCMC))) > 0  ) {      
    data_ggplot_model_2_alpha = droplevels(sq_MCMC[grep("alpha\\[", rownames(sq_MCMC)),]) 
  } else { data_ggplot_model_2_alpha = NULL }
  
  # 2.2. beta_i caterpillar plot distribution ----------
  if ( length(grep("beta\\[", rownames(sq_MCMC))) > 0  ) {
    data_ggplot_model_2_beta = droplevels(sq_MCMC[grep("beta\\[", rownames(sq_MCMC)),])    
  } else { data_ggplot_model_2_beta = NULL }
  
  # 2.3. theta_j caterpillar plot distribution ----------
  if ( length(grep("theta\\[", rownames(sq_MCMC))) > 0  ) {
    data_ggplot_model_2_theta = droplevels(sq_MCMC[grep("theta\\[", rownames(sq_MCMC)),])    
  } else { data_ggplot_model_2_theta = NULL }
  
  # 2.4. standardized epsilon_ijk distribution ----------
  if ( !is.null(x$epsilon)  ) {      
    epsilon_ijk = x$epsilon     
    sigma_epsilon = sq_MCMC[grep("sigma_epsilon", sq_MCMC$parameter), "q3"]
    std_res = epsilon_ijk / sigma_epsilon
    data_ggplot_model_2_epsilon = cbind.data.frame(x = c(1:length(epsilon_ijk)), std_res)
  } else { data_ggplot_model_2_epsilon = NULL }
  
  
  
  # 3. Return results ----------
  out = list(
    "MCMC" = MCMC,
    "MCMC_conv_not_ok" = mcmc_to_delete,
    "model2.presence.absence.matrix" = model2.presence.absence.matrix,
    "data_ggplot" = list(
      "alpha" = data_ggplot_model_2_alpha,
      "beta" = data_ggplot_model_2_beta,
      "theta" = data_ggplot_model_2_theta,
      "epsilon" = data_ggplot_model_2_epsilon
    )
  )
  
  class(out) <- c("PPBstats", "check_model_2")
  
  return(out)
}
