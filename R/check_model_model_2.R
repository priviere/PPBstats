check_model_model_2 = function(
  model = "model_2",
  out.model,
  analysis = NULL,
  nb_parameters_per_plot = 10
){
  # 1. Error message and update arguments ----------
  analysis = check_analysis_argument(analysis)
  
  # Default settings
  model2.presence.abscence.matrix = out.model$model2.presence.abscence.matrix
  
  # 2. Convergence, update MCMC when parameters did not converge ----------
  out.convergence = check_convergence(out.model, model_name = "model2")
  MCMC = out.con$MCMC
  sq_MCMC = out.conv$sq_MCMC
  out.convergence = out.conv$out.convergence
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
    
    mat = out.model$model2.presence.abscence.matrix
    if( !is.null(germ_not_ok) ) { mat = mat[!is.element(rownames(mat), germ_not_ok),] }
    if( !is.null(env_not_ok) ) { mat = mat[,!is.element(colnames(mat), env_not_ok)] }
    model2.presence.abscence.matrix = mat
    
    # update MCMC
    MCMC = MCMC[,!is.element(colnames(MCMC), conv_not_ok)] 
    attributes(MCMC)$model = "model2"
  }
  
  # 4. posteriors ----------
  
  out.posteriors = NULL
  if(analysis == "all" | analysis == "posteriors") {
    
    if(attributes(out.model)$PPBstats.object == "model2") {
      
      # 4.2.1. Update MCMC and model2.presence.abscence.matrix ----------
      if(analysis == "all" | analysis == "convergence") {

      }
      
      # 4.2.2. alpha_i, beta_i, theta_j caterpillar plot distribution ----------
      out_para_posteriors = NULL
      
      if ( length(grep("alpha\\[", rownames(sq_MCMC))) > 0  ) {      
        sq_MCMC_alpha = droplevels(sq_MCMC[grep("alpha\\[", rownames(sq_MCMC)),]) 
        xmin = min(sq_MCMC_alpha$q1); xmax = max(sq_MCMC_alpha$q5)
        
        sq_MCMC_alpha$split = add_split_col(sq_MCMC_alpha, each = nb_parameters_per_plot)
        sq_MCMC_alpha_split = plyr:::splitter_d(sq_MCMC_alpha, .(split))      
        
        out = lapply(sq_MCMC_alpha_split, function(x){ get.caterpillar.plot(x) } ) # + xlim(xmin, xmax) 
        out = list("alpha_posteriors" = out)
        out_para_posteriors = c(out_para_posteriors, out)
        message("The alpha_i posterior distributions are done.")
      }
      
      if ( length(grep("beta\\[", rownames(sq_MCMC))) > 0  ) {
        sq_MCMC_beta = droplevels(sq_MCMC[grep("beta\\[", rownames(sq_MCMC)),])    
        xmin = min(sq_MCMC_beta$q1); xmax = max(sq_MCMC_beta$q5)
        
        sq_MCMC_beta$split = add_split_col(sq_MCMC_beta, each = nb_parameters_per_plot)
        sq_MCMC_beta_split = plyr:::splitter_d(sq_MCMC_beta, .(split))      
        
        out = lapply(sq_MCMC_beta_split, function(x){ get.caterpillar.plot(x) } ) # + xlim(xmin, xmax) 
        out = list("beta_posteriors" = out)
        out_para_posteriors = c(out_para_posteriors, out)
        message("The beta_i posterior distributions are done.")      
      }
      
      if ( length(grep("theta\\[", rownames(sq_MCMC))) > 0  ) {
        sq_MCMC_theta = droplevels(sq_MCMC[grep("theta\\[", rownames(sq_MCMC)),])    
        xmin = min(sq_MCMC_theta$q1); xmax = max(sq_MCMC_theta$q5)
        
        sq_MCMC_theta$split = add_split_col(sq_MCMC_theta, each = nb_parameters_per_plot)
        sq_MCMC_theta_split = plyr:::splitter_d(sq_MCMC_theta, .(split))      
        
        out = lapply(sq_MCMC_theta_split, function(x){ get.caterpillar.plot(x) } ) # + xlim(xmin, xmax)
        out = list("theta_posteriors" = out)
        out_para_posteriors = c(out_para_posteriors, out)
        message("The theta_j posterior distributions are done.")
      }
      
      # 4.2.3. standardized epsilon_ijk distribution ----------
      out_stand_res = NULL
      
      if ( !is.null(out.model$epsilon)  ) {      
        
        epsilon_ijk = out.model$epsilon     
        sigma_epsilon = sq_MCMC[grep("sigma_epsilon", sq_MCMC$parameter), "q3"]
        std_res = epsilon_ijk / sigma_epsilon
        
        d_std_res = cbind.data.frame(x = c(1:length(epsilon_ijk)), std_res)
        out_stand_res = ggplot(d_std_res, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
        message("The standardised residuals distributions are done.")
      }
      
      out.posteriors = list("parameter_posteriors" = out_para_posteriors, "standardized_residuals" = out_stand_res)    
    }
    
  }
  # 5. Return outptus ----------
  out = list("model2.presence.abscence.matrix" = model2.presence.abscence.matrix,
             "convergence" = out.convergence, 
             "posteriors" = out.posteriors, 
             "MCMC" = MCMC)
  return(out)
  
}
