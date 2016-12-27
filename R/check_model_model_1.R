check_model_model_1 = function(
  model = "model_1",
  out.model,
  analysis = NULL,
  nb_parameters_per_plot = 10
){
  # 1. Error message and update arguments ----------
  analysis = check_analysis_argument(analysis)
  
  # Default settings
  model1.data_env_whose_param_did_not_converge = NULL
  
  # 2. Convergence ----------
  out.conv = check_convergence(out.model, model_name = "model2")
  MCMC = out.con$MCMC
  out.convergence = out.conv$out.convergence
  
  # 4. posteriors ----------
  out.posteriors = NULL
  if(analysis == "all" | analysis == "posteriors") {
    
    s = summary(out.model$MCMC)
    sq_MCMC = as.data.frame(s$quantiles)
    sq_MCMC$parameter = as.factor(rownames(sq_MCMC))
    colnames(sq_MCMC) = c("q1", "q2", "q3", "q4", "q5", "parameter")
    
    # 4.1.1. Update MCMC and get data frame with environments where some parameters did not converge ----------
    if(analysis == "all" | analysis == "convergence") {
      if( length(conv_not_ok) > 0 ) {
        
        mu_not_ok = conv_not_ok[grep("mu\\[", conv_not_ok)]
        if( length(mu_not_ok) > 0 ) {
          env_not_ok_mu = unique(sub("\\]", "", sub("mu\\[", "", sapply(mu_not_ok, function(x){unlist(strsplit(as.character(x), ","))[2]}))))
        } else { env_not_ok_mu = NULL }
        
        beta_not_ok = conv_not_ok[grep("beta\\[", conv_not_ok)]
        if( length(beta_not_ok) > 0 ) {
          env_not_ok_beta = unique(sub("\\]", "", sub("beta\\[", "", sapply(beta_not_ok, function(x){unlist(strsplit(as.character(x), ","))[1]}))))
        } else { env_not_ok_beta = NULL }
        
        sigma_not_ok = conv_not_ok[grep("sigma\\[", conv_not_ok)]
        if( length(sigma_not_ok) > 0 ) {
          env_not_ok_sigma = unique(sub("\\]", "", sub("sigma\\[", "", sigma_not_ok)))
        } else { env_not_ok_sigma = NULL }
        
        env_not_ok = unique(c(env_not_ok_mu, env_not_ok_beta, env_not_ok_sigma))
        if( length(env_not_ok) > 0 ) {
          model1.data_env_whose_param_did_not_converge = droplevels(filter(out.model$data.model1, environment %in% env_not_ok))
          attributes(model1.data_env_whose_param_did_not_converge)$PPBstats.object = "model1.data_env_whose_param_did_not_converge"
          
          # Update MCMC, delete all environments where at least one parameter do not converge
          message("MCMC are updated, the following environment were deleted : ", paste(env_not_ok, collapse = ", "))
          message("model1.data_env_whose_param_did_not_converge contains the raw data for these environments.")
          m1 = unlist(sapply(paste("sigma\\[", env_not_ok, sep = ""), function(x){grep(x, colnames(MCMC))} ))
          m2 = unlist(sapply(paste("beta\\[", env_not_ok, sep = ""), function(x){grep(x, colnames(MCMC))} ))
          m3 = grep("mu\\[", colnames(MCMC))
          m3 = colnames(MCMC)[m3][unlist(sapply(paste(",", env_not_ok, "]", sep = ""), function(x){grep(x, colnames(MCMC)[m3])} ))]
          m3 = c(1:ncol(MCMC))[is.element(colnames(MCMC), m3)]
          
          mcmc_to_delete = c(m1, m2, m3)
          MCMC = MCMC[,-mcmc_to_delete] 
          if(attributes(out.model)$PPBstats.object == "model1") { attributes(MCMC)$model = "model1" }
        }
      }
    }
    
    # 4.1.2. Format MCMC for further use ----------
    sq_MCMC$entry_mu = sub("mu\\[", "", sapply(sq_MCMC$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
    
    env_beta = sub("\\]", "", sub("beta\\[", "", sapply(sq_MCMC$parameter[grep("beta\\[", sq_MCMC$parameter)], function(x){unlist(strsplit(as.character(x), ","))[1]})))
    env_mu = sub("\\]", "", sub("mu\\[", "", sapply(sq_MCMC$parameter[grep("mu\\[", sq_MCMC$parameter)], function(x){unlist(strsplit(as.character(x), ","))[2]})))
    env_nu = env_rho = NA
    env_sigma = sub("\\]", "", sub("sigma\\[", "", sq_MCMC$parameter[grep("sigma\\[", sq_MCMC$parameter)]))
    sq_MCMC$environment =  c(env_beta, env_mu, env_nu, env_rho, env_sigma)
    
    sq_MCMC$location = sapply(sq_MCMC$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
    sq_MCMC$year = sapply(sq_MCMC$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
    
    # 4.1.3. sigma_j distribution ----------   
    out_sigma_distribution = NULL
    if( length(grep("nu", rownames(sq_MCMC))) > 0 & length(grep("rho", rownames(sq_MCMC))) > 0 & length(grep("sigma", rownames(sq_MCMC))) > 0 ) {
      nu = sq_MCMC["nu", "q3"]
      rho = sq_MCMC["rho", "q3"]
      d_sigma_distribution = cbind.data.frame(sigma_distribution = sqrt(1/rgamma(10000, nu, rho)))
      
      p = ggplot()
      p = p + geom_density(data = d_sigma_distribution, aes(x = sigma_distribution) )
      
      sigma = sq_MCMC[grep("sigma", sq_MCMC$parameter), "q3"]; names(sigma) = sq_MCMC$parameter[grep("sigma", sq_MCMC$parameter)]
      sigma = sort(sigma)
      d_sigma = cbind.data.frame(sigma = as.factor(names(sigma)), value = sigma)
      
      p.tmp = p + geom_vline(data = d_sigma, aes(xintercept = value, color = sigma))
      out = list(p.tmp)
      
      SEQ = unique(c(seq(0, nrow(d_sigma), 5), nrow(d_sigma)))
      for(s in 1:(length(SEQ) - 1)) {
        d_sigma_tmp = d_sigma[c((SEQ[s]+1):SEQ[s+1]),]
        p.tmp = p + geom_vline(data = d_sigma_tmp, aes(xintercept = value, color = sigma), show.legend = TRUE)
        out = c(out, list(p.tmp))
      }
      out_sigma_distribution = out
      message("The values of sigma in the inverse Gamme distribution are done.")
    }
    
    # 4.1.4. mu_ij, beta_jk and sigma_j caterpillar plot distribution ----------
    out_para_posteriors = NULL
    
    if ( length(grep("mu", rownames(sq_MCMC))) > 0  ) {
      sq_MCMC_mu = droplevels(sq_MCMC[grep("mu", rownames(sq_MCMC)),])
      xmin = min(sq_MCMC_mu$q1); xmax = max(sq_MCMC_mu$q5)
      
      sq_MCMC_mu_env = plyr:::splitter_d(sq_MCMC_mu, .(environment))
      out = lapply(sq_MCMC_mu_env, function(x){ get.caterpillar.plot(x) }) # + xlim(xmin, xmax)
      out = list("mu_posteriors" = out)
      out_para_posteriors = c(out_para_posteriors, out)
      message("The mu_ij posterior distributions are done.")
    }
    
    if ( length(grep("beta", rownames(sq_MCMC))) > 0  ) {
      sq_MCMC_beta = droplevels(sq_MCMC[grep("beta", rownames(sq_MCMC)),])   
      xmin = min(sq_MCMC_beta$q1); xmax = max(sq_MCMC_beta$q5)
      
      sq_MCMC_beta_env = plyr:::splitter_d(sq_MCMC_beta, .(environment))
      out = lapply(sq_MCMC_beta_env, function(x){ get.caterpillar.plot(x) }) # + xlim(xmin, xmax)
      out = list("beta_posteriors" = out)
      out_para_posteriors = c(out_para_posteriors, out)
      message("The beta_jk posterior distributions are done.")      
    }
    
    if ( length(grep("sigma", rownames(sq_MCMC))) > 0  ) {
      sq_MCMC_sigma = droplevels(sq_MCMC[grep("sigma", rownames(sq_MCMC)),])    
      xmin = min(sq_MCMC_sigma$q1); xmax = max(sq_MCMC_sigma$q5)
      
      sq_MCMC_sigma$split = add_split_col(sq_MCMC_sigma, nb_parameters_per_plot)
      sq_MCMC_sigma_split = plyr:::splitter_d(sq_MCMC_sigma, .(split))
      
      out = lapply(sq_MCMC_sigma_split, function(x){ get.caterpillar.plot(x) + ggtitle("") } ) # + xlim(xmin, xmax) 
      out = list("sigma_posteriors" = out)
      out_para_posteriors = c(out_para_posteriors, out)
      message("The sigma_j posterior distributions are done.")
    }
    
    # 4.1.5. standardized epsilon_ijk distribution ----------
    out_stand_res = NULL
    
    if ( !is.null(out.model$epsilon)  ) {      
      epsilon_ijk = out.model$epsilon
      
      sigma_j = sq_MCMC[grep("sigma", sq_MCMC$parameter), "q3"]
      names(sigma_j) = sq_MCMC$parameter[grep("sigma", sq_MCMC$parameter)]
      
      env = sub("\\]", "", sapply(names(epsilon_ijk), function(x) { sub("epsilon\\[", "", sapply(x, function(x){unlist(strsplit(as.character(x), ","))[2]})) }))
      sigma_j = sigma_j[paste("sigma[", env, "]", sep="")]
      
      d_std_res = cbind.data.frame(x = c(1:length(sigma_j)), std_res = epsilon_ijk / sigma_j)
      out_stand_res = ggplot(d_std_res, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
      message("The standardised residuals distributions are done.")
    }
    
    out.posteriors = list("sigma_distribution" = out_sigma_distribution, "parameter_posteriors" = out_para_posteriors, "standardized_residuals" = out_stand_res)
  }
  
  # 5. Return outptus ----------
  out = list("convergence" = out.convergence, 
             "posteriors" = out.posteriors, 
             "MCMC" = MCMC,
             "model1.data_env_whose_param_did_not_converge" = model1.data_env_whose_param_did_not_converge)
  return(out)
  
}
