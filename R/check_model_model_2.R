check_model_model_2 = function(
  model = "model_2",
  out.model,
  analysis = NULL,
  nb_parameters_per_plot = 10
){
  # 1. Error message and update arguments ----------
  if( is.null(attributes(out.model)$PPBstats.object) ) { stop("out.model should be an output from model 2 (PPBstats::model_2).") } 
  
  if(!is.null(analysis)) { 
    if( !is.element(analysis, c("experimental_design", "convergence", "posteriors")) ){ stop("analysis must be \"experimental_design\", \"convergence\" or \"posteriors\".") }  
    if( !is.element(analysis, c("convergence")) ){ warning("\"convergence\" is not chosen! You may make mistakes in the interpretation of the results !!!") }  
  } else { analysis = "all" }
  
  # Default settings
  model2.presence.abscence.matrix = out.model$model2.presence.abscence.matrix
  
  # 1. experimental design ----------
  out.experimental.design = NULL
  if(analysis == "all" | analysis == "experimental_design") {
    
    m = out.model$data.presence.abscence.matrix
    
    if(attributes(out.model)$PPBstats.object == "model2"){
      d <- data.frame(germplasm = rep(row.names(m), ncol(m)), 
                      environment = rep(colnames(m), each=nrow(m)),
                      score = as.factor(as.character(as.vector(m)))
      )
      
    }
    
    nb_NA = round(length(which(d$score == 0)) / ( length(which(d$score == 0)) + length(which(d$score != 0)) ), 2) * 100
    p = ggplot(d, aes(x = germplasm, y = environment))  
    p = p + geom_raster(aes(fill = score)) + ggtitle(paste("GxE combinaisons (",  nb_NA, "% of 0)", sep = ""))
    out.experimental.design = list("plot" = p, "data.presence.abscence.matrix" = m)
    message("The experimental design plot is done.")
  }
  
  # 2. Get MCMC data frame ----------
  MCMC = out.model$MCMC
  MCMC = rbind.data.frame(MCMC[[1]], MCMC[[2]])
  attributes(MCMC)$model = "model2"
  
  # 3. convergence ----------
  out.convergence = NULL
  if(analysis == "all" | analysis == "convergence") {
    message("The Gelman-Rubin test is running for each parameter ...")
    test = gelman.diag(out.model$MCMC, multivariate = FALSE)$psrf[,1]
    conv_ok = names(which(test < 1.05))
    conv_not_ok = names(which(test > 1.05))
    
    if( length(conv_not_ok) > 0 ) {
      message("The two MCMC of the following parameters do not converge thanks to the Gelman-Rubin test : ", paste(conv_not_ok, collapse = ", ") ,". Therefore, they are not present in MCMC output.")
      mcmc = MCMC[,is.element(colnames(MCMC), conv_not_ok)]
      
      out.convergence = NULL
      for (para in conv_not_ok) {
        
        D = cbind.data.frame(Iteration = rep(c(1:(nrow(MCMC)/2)), 2), 
                             Chain = factor(rep(c(1,2), each = (nrow(MCMC)/2))), 
                             Parameter = para, 
                             value = as.vector(MCMC[,para])
        )
        
        traceplot = ggplot(D, aes(x = Iteration, y = value, color = Chain)) + geom_line() + ggtitle(para) # cf ggmcmc:ggs_traceplot
        density = ggplot(D, aes(x = value, fill = Chain, color = Chain)) + geom_density() + ggtitle(para) # cf ggmcmc:ggs_density
        plot = list(list("traceplot" = traceplot, "density" = density))
        names(plot) = para
        out.convergence = c(out.convergence, plot)
      }
    } else { message("The two MCMC for each parameter converge thanks to the Gelman-Rubin test."); out.convergence = NULL }
  }
  
  # 4. posteriors ----------
  
  out.posteriors = NULL
  if(analysis == "all" | analysis == "posteriors") {
    
    s = summary(out.model$MCMC)
    sq_MCMC = as.data.frame(s$quantiles)
    sq_MCMC$parameter = as.factor(rownames(sq_MCMC))
    colnames(sq_MCMC) = c("q1", "q2", "q3", "q4", "q5", "parameter")
    
    if(attributes(out.model)$PPBstats.object == "model2") {
      
      # 4.2.1. Update MCMC and model2.presence.abscence.matrix ----------
      if(analysis == "all" | analysis == "convergence") {
        if( length(conv_not_ok) > 0 ) {
          
          MCMC = MCMC[,!is.element(colnames(MCMC), conv_not_ok)] 
          if(attributes(out.model)$PPBstats.object == "model2") { attributes(MCMC)$model = "model2" }
          
          alpha_not_ok = conv_not_ok[grep("alpha\\[", conv_not_ok)]
          if( length(alpha_not_ok) > 0 ) {
            germ_not_ok_alpha = sub("\\]", "", sub("alpha\\[", "", alpha_not_ok))
          } else { germ_not_ok_alpha = NULL }
          
          beta_not_ok = conv_not_ok[grep("beta\\[", conv_not_ok)]
          if( length(beta_not_ok) > 0 ) {
            germ_not_ok_beta = sub("\\]", "", sub("beta\\[", "", beta_not_ok))
          } else { germ_not_ok_beta = NULL }
          
          theta_not_ok = conv_not_ok[grep("theta\\[", conv_not_ok)]
          if( length(theta_not_ok) > 0 ) {
            env_not_ok = sub("\\]", "", sub("theta\\[", "", theta_not_ok))
          } else { env_not_ok = NULL }
          
          germ_not_ok = unique(c(germ_not_ok_alpha, germ_not_ok_beta))
          
          mat = out.model$model2.presence.abscence.matrix
          if( !is.null(germ_not_ok) ) { mat = mat[!is.element(rownames(mat), germ_not_ok),] }
          if( !is.null(env_not_ok) ) { mat = mat[,!is.element(colnames(mat), env_not_ok)] }
          model2.presence.abscence.matrix = mat
        }
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
  out = list("data.experimental_design" = out.experimental.design,
             "model2.presence.abscence.matrix" = model2.presence.abscence.matrix,
             "convergence" = out.convergence, 
             "posteriors" = out.posteriors, 
             "MCMC" = MCMC,
  return(out)
  
}
