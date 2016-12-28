ggplot_check_model_model_2 = function(
  out_check_model,
  nb_parameters_per_plot = 10
  ){
  # Get data ----------
  
    data_ggplot = out_check_model$data_ggplot
    data_ggplot_model_2_alpha = data_ggplot$alpha
    data_ggplot_model_2_beta = data_ggplot$beta
    data_ggplot_model_2_theta = data_ggplot$theta
    data_ggplot_model_2_epsilon = data_ggplot$epsilon
    
    # 2.1. alpha_i caterpillar plot distribution ----------
    if ( !is.null(data_ggplot_model_2_alpha) ) {
      xmin = min(data_ggplot_model_2_alpha$q1)
      xmax = max(data_ggplot_model_2_alpha$q5)
      
      data_ggplot_model_2_alpha$split = add_split_col(data_ggplot_model_2_alpha, each = nb_parameters_per_plot)
      data_ggplot_model_2_alpha_split = plyr:::splitter_d(data_ggplot_model_2_alpha, .(split))      
      
      out_alpha = lapply(data_ggplot_model_2_alpha_split, function(x){ get.caterpillar.plot(x) } ) # + xlim(xmin, xmax) 
    }
    
    
    # 2.2. beta_i caterpillar plot distribution ----------
    if ( !is.null(data_ggplot_model_2_beta) ) {
      xmin = min(data_ggplot_model_2_beta$q1)
      xmax = max(data_ggplot_model_2_beta$q5)
      
      data_ggplot_model_2_beta$split = add_split_col(data_ggplot_model_2_beta, each = nb_parameters_per_plot)
      data_ggplot_model_2_beta_split = plyr:::splitter_d(data_ggplot_model_2_beta, .(split))      
      
      out_beta = lapply(data_ggplot_model_2_beta_split, function(x){ get.caterpillar.plot(x) } ) # + xlim(xmin, xmax) 
      message("The beta_i posterior distributions are done.")      
    }
    
    
    # 2.3. theta_j caterpillar plot distribution ----------
    if ( !is.null(data_ggplot_model_2_theta) ) {
      xmin = min(data_ggplot_model_2_theta$q1)
      xmax = max(data_ggplot_model_2_theta$q5)
      
      data_ggplot_model_2_theta$split = add_split_col(data_ggplot_model_2_theta, each = nb_parameters_per_plot)
      data_ggplot_model_2_theta_split = plyr:::splitter_d(data_ggplot_model_2_theta, .(split))      
      
      out_theta = lapply(data_ggplot_model_2_theta_split, function(x){ get.caterpillar.plot(x) } ) # + xlim(xmin, xmax)
      message("The theta_j posterior distributions are done.")
    }
    
    if ( !is.null(data_ggplot_model_2_epsilon) ) {
      out_epsilon = ggplot(data_ggplot_model_2_epsilon, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
      message("The standardised residuals distributions are done.")
    }
    
    
    out_model_2 = list(
      "alpha" = out_alpha,
      "beta" = out_beta,
      "theta" = out_theta,
      "epsilon" = out_epsilon
    )
    
    return(out_model_2)
  
}