ggplot_check_model = function(
  out_check_model,
  nb_parameters_per_plot = 10
  ){
  
  get.caterpillar.plot = function(x){ # cf ggmcmc:ggs_caterpillar
    p = ggplot(x, aes(x = q3, y = reorder(parameter, q3))) 
    p = p + geom_point(size = 3) # median 
    p = p + geom_segment(aes(x = q2, xend = q4, yend = reorder(parameter, q3)), size = 1.5) # 25%-75%
    p = p + geom_segment(aes(x = q1, xend = q5, yend = reorder(parameter, q3)), size = 0.5) # 2.5%-25% and 75%-97.5%
    p = p + ylab("parameter") + xlab("value") + ggtitle(x[1, "environment"])
    return(p)
  }
  
  # 1. model 1 ----------
  if( attributes(out_check_model)$check_model == "model_1" ) {
    data_ggplot = out_check_model$data_ggplot
    data_ggplot_model_1_sigma_j = data_ggplot$sigma_j
    data_ggplot_model_1_mu_ij = data_ggplot$mu_ij
    data_ggplot_model_1_beta_jk = data_ggplot$beta_jk
    data_ggplot_model_1_sigma_j_2 = data_ggplot$sigma_j_2
    data_ggplot_model_1_epsilon_ijk = data_ggplot$epsilon_ijk
    
    # sigma_j ----------
    if( !is.null(data_ggplot_model_1_sigma_j)){
      d_sigma_distribution = data_ggplot_model_1_sigma_j$d_sigma_distribution
      d_sigma = data_ggplot_model_1_sigma_j$d_sigma
      
      p = ggplot()
      p = p + geom_density(data = d_sigma_distribution, aes(x = sigma_distribution) )
      p.tmp = p + geom_vline(data = d_sigma, aes(xintercept = value, color = sigma))
      out = list(p.tmp)
      
      SEQ = unique(c(seq(0, nrow(d_sigma), 5), nrow(d_sigma)))
      for(s in 1:(length(SEQ) - 1)) {
        d_sigma_tmp = d_sigma[c((SEQ[s]+1):SEQ[s+1]),]
        p.tmp = p + geom_vline(data = d_sigma_tmp, aes(xintercept = value, color = sigma), show.legend = TRUE)
        out = c(out, list(p.tmp))
      }
      out_sigma_j_gamma = out
      message("Distribution of sigma_j in the inverse Gamme distribution are done.")
    } else { out_sigma_j_gamma = NULL }
    
    # mu_ij caterpillar plot ----------
    if(!is.null(data_ggplot_model_1_mu_ij)){
      xmin = min(data_ggplot_model_1_mu_ij$q1); xmax = max(data_ggplot_model_1_mu_ij$q5)
      data_ggplot_model_1_mu_ij = plyr:::splitter_d(data_ggplot_model_1_mu_ij, .(environment))
      out = lapply(data_ggplot_model_1_mu_ij, function(x){ get.caterpillar.plot(x) }) # + xlim(xmin, xmax)
      out_mu_ij = list("mu_posteriors" = out)
      message("The mu_ij posterior distributions are done.")
    } else { out_mu_ij = NULL }
    
    # beta_jk caterpillar plot ----------
    if(!is.null(data_ggplot_model_1_beta_jk)){
      xmin = min(data_ggplot_model_1_beta_jk$q1); xmax = max(data_ggplot_model_1_beta_jk$q5)
      data_ggplot_model_1_beta_jk = plyr:::splitter_d(data_ggplot_model_1_beta_jk, .(environment))
      out = lapply(data_ggplot_model_1_beta_jk, function(x){ get.caterpillar.plot(x) }) # + xlim(xmin, xmax)
      out_beta_jk = list("beta_posteriors" = out)
      message("The beta_jk posterior distributions are done.")      
    } else { out_beta_jk = NULL }
    
    # sigma_j caterpillar plot ----------
    if(!is.null(data_ggplot_model_1_sigma_j_2)){
      xmin = min(data_ggplot_model_1_sigma_j_2$q1); xmax = max(data_ggplot_model_1_sigma_j_2$q5)
      
      data_ggplot_model_1_sigma_j_2$split = add_split_col(data_ggplot_model_1_sigma_j_2, nb_parameters_per_plot)
      data_ggplot_model_1_sigma_j_2 = plyr:::splitter_d(data_ggplot_model_1_sigma_j_2, .(split))
      
      out = lapply(data_ggplot_model_1_sigma_j, function(x){ get.caterpillar.plot(x) + ggtitle("") } ) # + xlim(xmin, xmax) 
      out_sigma_j = list("sigma_posteriors" = out)
      message("The sigma_j posterior distributions are done.")
    } else { out_sigma_j = NULL }
    
    # standardized epsilon_ijk distribution ----------
    if(!is.null(data_ggplot_model_1_epsilon_ijk)){
      out_epsilon_ijk = ggplot(data_ggplot_model_1_epsilon_ijk, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
      message("The standardised residuals distributions are done.")
    } else { out_epsilon_ijk = NULL }
  
    out_model_1 = list(
      out_sigma_j_gamma,
      out_mu_ij,
      out_beta_jk,
      out_sigma_j,
      out_epsilon_ijk
    )
      
  }
  
  
  
}