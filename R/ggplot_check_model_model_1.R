ggplot_check_model_model_1 = function(
  out_check_model,
  nb_parameters_per_plot = 10
){
  # Get data ----------
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
    xmin = min(data_ggplot_model_1_mu_ij$q1)
    xmax = max(data_ggplot_model_1_mu_ij$q5)
    
    data_ggplot_model_1_mu_ij = plyr:::splitter_d(data_ggplot_model_1_mu_ij, .(environment))
    
    fun1 = function(x, xmin, xmax){ get.caterpillar.plot(x) + coord_cartesian(xlim = c(xmin, xmax)) }
    
    fun2 = function(x, nb_parameters_per_plot, xmin, xmax){ 
      x$split = add_split_col(x, nb_parameters_per_plot)
      xx = plyr:::splitter_d(x, .(split))
      out = lapply(xx, fun1, xmin, xmax)
      return(out)
    }
    
    out_mu_ij = lapply(data_ggplot_model_1_mu_ij, fun2, nb_parameters_per_plot, xmin, xmax)
    
    message("The mu_ij posterior distributions are done.")
  } else { out_mu_ij = NULL }
  
  # beta_jk caterpillar plot ----------
  if(!is.null(data_ggplot_model_1_beta_jk)){
    xmin = min(data_ggplot_model_1_beta_jk$q1)
    xmax = max(data_ggplot_model_1_beta_jk$q5)
    
    data_ggplot_model_1_beta_jk = plyr:::splitter_d(data_ggplot_model_1_beta_jk, .(environment))
    
    out_beta_jk = lapply(data_ggplot_model_1_beta_jk, fun2, nb_parameters_per_plot, xmin, xmax)
    message("The beta_jk posterior distributions are done.")      
  } else { out_beta_jk = NULL }
  
  # sigma_j caterpillar plot ----------
  if(!is.null(data_ggplot_model_1_sigma_j_2)){
    xmin = min(data_ggplot_model_1_sigma_j_2$q1); xmax = max(data_ggplot_model_1_sigma_j_2$q5)
    
    data_ggplot_model_1_sigma_j_2$split = add_split_col(data_ggplot_model_1_sigma_j_2, nb_parameters_per_plot)
    data_ggplot_model_1_sigma_j_2 = plyr:::splitter_d(data_ggplot_model_1_sigma_j_2, .(split))
    
    fun1 = function(x, xmin, xmax){ get.caterpillar.plot(x) + ggtitle("") + coord_cartesian(xlim = c(xmin, xmax)) }
    
    out_sigma_j = lapply(data_ggplot_model_1_sigma_j_2, fun1, xmin, xmax) 
    message("The sigma_j posterior distributions are done.")
  } else { out_sigma_j = NULL }
  
  # standardized epsilon_ijk distribution ----------
  if(!is.null(data_ggplot_model_1_epsilon_ijk)){
    out_epsilon_ijk = ggplot(data_ggplot_model_1_epsilon_ijk, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
    message("The standardised residuals distributions are done.")
  } else { out_epsilon_ijk = NULL }
  
  out_model_1 = list(
    "sigma_j_gamma" = out_sigma_j_gamma,
    "mu_ij" = out_mu_ij,
    "beta_jk" = out_beta_jk,
    "sigma_j" = out_sigma_j,
    "epsilon_ijk" = out_epsilon_ijk
  )
  
  return(out_model_1)
}
