plot.check_model_model_variance_intra = function(
  x,
  nb_parameters_per_plot = 10
){
  # Get data ----------
  data_ggplot = x$data_ggplot
  data_ggplot_model_varintra_sigma_ij = data_ggplot$sigma_ij
  data_ggplot_model_varintra_mu_ij = data_ggplot$mu_ij
  colnames(data_ggplot_model_varintra_mu_ij)[grep("environment",colnames(data_ggplot_model_varintra_mu_ij))] = "plot"
  data_ggplot_model_varintra_mu_ij$environment = paste(data_ggplot_model_varintra_mu_ij$location,data_ggplot_model_varintra_mu_ij$year,sep=":")
  data_ggplot_model_varintra_sigma_ij$environment = unlist(lapply(data_ggplot_model_varintra_sigma_ij$environment, function(x){return(strsplit(x,",")[[1]][2])}))
  data_ggplot_model_varintra_sigma_ij$entry = paste(data_ggplot_model_varintra_sigma_ij$location,data_ggplot_model_varintra_sigma_ij$year,sep=":")
  MCMC_conv_not_ok = x$MCMC_conv_not_ok
  
  
  # mu_ij caterpillar plot ----------
  if(!is.null(data_ggplot_model_varintra_mu_ij)){
    xmin = min(data_ggplot_model_varintra_mu_ij$q1)
    xmax = max(data_ggplot_model_varintra_mu_ij$q5)
    
    data_ggplot_model_varintra_mu_ij = plyr:::splitter_d(data_ggplot_model_varintra_mu_ij, .(environment))
    
    fun2 = function(x, nb_parameters_per_plot, xmin, xmax){ 
      x$split = add_split_col(x, nb_parameters_per_plot)
      xx = plyr:::splitter_d(x, .(split))
      out = lapply(xx, function(x){ get.caterpillar.plot(x, xmin, xmax) } )
      return(out)
    }
    
    out_mu_ij = lapply(data_ggplot_model_varintra_mu_ij, fun2, nb_parameters_per_plot, xmin, xmax)
    
    message("The mu_ij posterior distributions are done.")
  } else { out_mu_ij = NULL }
  

  # sigma_ij caterpillar plot ----------
  if(!is.null(data_ggplot_model_varintra_sigma_ij)){
    xmin = min(data_ggplot_model_varintra_sigma_ij$q1); xmax = max(data_ggplot_model_varintra_sigma_ij$q5)
    
    data_ggplot_model_varintra_sigma_ij = plyr:::splitter_d(data_ggplot_model_varintra_sigma_ij, .(environment))
    
    out_sigma_ij = lapply(data_ggplot_model_varintra_sigma_ij, fun2, nb_parameters_per_plot, xmin, xmax)

    message("The sigma_ij posterior distributions are done.")
  } else { out_sigma_ij = NULL }
  
 
  # MCMC traceplot and density that did not converge ----------
  if( !is.null(MCMC_conv_not_ok) ){
    out_mcmc_not_converge = get_mcmc_traceplot_density(MCMC_conv_not_ok)
    message("Trace and density plot for MCMC that did not converged are done.")
  } else { 
    out_mcmc_not_converge = NULL
  }
  
  # Return results ---------- 
  out_model_variance_intra = list(
    "mu_ij" = out_mu_ij,
    "sigma_ij" = out_sigma_ij,
    "mcmc_not_converge_traceplot_density" = out_mcmc_not_converge
  )
  
  return(out_model_variance_intra)
}
