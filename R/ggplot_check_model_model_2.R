#' Get ggplot from check_model_model_2
#'
#' @description
#' \code{ggplot_check_model_model_2} returns ggplot from \code{\link{check_model_model_2}}
#' 
#' @param x outputs from \code{\link{check_model_model_2 function}}
#' 
#' @details See \code{\link{get_ggplot}}
#' 
#' @return See \code{\link{get_ggplot}}
#' 
#' @seealso 
#' \itemize{
#' \item  \code{\link{get_ggplot}}, 
#' \item \code{\link{check_model_model_2}}
#' }
#'
#'
plot.check_model_2 <- function(
  x,
  nb_parameters_per_plot = 10
){
  # Get data ----------
  
  data_ggplot = x$data_ggplot
  data_ggplot_model_2_alpha = data_ggplot$alpha
  data_ggplot_model_2_beta = data_ggplot$beta
  data_ggplot_model_2_theta = data_ggplot$theta
  data_ggplot_model_2_epsilon = data_ggplot$epsilon
  MCMC_conv_not_ok = x$MCMC_conv_not_ok
  
  # 2.1. alpha_i caterpillar plot distribution ----------
  if ( !is.null(data_ggplot_model_2_alpha) ) {
    xmin = min(data_ggplot_model_2_alpha$q1)
    xmax = max(data_ggplot_model_2_alpha$q5)
    
    data_ggplot_model_2_alpha$split = add_split_col(data_ggplot_model_2_alpha, each = nb_parameters_per_plot)
    data_ggplot_model_2_alpha_split = plyr:::splitter_d(data_ggplot_model_2_alpha, .(split))      
    
    out_alpha = lapply(data_ggplot_model_2_alpha_split, function(x){ get.caterpillar.plot(x, xmin, xmax) } )
  } else { out_alpha = NULL }
  
  
  # 2.2. beta_i caterpillar plot distribution ----------
  if ( !is.null(data_ggplot_model_2_beta) ) {
    xmin = min(data_ggplot_model_2_beta$q1)
    xmax = max(data_ggplot_model_2_beta$q5)
    
    data_ggplot_model_2_beta$split = add_split_col(data_ggplot_model_2_beta, each = nb_parameters_per_plot)
    data_ggplot_model_2_beta_split = plyr:::splitter_d(data_ggplot_model_2_beta, .(split))      
    
    out_beta = lapply(data_ggplot_model_2_beta_split, function(x){ get.caterpillar.plot(x, xmin, xmax) }) 
    message("The beta_i posterior distributions are done.")      
  } else { out_beta = NULL }
  
  
  # 2.3. theta_j caterpillar plot distribution ----------
  if ( !is.null(data_ggplot_model_2_theta) ) {
    xmin = min(data_ggplot_model_2_theta$q1)
    xmax = max(data_ggplot_model_2_theta$q5)
    
    data_ggplot_model_2_theta$split = add_split_col(data_ggplot_model_2_theta, each = nb_parameters_per_plot)
    data_ggplot_model_2_theta_split = plyr:::splitter_d(data_ggplot_model_2_theta, .(split))      
    
    out_theta = lapply(data_ggplot_model_2_theta_split, function(x){ get.caterpillar.plot(x, xmin, xmax) } )
    message("The theta_j posterior distributions are done.")
  } else { out_theta = NULL }
  
  if ( !is.null(data_ggplot_model_2_epsilon) ) {
    out_epsilon = ggplot(data_ggplot_model_2_epsilon, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
    message("The standardised residuals distributions are done.")
  } else { out_epsilon = NULL }
  
  # 2.4. MCMC traceplot and density that did not converge ----------
  if( !is.null(MCMC_conv_not_ok) ){
    out_mcmc_not_converge = get_mcmc_traceplot_density(MCMC_conv_not_ok)
    message("Trace and density plot for MCMC that did not converged are done.")
  } else { 
    out_mcmc_not_converge = NULL
    }
  
  # 3. Return results ---------- 
  out_model_2 = list(
    "alpha_i" = out_alpha,
    "beta_i" = out_beta,
    "theta_j" = out_theta,
    "epsilon_ij" = out_epsilon,
    "mcmc_not_converge_traceplot_density" = out_mcmc_not_converge
  )
  
  return(out_model_2)
}