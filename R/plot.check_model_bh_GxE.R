#' Get ggplot to visualize output from \code{\link{check_model.fit_model_bh_GxE}}
#'
#' @description
#' \code{plot.check_model_bh_GxE} returns ggplot to visualize outputs from \code{\link{check_model.fit_model_bh_GxE}}
#'
#' @param x Output from \code{\link{check_model.fit_model_bh_GxE}}
#' 
#' @param nb_parameters_per_plot number of parameter per plot to display
#' 
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' 
#' For mcmc_not_converge_traceplot_density : If you wish exhaustive information, look at \code{ggmcmc::ggmcmc} with \code{ggmcmc(out_model$MCMC)}. 
#' But be careful with the size of your MCMC output which are often too big to be performed in R.
#' 
#' See example in the book: https://priviere.github.io/PPBstats_book/family-2.html#model-2
#' 
#' @return 
#' 
#' \itemize{
#'  \item alpha_i : distribution of each alpha_i. 
#'  There are as many graph as needed with \code{nb_parameters_per_plot} alpha_i per graph.
#'  \item beta_i : distribution of each beta_i. 
#'  There are as many graph as needed with \code{nb_parameters_per_plot} beta_i per graph.
#'  \item theta_j : distribution of each theta_j. 
#'  There are as many graph as needed with \code{nb_parameters_per_plot} theta_j per graph.
#'  \item epsilon_ij : standardised residuals distribution.  
#'  If the model went well it should be between -2 and 2.
#'  \item mcmc_not_converge_traceplot_density : a list with the plots of trace and density to check the convergence of the two MCMC only for chains that are not converging thanks to the Gelman-Rubin test. 
#'  If all the chains converge, it is NULL.
#'  }
#'   
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{check_model.fit_model_bh_GxE}}
#' 
#' @export
#' 
#' @import plyr
#' @import ggplot2
#' 
plot.check_model_bh_GxE <- function(
  x,
  nb_parameters_per_plot = 8, ...
){
  std_res  = NULL  # to avoid no visible binding for global variable
  
  # Get data ----------
  
  data_ggplot = x$data_ggplot
  data_ggplot_model_bh_GxE_alpha = data_ggplot$alpha
  data_ggplot_model_bh_GxE_beta = data_ggplot$beta
  data_ggplot_model_bh_GxE_theta = data_ggplot$theta
  data_ggplot_model_bh_GxE_epsilon = data_ggplot$epsilon
  MCMC_conv_not_ok = x$MCMC_conv_not_ok
  
  # 2.1. alpha_i caterpillar plot distribution ----------
  if ( !is.null(data_ggplot_model_bh_GxE_alpha) ) {
    xmin = min(data_ggplot_model_bh_GxE_alpha$q1)
    xmax = max(data_ggplot_model_bh_GxE_alpha$q5)
    
    data_ggplot_model_bh_GxE_alpha$split = add_split_col(data_ggplot_model_bh_GxE_alpha, each = nb_parameters_per_plot)
    data_ggplot_model_bh_GxE_alpha_split = plyr:::splitter_d(data_ggplot_model_bh_GxE_alpha, .(split))      
    
    out_alpha = lapply(data_ggplot_model_bh_GxE_alpha_split, function(x){ get.caterpillar.plot(x, xmin, xmax) } )
  } else { out_alpha = NULL }
  
  
  # 2.2. beta_i caterpillar plot distribution ----------
  if ( !is.null(data_ggplot_model_bh_GxE_beta) ) {
    xmin = min(data_ggplot_model_bh_GxE_beta$q1)
    xmax = max(data_ggplot_model_bh_GxE_beta$q5)
    
    data_ggplot_model_bh_GxE_beta$split = add_split_col(data_ggplot_model_bh_GxE_beta, each = nb_parameters_per_plot)
    data_ggplot_model_bh_GxE_beta_split = plyr:::splitter_d(data_ggplot_model_bh_GxE_beta, .(split))      
    
    out_beta = lapply(data_ggplot_model_bh_GxE_beta_split, function(x){ get.caterpillar.plot(x, xmin, xmax) }) 
    message("The beta_i posterior distributions are done.")      
  } else { out_beta = NULL }
  
  
  # 2.3. theta_j caterpillar plot distribution ----------
  if ( !is.null(data_ggplot_model_bh_GxE_theta) ) {
    xmin = min(data_ggplot_model_bh_GxE_theta$q1)
    xmax = max(data_ggplot_model_bh_GxE_theta$q5)
    
    data_ggplot_model_bh_GxE_theta$split = add_split_col(data_ggplot_model_bh_GxE_theta, each = nb_parameters_per_plot)
    data_ggplot_model_bh_GxE_theta_split = plyr:::splitter_d(data_ggplot_model_bh_GxE_theta, .(split))      
    
    out_theta = lapply(data_ggplot_model_bh_GxE_theta_split, function(x){ get.caterpillar.plot(x, xmin, xmax) } )
    message("The theta_j posterior distributions are done.")
  } else { out_theta = NULL }
  
  if ( !is.null(data_ggplot_model_bh_GxE_epsilon) ) {
    out_epsilon = ggplot(data_ggplot_model_bh_GxE_epsilon, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
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
  out_model_bh_GxE = list(
    "alpha_i" = out_alpha,
    "beta_i" = out_beta,
    "theta_j" = out_theta,
    "epsilon_ij" = out_epsilon,
    "mcmc_not_converge_traceplot_density" = out_mcmc_not_converge
  )
  
  return(out_model_bh_GxE)
}