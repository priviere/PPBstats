#' Get ggplot to visualize output from \code{\link{check_model.fit_model_bh_intra_location}}
#'
#' @description
#' \code{plot.check_model_bh_GxE} returns ggplot to visualize outputs from \code{\link{check_model.fit_model_bh_intra_location}}
#'
#' @param x Output from \code{\link{check_model.fit_model_bh_intra_location}}
#' 
#' @param nb_parameters_per_plot number of parameter per plot to display
#' 
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' 
#' See example in the book: https://priviere.github.io/PPBstats_book/family-2.html#model-1
#' 
#' For mcmc_not_converge_traceplot_density : If you wish exhaustive information, look at \code{ggmcmc::ggmcmc} with \code{ggmcmc(out_model$MCMC)}. 
#' But be careful with the size of your MCMC output which are often too big to be performed in R.
#' 
#' @return 
#' \itemize{
#'  \item sigma_j_gamma : mean of each sigma_j displayed on the inverse Gamma distribution. 
#'  The first graph represent all the sigma_j, the other graph represent \code{nb_parameters_per_plot} sigma_j per graph.
#'  \item mu_ij : distribution of each mu_ij in a list with as many elements as environment. 
#'  For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} mu_ij per graph.
#'  \item beta_jk : distribution of each beta_jk in a list with as many elements as environment. 
#'  For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} beta_jk per graph.
#'  \item sigma_j : distribution of each sigma_j. 
#'  There are as many graph as needed with \code{nb_parameters_per_plot} sigma_j per graph.
#'  \item epsilon_ijk : standardised residuals distribution.
#'  If the model went well it should be between -2 and 2.
#'  \item mcmc_not_converge_traceplot_density : a list with the plots of trace and density to check the convergence of the two MCMC only for chains that are not converging thanks to the Gelman-Rubin test. 
#'  If all the chains converge, it is NULL.
#'  }
#'   
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{check_model.fit_model_bh_intra_location}}
#' 
#' @export
#' 
#' @import plyr
#' @import ggplot2
#' 
plot.check_model_bh_intra_location <- function(
  x,
  nb_parameters_per_plot = 8, ...
){
  sigma_distribution = value = std_res = NULL  # to avoid no visible binding for global variable
  
  # Get data ----------
  data_ggplot = x$data_ggplot
  data_ggplot_model_1_sigma_j = data_ggplot$sigma_j
  data_ggplot_model_1_mu_ij = data_ggplot$mu_ij
  data_ggplot_model_1_beta_jk = data_ggplot$beta_jk
  data_ggplot_model_1_sigma_j_2 = data_ggplot$sigma_j_2
  data_ggplot_model_1_epsilon_ijk = data_ggplot$epsilon_ijk
  MCMC_conv_not_ok = x$MCMC_conv_not_ok
  
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
    
    fun2 = function(x, nb_parameters_per_plot, xmin, xmax){ 
      x$split = add_split_col(x, nb_parameters_per_plot)
      xx = plyr:::splitter_d(x, .(split))
      out = lapply(xx, function(x){ get.caterpillar.plot(x, xmin, xmax) } )
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
    
    fun1 = function(x, xmin, xmax){ get.caterpillar.plot(x, xmin, xmax) + ggtitle("") }
    
    out_sigma_j = lapply(data_ggplot_model_1_sigma_j_2, fun1, xmin, xmax) 
    message("The sigma_j posterior distributions are done.")
  } else { out_sigma_j = NULL }
  
  # standardized epsilon_ijk distribution ----------
  if(!is.null(data_ggplot_model_1_epsilon_ijk)){
    out_epsilon_ijk = ggplot(data_ggplot_model_1_epsilon_ijk, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
    message("The standardised residuals distributions are done.")
  } else { out_epsilon_ijk = NULL }
  
  # MCMC traceplot and density that did not converge ----------
  if( !is.null(MCMC_conv_not_ok) ){
    out_mcmc_not_converge = get_mcmc_traceplot_density(MCMC_conv_not_ok)
    message("Trace and density plot for MCMC that did not converged are done.")
  } else { 
    out_mcmc_not_converge = NULL
  }
  
  # Return results ---------- 
  out_model_1 = list(
    "sigma_j_gamma" = out_sigma_j_gamma,
    "mu_ij" = out_mu_ij,
    "beta_jk" = out_beta_jk,
    "sigma_j" = out_sigma_j,
    "epsilon_ijk" = out_epsilon_ijk,
    "mcmc_not_converge_traceplot_density" = out_mcmc_not_converge
  )
  
  return(out_model_1)
}
