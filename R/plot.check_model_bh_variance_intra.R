#' Get ggplot to visualize output from \code{\link{check_model.fit_model_bh_variance_intra}}
#'
#' @description
#' \code{plot.check_model_bh_variance_intra} returns ggplot to visualize outputs from \code{\link{check_model.fit_model_bh_variance_intra}}
#'
#' @param x Output from \code{\link{check_model.fit_model_bh_variance_intra}}
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
#' See example in the book: https://priviere.github.io/PPBstats_book/family-4.html#variance-intra
#' 
#' @return 
#' 
#' \itemize{
#'  \item mu_ijk : distribution of each mu_ijk. 
#'  There are as many graph as needed with \code{nb_parameters_per_plot} alpha_i per graph.
#'  \item sigma_ij : distribution of each sigma_ij. 
#'  There are as many graph as needed with \code{nb_parameters_per_plot} alpha_i per graph.
#'  \item mcmc_not_converge_traceplot_density : a list with the plots of trace and density to check the convergence of the two MCMC only for chains that are not converging thanks to the Gelman-Rubin test. 
#'  If all the chains converge, it is NULL.
#'  }
#'   
#' @author Gaelle Van Frank and Pierre Riviere
#' 
#' @seealso \code{\link{check_model.fit_model_bh_variance_intra}}
#' 
#' @export
#' 
#' @import plyr
#' @import ggplot2
#' 
plot.check_model_bh_variance_intra = function(
  x,
  nb_parameters_per_plot = 10, ...
){
  # Get data ----------
  data_ggplot = x$data_ggplot
  data_ggplot_model_bh_variance_intra_sigma_ij = data_ggplot$sigma_ij
  data_ggplot_model_bh_variance_intra_sigma_ij = data_ggplot$mu_ijk
  colnames(data_ggplot_model_bh_variance_intra_sigma_ij)[grep("environment",colnames(data_ggplot_model_bh_variance_intra_sigma_ij))] = "plot"
  data_ggplot_model_bh_variance_intra_sigma_ij$environment = paste(data_ggplot_model_bh_variance_intra_sigma_ij$location,data_ggplot_model_bh_variance_intra_sigma_ij$year,sep=":")
  data_ggplot_model_bh_variance_intra_sigma_ij$environment = unlist(lapply(data_ggplot_model_bh_variance_intra_sigma_ij$environment, function(x){return(strsplit(x,",")[[1]][2])}))
  data_ggplot_model_bh_variance_intra_sigma_ij$entry = paste(data_ggplot_model_bh_variance_intra_sigma_ij$location,data_ggplot_model_bh_variance_intra_sigma_ij$year,sep=":")
  MCMC_conv_not_ok = x$MCMC_conv_not_ok
  
  
  # mu_ijk caterpillar plot ----------
  if(!is.null(data_ggplot_model_bh_variance_intra_sigma_ij)){
    xmin = min(data_ggplot_model_bh_variance_intra_sigma_ij$q1)
    xmax = max(data_ggplot_model_bh_variance_intra_sigma_ij$q5)
    
    data_ggplot_model_bh_variance_intra_sigma_ij = splitter_d(data_ggplot_model_bh_variance_intra_sigma_ij, .(environment))
    
    fun2 = function(x, nb_parameters_per_plot, xmin, xmax){ 
      x$split = add_split_col(x, nb_parameters_per_plot)
      xx = splitter_d(x, .(split))
      out = lapply(xx, function(x){ get.caterpillar.plot(x, xmin, xmax) } )
      return(out)
    }
    
    out_mu_ijk = lapply(data_ggplot_model_bh_variance_intra_sigma_ij, fun2, nb_parameters_per_plot, xmin, xmax)
    
    message("The mu_ijkk posterior distributions are done.")
  } else { out_mu_ijk = NULL }
  

  # sigma_ij caterpillar plot ----------
  if(!is.null(data_ggplot_model_bh_variance_intra_sigma_ij)){
    xmin = min(data_ggplot_model_bh_variance_intra_sigma_ij$q1); xmax = max(data_ggplot_model_bh_variance_intra_sigma_ij$q5)
    
    data_ggplot_model_bh_variance_intra_sigma_ij = splitter_d(data_ggplot_model_bh_variance_intra_sigma_ij, .(environment))
    
    out_sigma_ij = lapply(data_ggplot_model_bh_variance_intra_sigma_ij, fun2, nb_parameters_per_plot, xmin, xmax)

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
  out_model_bh_variance_intra = list(
    "mu_ijk" = out_mu_ijk,
    "sigma_ij" = out_sigma_ij,
    "mcmc_not_converge_traceplot_density" = out_mcmc_not_converge
  )
  
  return(out_model_bh_variance_intra)
}
