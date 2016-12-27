# 0. help ----------
#' Check with plots if the model went well based on the Gelman-Rubin test and plots of posteriors distributions 
#'
#' @description
#' \code{check_model} displays plots to see if the model went well based on the Gelman-Rubin test and plots of posteriors distributions. It is important to run this step before going ahead with the analysis otherwise you may make mistakes in the interpretation of the results.
#'
#' @param out.model outputs from model 1 (\code{MC}) or model 2 (\code{FWH})
#'  
#' @param analysis "experimental_design", convergence" or "posteriors". If NULL, the three are done.
#' 
#' @param nb_parameters_per_plot The number of parameters per plot to facilitate the visualisation
#' 
#' @details
#' For analyse = "convergence", the test used is the Gelman-Rubin test. 
#' It may take some time to run.
#' More details with ?\code{gelman.diag} from the \code{coda} package. 
#' Note that for \code{gelman.diag}, the argument \code{multivariate = FALSE} is used.
#' If you wish exhaustive information, look at \code{ggmcmc::ggmcmc} with \code{ggmcmc(out.model$MCMC)}. 
#' But be careful with the size of your MCMC output which are often too big to be performed in R. 
#' 
#' More information in  the vignette. Type vignette("PPBstats").
#' 
#' @return The function returns a list with 
#' 
#' \itemize{
#' \item "data.experimental_design" : a plot representing the presence/abscence matrix of GxE combinaisons in the data.
#' 
#' \item "model.presence.abscence.matrix" : a matrix germplasm x environment with the number of occurence in the data used for the model (i.e. with at least two germplasm by environments.)
#' 
#' \item "convergence" : a list with the plots of trace and density to check the convergence of the two MCMC only for chains that are not converging thanks to the Gelman-Rubin test. If all the chains converge, it is NULL
#' 
#' \item "posteriors" a list with
#' \itemize{
#' 
#'  \item for model 1
#'  \itemize{
#'    \item "sigma_distribution" : the distribution of the sigma is displayed on the Inverse Gamma distribution
#'    \item "parameter_posteriors" : a caterpillar plot is displayed for each mu_ij, beta_jk for each environment and for sigma_j 
#'    \item "standardized_residuals" : a plot to check the normality of the residuals
#'  }
#'  
#'  \item for model 2
#'    \itemize{
#'    \item "parameter_posteriors" : a list with caterpillar plot for each alpha_i, beta_i and theta_j
#'    \item "standardized_residuals" : a plot to check the normality of the residuals. If the model went well it should be between -2 and 2.
#'    }
#'    
#'  }
#'  
#' \item "MCMC" : a data fame resulting from the concatenation of the two MCMC for each parameter. This object can be used for further analysis. There are as many columns than parameters and as many rows than iterations/10 (10 being the thin value by default in the models). The MCMC contains only parameters that converge thanks to the Gelman-Rubin test (if it has been done). For model 1, all environments where at least one parameter do not converge are deleted.
#' 
#' \item "model1.data_env_whose_param_did_not_converge" : a list with data frame with environments where some parameters did not converge for mu and beta.
#' }
#' 
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{MC}}, \code{\link{FWH}}, \code{\link{get.mean.comparisons}}
#' 
#' 
check_model = function(
out.model
)
# let's go !!! ----------
{
  # 1. Error message and update arguments ----------
  if( is.null(attributes(out.model)$PPBstats.object) ) { stop("out.model should be an output from model 1 (PPBstats::model_1), model 2 (PPBstats::model_2) or GxE (PPBstats::GxE).") } 
  
  # 2. Check model ----------
  if(attributes(out.model)$PPBstats.object == "model1") {
    out = check_model_model_1(out.model)
  }
  
  if(attributes(out.model)$PPBstats.object == "model2") {
    out = check_model_model_2(out.model)
  }

return(out)
}

