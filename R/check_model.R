#' Check if the model went well 
#'
#' @description
#' \code{check_model} computes tests to assess if the model went well. 
#' It is important to run this step before going ahead with the analysis otherwise you may make mistakes in the interpretation of the results.
#' 
#' @param x outputs from \code{\link{model_GxE}}, \code{\link{model_bh_intra_location}}, \code{\link{model_bh_GxE}}, \code{\link{model_spatial}}, \code{\link{model_bh_variance-intra}}
#' 
#' @details
#' 
#' For \code{\link{model_bh_intra_location}}, \code{\link{model_bh_GxE}} and \code{\link{model_bh_variance-intra}}, the test used for convergence is the Gelman-Rubin test.
#' It may take some time to run.
#' More details with ?\code{gelman.diag} from the \code{coda} package.
#' Note that for \code{gelman.diag}, the argument \code{multivariate = FALSE} is used.
#' 
#'
#' MCMC outputs are a data fame resulting from the concatenation of the two MCMC for each parameter. 
#' This object can be used for further analysis. 
#' There are as many columns as parameters and as many rows as iterations/10 (10 being the thin value by default in the models). 
#' The MCMC contains only parameters that converge thanks to the Gelman-Rubin test (if it has been done). 
#' For \code{\link{model_bh_intra_location}}, all environments where at least one parameter do not converge are not taken into account in the MCMC outputs.
#' 
#' The outputs of the function is used in 
#' \itemize{
#'  \item \code{\link{mean_comparisons}} for \code{\link{model_GxE}}, \code{\link{model_bh_intra_location}}, \code{\link{model_bh_GxE}} and \code{\link{model_bh_variance-intra}}
#'  \item \code{\link{parameter_groups}} for \code{\link{model_GxE}} and \code{\link{model_bh_GxE}}
#'  \item \code{\link{predict_the_past_model_bh_GxE}} for \code{\link{model_bh_GxE}}
#' }
#' 
#' 
#' @return The function returns a list depending on check_model input 
#' 
#' \itemize{
#' 
#' \item model_GxE
#' \itemize{
#'  \item model_GxE the output from the model
#'  \item data_ggplot a list containing information for ggplot:
#'  \itemize{
#'   \item data_ggplot_residuals a list containing :
#'    \itemize{
#'     \item data_ggplot_normality
#'     \item data_ggplot_skewness_test
#'     \item data_ggplot_kurtosis_test
#'     \item data_ggplot_qqplot
#'     }
#'   \item data_ggplot_variability_repartition_pie
#'   \item data_ggplot_var_intra
#'  }
#' }
#'
#' 
#' \item model_bh_intra_location
#' \itemize{
#'  \item MCMC : a data fame resulting from the concatenation of the two MCMC for each parameter (See details for more information).
#'  \item MCMC_conv_not_ok : a data fame resulting from the concatenation of the two MCMC for each parameter for environment where  some parameters did not converge for mu and beta
#'  \item data_env_with_no_controls : data frame with environnement with no controls
#'  \item data_env_whose_param_did_not_converge : a list with data frame with environments where some parameters did not converge for mu and beta.
#'  \item data_ggplot a list containing information for ggplot:
#'  \itemize{
#'   \item sigma_j
#'   \item mu_ij
#'   \item beta_jk
#'   \item sigma_j_2
#'   \item epsilon_ijk
#'  }
#' }
#' 
#' 
#' \item model_bh_GxE
#' \itemize{
#'  \item MCMC : a data fame resulting from the concatenation of the two MCMC for each parameter. (See details for more information).
#'  \item MCMC_conv_not_ok : a data fame resulting from the concatenation of the two MCMC for each parameter for environment where  some parameters did not converge for mu and beta
#'  \item model2.presence.absence.matrix : a matrix germplasm x environment with the number of occurence in the data used for the model (i.e. with at least two germplasm by environments.)
#'  \item data_ggplot a list containing information for ggplot:
#'  \itemize{
#'   \item alpha
#'   \item beta
#'   \item theta
#'   \item epsilon
#'  }
#' }
#' 
#' \item model_spatial
#' \itemize{
#'  \item model_spatial : the output from the model
#'  \item data_ggplot a list containing information for ggplot:
#'  \itemize{
#'   \item data_ggplot_residuals a list containing :
#'    \itemize{
#'     \item data_ggplot_normality
#'     \item data_ggplot_skewness_test
#'     \item data_ggplot_kurtosis_test
#'     \item data_ggplot_qqplot
#'     }
#'   \item data_ggplot_variability_repartition_pie
#'   }
#' }
#' 
#' \item model_bh_variance-intra
#' \itemize{
#'  \item MCMC : a data fame resulting from the concatenation of the two MCMC for each parameter. (See details for more information).
#'  \item MCMC_conv_not_ok : a data fame resulting from the concatenation of the two MCMC for each parameter for environment where  some parameters did not converge for mu and beta
#'  \item data_ggplot a list containing information for ggplot:
#'  \itemize{
#'   \item mu
#'   \item sigma
#'   \item epsilon
#'  }
#' }
#' 
#' \item model_napping
#' \itemize{
#'  \item variances of dimensions
#' }
#' 
#' }
#'
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{model_GxE}}, 
#' \item \code{\link{model_spatial}}, 
#' \item \code{\link{model_bh_intra_location}}, 
#' \item \code{\link{model_bh_variance-intra}},
#' \item \code{\link{model_bh_GxE}}, 
#' \item \code{\link{model_napping}}
#' \item \code{\link{mean_comparisons}}
#' }
#' 
check_model <- function(x) UseMethod("check_model")

check_model.default <- function(x) {
  ## Method not found
  ## This message needs updating whenever new models are included in PPBstats
  ## TODO: have a list of supported models somewhere, and use it to compose
  ## the error message automatically, or use a more generic message.
  mess = paste(substitute(x),
               "is a model type not yet fully supported by PPBstats")
  stop(mess)
}
