#' Check if the model went well 
#'
#' @description
#' \code{check_model} computes tests to assess if the model went well. 
#' It is important to run this step before going ahead with the analysis otherwise you may make mistakes in the interpretation of the results.
#' 
#' @param x outputs from \code{\link{GxE}}, \code{\link{model_1}}, \code{\link{model_2}}
#' 
#' @details
#' 
#' For \code{\link{model_1}} and \code{\link{model_2}}, the test used for convergence is the Gelman-Rubin test.
#' It may take some time to run.
#' More details with ?\code{gelman.diag} from the \code{coda} package.
#' Note that for \code{gelman.diag}, the argument \code{multivariate = FALSE} is used.
#' 
#'
#' MCMC outputs are a data fame resulting from the concatenation of the two MCMC for each parameter. 
#' This object can be used for further analysis. 
#' There are as many columns as parameters and as many rows as iterations/10 (10 being the thin value by default in the models). 
#' The MCMC contains only parameters that converge thanks to the Gelman-Rubin test (if it has been done). 
#' For \code{\link{model_1}}, all environments where at least one parameter do not converge are not taken into account in the MCMC outputs.
#' 
#' The outputs of the function is used in 
#' \itemize{
#'  \item \code{\link{mean_comparisons}} for \code{\link{GxE}}, \code{\link{model_1}} and \code{\link{model_2}}
#'  \item \code{\link{parameter_groups}} for \code{\link{GxE}} and \code{\link{model_2}}
#'  \item \code{\link{predict_the_past_model_2}} for \code{\link{model_2}}
#' }
#' 
#' 
#' @return The function returns a list depending on check_model input 
#' 
#' \itemize{
#' 
#' \item GxE
#' \itemize{
#'  \item GxE the output from the model
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
#' \item model_1
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
#' \item model_2
#' \itemize{
#'  \item MCMC : a data fame resulting from the concatenation of the two MCMC for each parameter. (See details for more information).
#'  \item MCMC_conv_not_ok : a data fame resulting from the concatenation of the two MCMC for each parameter for environment where  some parameters did not converge for mu and beta
#'  \item model2.presence.abscence.matrix : a matrix germplasm x environment with the number of occurence in the data used for the model (i.e. with at least two germplasm by environments.)
#'  \item data_ggplot a list containing information for ggplot:
#'  \itemize{
#'   \item alpha
#'   \item beta
#'   \item theta
#'   \item epsilon
#'  }
#' }
#' 
#' }
#'
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{GxE}}, 
#' \item \code{\link{model_1}}, 
#' \item \code{\link{model_2}}, 
#' \item \code{\link{check_model_GxE}}, 
#' \item \code{\link{check_model_1}}, 
#' \item \code{\link{check_model_2}}, 
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
