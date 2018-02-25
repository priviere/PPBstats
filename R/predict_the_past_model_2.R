#' Predict values of germplasms in environments where they have not been grown based on Hierarchical Bayesian GxE model
#'
#' @description
#' \code{predict_the_past_model_bh_GxE} predicts values of germplasms in environments where 
#' they have not been grown based on Hierarchical Bayesian GxE model.
#' 
#' @param out_check_model_model_bh_GxE object from \code{\link{check_model}} from \code{\link{model_bh_GxE}}
#' 
#' @param env name of the environment where the germplasm effect are predicted
#' 
#' @return The function returns a MCMC for the given environment. 
#' This MCMC output can be used in the same way as the output from \code{\link{check_model}} from \code{\link{model_1}}.
#' 
#' @details
#' The estimations of the values are based on the MCMC outputs.
#' More informations can be found in the vignette.
#' 
#' It is like mu_ij effect that are estimated (as for Hierarchical Bayesian intra-location model), i.e. the effect of a germplasm in an environment.
#' 
#' Due to memory issues, it is better to run the function for only one environment instead of all by default.
#' This allows the same ggplot as for Hierarchical Bayesian intra-location model.
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}, 
#' \item \code{\link{check_model_model_bh_GxE}}, 
#' \item \code{\link{mean_comparisons}}, 
#' \item \code{\link{mean_comparisons_predict_the_past_model_bh_GxE}}
#' }
#' 
#' 
predict_the_past_model_bh_GxE = function(
  out_check_model_model_bh_GxE,
  env = NULL
) {
  ## TODO: Ideally, there should be a generic predict_the_past()
  ## which only has a method for a "check_model_bh_GxE" object.
  ## This way is extensible to a future situation where there might be more
  ## models to which the past can be "predicted" with other methods.
  ## For the moment, I leave it as it to preserve back-compatibility.
  
  # 1. Error message ----------  
  if( !inherits(out_check_model_model_bh_GxE, "check_model_bh_GxE") ) {
    stop("out_check_model_model_bh_GxE must come from check_model and model_bh_GxE.")
  }
  
  w = out_check_model_model_bh_GxE$model2.presence.absence.matrix
  MCMC = out_check_model_model_bh_GxE$MCMC
  
  if( is.null(env) ){ stop("env can not be NULL") }
  if( !is.element(env, colnames(w)) ){ stop("env ", env," does not exist.")  }
  
  # 2. Get the estimation of mu_ij based on MCMC outputs ----------
  
  # 2.1. function to get mu ----------
  get_mu = function(germ, MCMC){
    if (is.element(paste("alpha","[",germ,"]",sep=""), colnames(MCMC)) & is.element(paste("theta","[",env,"]",sep=""), colnames(MCMC)))  {
      mu = MCMC[,paste("alpha[",germ,"]",sep="")] + MCMC[,paste("beta[",germ,"]",sep="")] * MCMC[,paste("theta[",env,"]",sep="")]
    } else { 
      mu = NULL
      warning("Estimated or predicted value for germplasm ", germ, " in environment ", env, " is not possible. This is because the estimation of germplasm or environment effects did not converge and therefore were not in the MCMC.") 
    }
    return(mu)
  }
  
  
  # 2.2. mu estimated ----------
  germ_estimated = rownames(w)[which(w[,which(colnames(w) == env)] != 0)]
  OUT_MCMC_estimated = data.frame(matrix(NA, ncol = length(germ_estimated), nrow = nrow(MCMC)))
  for (i in 1:length(germ_estimated)) {
    mu_estimated = get_mu(germ_estimated[i], MCMC)  
    OUT_MCMC_estimated[i] = mu_estimated
  }
  names(OUT_MCMC_estimated) = paste("mu[", germ_estimated, ",", env,"]", sep = "")
  
  # 2.3. mu predicted ----------
  germ_to_predict = rownames(w)[which(w[,which(colnames(w) == env)] == 0)]
  OUT_MCMC_predicted = data.frame(matrix(NA, ncol = length(germ_to_predict), nrow = nrow(MCMC)))
  for (i in 1:length(germ_to_predict)) {
    mu_predicted = get_mu(germ_to_predict[i], MCMC)  
    OUT_MCMC_predicted[i] = mu_predicted
  }
  names(OUT_MCMC_predicted) = paste("mu[", germ_to_predict, ",", env,"]", sep = "")
  
  OUT_MCMC = cbind.data.frame(OUT_MCMC_estimated, OUT_MCMC_predicted)
  parameter_statuts = c(
    rep("estimated", ncol(OUT_MCMC_estimated)), 
    rep("predicted", ncol(OUT_MCMC_predicted))
  )
  names(parameter_statuts) = colnames(OUT_MCMC)
  
  # 3. Return the results ----------
  out = list(
    "MCMC" = OUT_MCMC,
    "parameter_statuts" = parameter_statuts
    )
  class(out) <- c("PPBstats", "predict_the_past_model_bh_GxE")
  return(out)
}
