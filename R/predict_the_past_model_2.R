#' Estimate value of a germplasm in an environment based on model 2
#'
#' @description
#' \code{predict_the_past_model_2} estimates value of a germplasm in an environment based on model 2.
#' 
#' @param out_check_model_model_2 object from \code{\link{check_model}} from \code{\link{model_2}}
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
#' It is like mu_ij effect that are estimated (as for model 1), i.e. the effect of a germplasm in an environment.
#' 
#' Due to memory issues, it is better to run the function for only one environment instread of all by default.
#' This allows the same ggplot as for model 1.
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}, 
#' \item \code{\link{check_model_model_2}}, 
#' \item \code{\link{mean_comparisons}}, 
#' \item \code{\link{mean_comparisons_predict_the_past_model_2}}
#' }
#' 
#' 
predict_the_past_model_2 = function(
  out_check_model_model_2,
  env = NULL
)
{
  # 1. Error message ----------  
  if( attributes(out_check_model_model_2)$PPBstats.object != "check_model_model_2") {  stop("out_check_model_model_2 must come from check_model and model_2.") }
  
  w = out_check_model_model_2$model2.presence.abscence.matrix
  MCMC = out_check_model_model_2$MCMC
  
  if( is.null(env) ){ stop("env can not be NULL") }
  if( !is.element(env, colnames(w)) ){ stop("env ", env," does not exist.")  }
  
  # 2. Get the estimation of mu_ij based on MCMC outputs ----------
  j = which(colnames(w) == env)
  germ = rownames(w)[which(w[,j] == 0)]
  OUT = data.frame(matrix(NA, ncol = length(germ), nrow = nrow(MCMC)))
  if(length(germ) > 0) { # if length(geno) == 0, it means no germplasms must be estimated
    for (i in 1:length(germ)) {
      if (is.element(paste("alpha","[",germ[i],"]",sep=""), colnames(MCMC)) & is.element(paste("theta","[",env,"]",sep=""), colnames(MCMC)))  {
        mu_estimated = MCMC[,paste("alpha[",germ[i],"]",sep="")] + MCMC[,paste("beta[",germ[i],"]",sep="")] * MCMC[,paste("theta[",env,"]",sep="")]
        OUT[i] = mu_estimated
      } else { warning("Estimated value for germplasm ", germ, " in environment ", env, " is not possible. This is because the estimation of germplasm and or environment effects did not converge and therefore were not in the MCMC.") 
      }
    }
    names(OUT) = paste("mu[", germ, ",", env,"]", sep = "")
  } else { OUT = NULL }
  
  # 3. Return the results ----------
  out = list("MCMC" = OUT)
  attributes(out)$PPBstats.object = "predict_the_past_model_2"
  return(out)
}
