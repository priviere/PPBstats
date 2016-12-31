# 0. help ----------
#' Estimate value of a germplasm in an environment based on the FWH model.
#'
#' @description
#' \code{predict.the.past} estimates value of a germplasm in an environment based on the FWH model.
#' 
#' @param out.analyse.outputs object from \code{analyse.outputs}
#' 
#' @param output.format Format of the output: "summary" or "raw". See details.
#' 
#' 
#' @return The function returns a list with for each environment, the estimated value of the germplasms that were not grown in this environment.
#' 
#' @details
#' The estimations of the values are based on the MCMC outputs.
#' More informations can be found in the vignette (type vignette("PPBstats")).
#' 
#' Due to memory issues, it may be better to choose output.format = "summary".
#' This allows caterpillar plots, barplots and interaction plots but no mean comparisons that are based on the whole MCMC.
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{FWH}}, \code{\link{analyse.outputs}}
#' 
#' 
predict_the_past_model_2 = function(
  out_check_model_model_2,
  env = NULL
)
  # let's go !!! ----------
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
  OUT = NULL
  if(length(germ) > 0) { # if length(geno) == 0, it means no germplasms must be estimated
    pb <- txtProgressBar(min = 0, max = length(germ), style = 3)
    for (i in 1:length(germ)) {
      if (is.element(paste("alpha","[",germ[i],"]",sep=""), colnames(MCMC)) & is.element(paste("theta","[",env,"]",sep=""), colnames(MCMC)))  {
        mu_estimated = MCMC[,paste("alpha[",germ[i],"]",sep="")] + MCMC[,paste("beta[",germ[i],"]",sep="")] * MCMC[,paste("theta[",env,"]",sep="")]
        OUT = rbind.data.frame(OUT, mu_estimated)
      } else { warning("Estimated value for germplasm ", germ, " in environment ", env, " is not possible. This is because the estimation of germplasm and or environment effects did not converge and therefore were not in the MCMC.") 
      }
      setTxtProgressBar(pb, i)
    }
    names(OUT) = paste("[", germ, ",", env,"]", sep = "")
  } else { OUT = NULL }
  
  attributes(OUT)$PPBstats.object = "predict.the.past"
  return(OUT)
}
