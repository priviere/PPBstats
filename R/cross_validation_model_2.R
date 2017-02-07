#' Run complete cross validation with model 2
#'
#' @description
#' \code{cross_validation_model_2} runs complete cross validation with model 2
#' 
#' @param data The data frame on which the model will be run. It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "..."), with "..." the variables.
#'  
#' @param variable The variable on which runs the model
#' 
#' @param nb_iterations Number of iterations of the MCMC
#' 
#' @param mc.cores The number of cores used for parallelisation of the computing
#' 
#' @details
#' 
#' The convergence is not checked for each validation. If the parameters converge (cf \code{check_model}), then it is assumed that the parameters in the cross validation converge as well.
#' 
#' The model is run on data sets where germplasms are on at least three environments so the smallest data set where the cross validation is run has germplasms on at least two environments. 
#' 
#' Parallelization is done with the function \code{parallel::mclapply}.
#' 
#' @return 
#' The function returns a data frame with real values and estimated values by cross validation
#' 
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{model_2}}, \code{\link{check_model}}, \code{\link{check_model_model_2}}
#' 
#'
cross_validation_model_2 = function(
  data, 
  variable, 
  nb_iterations = 100000,
  mc.cores = 1)
{
  # 1. Error message ----------
  check_data_vec_variables(data, variable)
  
  # The other error messages are done with the used of the function model_2
  
  # 2. Data formating ----------
  
  # 2.1. Get the environments ----------
  environment = paste(as.character(data$location), as.character(data$year), sep = ":")
  
  D = cbind.data.frame(
    germplasm = as.factor(as.character(data$germplasm)),
    year = as.factor(as.character(data$year)),
    location = as.factor(as.character(data$location)),
    environment = as.factor(environment),
    block = as.factor(as.character(data$block)),
    X = as.factor(as.character(data$X)),
    Y = as.factor(as.character(data$Y)),
    variable = as.numeric(as.character(data[,variable]))
  )
  
  formule = as.formula("variable ~ germplasm + year + location + environment + block + X + Y")
  D = droplevels(aggregate(formule, FUN = mean, data = D))
  
  
  # 2.2. Get only germplasm that are on at least three environments ----------
  w = with(D, table(germplasm, environment))
  t = apply(w, 1, sum)
  germ.to.get = names(t[which(t>=3)])
  germplasm.to.get = is.element(D$germplasm, germ.to.get)
  D = droplevels(D[germplasm.to.get,])
  
  # 3. Create datasets ----------
  list_data = NULL
  for(i in 1:nrow(D)) {
    d = D[-i,]
    env = as.character(D[i, "environment"])
    germ = as.character(D[i, "germplasm"])
    observed.value = d[i, "variable"]
    
    out = list("data" = d, "num_data" = i, "nb_total_data" = nrow(D), "observed.value" = observed.value, "germplasm" = germ, "environment" = env, "nb_iterations" = nb_iterations)
    list_data = c(list_data, list(out))
  }

  # 4. Run the FWH model on each data set ----------
  fun = function(x) {
    
    message("
            ------------------------------------------------------------\n
            ------------------------------------------------------------\n
            Model runs for data ", x$num_data, "/", x$nb_total_data, ". 
            ------------------------------------------------------------\n
            ------------------------------------------------------------\n
            ")
    
    out = model_2(  data = x$data,
                variable = "variable",
                nb_iterations = x$nb_iterations,
                return.alpha = TRUE,
                return.sigma_alpha = FALSE,
                return.beta = TRUE,
                return.sigma_beta = FALSE,
                return.theta = TRUE,
                return.sigma_theta = FALSE,
                return.epsilon = FALSE,
                return.sigma_epsilon = FALSE,
                return.DIC = FALSE
    )
    
    germ = x$germplasm
    env = x$environment
    
    MCMC = rbind.data.frame(out$MCMC[[1]], out$MCMC[[2]])
    estimated.value = median(MCMC[, paste("alpha[", germ, "]", sep = "")] + MCMC[, paste("beta[", germ, "]", sep = "")] * MCMC[, paste("theta[", env, "]", sep = "")])
    
    out = c("observed.value" = x$observed.value, "estimated.value" = estimated.value)
    return(out)
  }
  
  OUT = mclapply(list_data, function(x) {fun(x)}, mc.cores = mc.cores)
  
  observed.value = unlist(OUT)[grep("observed.value", names(unlist(OUT)))]
  estimated.value = unlist(OUT)[grep("estimated.value", names(unlist(OUT)))]
    
  # 5. Return results ----------
  
  out = cbind.data.frame(observed.value, estimated.value)
  
  attributes(out)$PPBstats.object = "cross_validation_model_2"
  
  return(out)
}

