#' Run model variance_intra
#'
#' @description
#' \code{model_variance_intra} runs model to get intra-population variance on each environment of the network. See details for more information.
#'
#' @param data The data frame on which the model is run. It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "..."), with "..." the variables.
#'  
#' @param variable The variable on which runs the model
#' 
#' @param nb_iterations Number of iterations of the MCMC
#' 
#' @param thin thinning interval to reduce autocorrelations between samples of the MCMC
#' 
#' @param return.mu Return the value for each entry in each environment and each plot (mu_ijk)
#' 
#' @param return.sigma Return the value for each intra-population variance in each environment (sigma_ij)
#' 
#' @param return.epsilon Return the value of all residuals in each environment on each plot (epsilon_ijk)
#' 
#' @param return.DIC Return the DIC value of the model. See details for more information.
#' 
#' @details
#' 
#' Model on intra-population variance estimates entry effects (mu_ijk) and within-population variance (sigma_ij) on each environment. 
#' An environment is a combinaison of a location and a year.
#' 
#' The variance are taken in an inverse Gamma distribution of parameters 10^-6. 
#' More information can be found in the vignette.
#' 
#' For DIC value, see ?\code{dic.samples} from the \code{rjags} package for more information.
#' 
#' @return The function returns a list with 
#' 
#' \itemize{
#' \item "data.model_variance_intra": the dataframe used to run mode variance_intra
#' \item "MCMC": a list with the two MCMC chains (mcmc object)
#' \item "DIC": the DIC value of the model
#' }
#' 
#' @author Pierre Riviere and Gaelle van Frank for R code; Olivier David for JAGS code
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{check_model}}
#' \item \code{\link{check_model.model_variance_intra}}
#' }
#' 
model_variance_intra <- function(
  data,
  variable,
  nb_iterations = 100000,
  thin = 10,
  return.mu = TRUE,
  return.sigma = TRUE,
  return.epsilon = FALSE,
  return.DIC = FALSE
)
{
  # 1. Error message and update arguments ----------
  check_data_vec_variables(data, variable)
  
  if(nb_iterations < 20000) { warning("nb_iterations is below 20 000, which seems small to get convergence in the MCMC.")  }
  
  # Get the parameters to estimate
  parameters = NULL # be careful, the parameters should be in the alphabetic order

  if(return.mu) {parameters = c(parameters, "mu")}
  if(return.sigma) {parameters = c(parameters, "sigma_y")}
  if(is.null(parameters)) { stop("You should choose at least one parameter to return: mu or sigma.") }

  
  # 2. Data formating ----------
  # 2.1. Get the environments ----------
  environment = paste(data$location, data$year, sep = ":")
  #  entry=paste(data$germplasm, environment, data$ind, sep = ":")
  entry=paste(data$germplasm, environment, sep = ",")
  
  D = cbind.data.frame(
    germplasm = as.factor(data$germplasm),
    environment = as.factor(environment),
    entry = as.factor(entry),
    location=as.factor(data$location),
    year=as.factor(data$year),
    block = as.factor(data$block),
    X = as.factor(data$X),
    Y = as.factor(data$Y),
    variable = as.numeric(data[,variable]))
  
  
  D$ID = paste(D$germplasm, paste(D$environment, D$block, D$X, D$Y, sep = ":"),sep=",")
  D=D[!is.na(D$variable),]
 
  # 3. Get the informations for the model ----------
  germplasm = as.character(D$germplasm)
  environment = as.character(D$environment)
  block = as.character(D$block)
  entry = as.character(D$entry) 
  y = D$variable
  ID = D$ID

  # Transform names with numbers to be ok with jags
  b = unique(block)
  block.names.data = b; names(block.names.data) = seq(1, length(b), 1)
  block.names.jags = seq(1, length(b), 1); names(block.names.jags) = b
  block = as.numeric(as.factor(block.names.jags[block]))
  
  l = unique(environment)
  environment.names.data = l; names(environment.names.data) = seq(1, length(l), 1)
  environment.names.jags = seq(1, length(l), 1); names(environment.names.jags) = l
  environment = as.numeric(as.factor(environment.names.jags[environment]))
  
  g = unique(germplasm)
  germplasm.names.data = g; names(germplasm.names.data) = seq(1, length(g), 1)
  germplasm.names.jags = seq(1, length(g), 1); names(germplasm.names.jags) = g
  germplasm = as.numeric(as.factor(germplasm.names.jags[germplasm]))
  
  e = unique(entry)
  entry.names.data = e; names(entry.names.data) = seq(1, length(e), 1)
  entry.names.jags = seq(1, length(e), 1); names(entry.names.jags) = e
  entry = as.numeric(as.factor(entry.names.jags[entry]))
  
  id = unique(ID)
  ID.names.data = id; names(ID.names.data) = seq(1, length(id), 1)
  ID.names.jags = seq(1, length(id), 1); names(ID.names.jags) = id
  ID = as.numeric(as.factor(ID.names.jags[ID]))
  
  mean_prior_mu = tapply(y, environment, mean, na.rm = TRUE) # mean of y in each environment to be in the prior
  mean_prior_mu = mean_prior_mu[environment]

  
  nb_environment = max(environment)
  nb_entry=max(entry)
  nb_ID=max(ID)
	nb_germplasm = max(germplasm)
  

  # 4. Write and run the model ----------
  
  likelihood_model_jags = "
  for (i in 1:nb_y) {
  y[i] ~ dnorm(mu[ID[i]],tau_y[entry[i]])
  epsilon[i] <- (y[i] - mu[ID[i]])
  }
  "
  
  priors_model_jags = "
  for (i in 1:nb_ID) { 
  mu[i] ~ dnorm(mean_prior_mu[i],1.0E-6)}   
  # sigma_y depends of germplasm  and environment
  for (i in 1:nb_entry){
  tau_y[i] ~ dgamma(1.0E-6,1.0E-6) 
  sigma_y[i] <- pow(tau_y[i],-0.5)
  }
  "
  
  d_model <- list(y = y, nb_y = length(y), entry=entry,nb_entry=nb_entry,ID=ID,nb_ID=nb_ID, mean_prior_mu=mean_prior_mu)    
  
  model_jags = paste("model {", likelihood_model_jags, priors_model_jags, "}")    
  
  # Initial values
  init1 <- list(".RNG.name"="base::Mersenne-Twister", ".RNG.seed"=1234)
  init2 <- list(".RNG.name"="base::Wichmann-Hill", ".RNG.seed"=5678)
  init <- list(init1, init2)
  
  # Model
  model <- jags.model(file = textConnection(model_jags), data = d_model, inits = init, n.chains = 2)
  
  # DIC
  if(return.DIC) {
    message("Calculation of DIC ...")
    DIC = dic.samples(model, n.iter = nb_iterations, thin = thin, type = "pD")
  } else {DIC = NULL}
  
  update(model, 1000) # Burn-in
  
  # run the model
  mcmc = coda.samples(model, parameters, n.iter = nb_iterations, thin = thin)
  
  # 5. Rename the parameters ----------
  # once again, the name of the parameters must be in the alphabetic order
  n = colnames(mcmc[[1]])
  
  para.name = NULL

  if("mu" %in% parameters) {
    mu = n[grep("mu", n)]
    mu = sub("mu\\[", "", mu)
    mu = sub("\\]", "", mu)
    mu = paste("mu[", ID.names.data[as.character(mu)], "]", sep = "") # be careful, cf up
    para.name = c(para.name, mu)
  }

  if("sigma_y" %in% parameters) {
    sigma = n[grep("sigma_y", n)]
    sigma = sub("sigma_y\\[" ,"", sigma)
    sigma=sub("\\]", "", sigma)
    sigma = paste("sigma[", entry.names.data[as.character(sigma)], "]", sep = "")  # be careful, cf up
    para.name = c(para.name, sigma)
  }
  
  colnames(mcmc[[1]]) = colnames(mcmc[[2]]) = para.name
  
  # 6. For residuals, it is done alone otherwise the memory do not manage with too big MCMC data frame ----------
  if(return.epsilon) {
    mcmc_res <- coda.samples(model, "epsilon", n.iter= nb_iterations, thin = thin)
    
    mcmc1_res = mcmc_res[[1]]
    mcmc2_res = mcmc_res[[2]]
    MCMC_res = rbind.data.frame(as.data.frame(mcmc1_res), as.data.frame(mcmc2_res))
    
    MCMCres = MCMC_res[,grep("epsilon", colnames(MCMC_res))]
    epsilon = apply(MCMCres, 2, median, na.rm = TRUE)
    
    names(epsilon) = paste("epsilon[", names(ID.names.jags), "]", sep = "") # not really rigorous but ok for analysis.outputs (it misses the l in epsilon_ijkl)
    
  } else {epsilon = NULL}

  OUT = list(
    "data.model_variance_intra" = data,
    "MCMC" = mcmc,
    "epsilon" = epsilon,
    "DIC"= DIC
  )
  
  class(OUT) <- c("PPBstats", "fit_model_variance_intra")
  return(OUT)
}