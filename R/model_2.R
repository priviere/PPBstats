# 0. help ----------
#' Run model 2 to get main germplasm, environment and sensitivity effects over the network.
#'
#' @description
#' \code{FWH} runs model 2 known as the Finlay Wilkinson Hierarchical (FWH) model to get main germplasm, environment and sensitivity effects over the network
#' 
#' @param data The data frame on which the model will be run. It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "..."), with "..." the variables.
#'  
#' @param variable The variable on which runs the model
#' 
#' @param nb_iterations Number of iterations of the MCMC
#' 
#' @param thin thinning interval to reduce autocorrelations between samples of the MCMC
#' 
#' @param return.alpha Return the value for each germplasm main effect (alpha_i)
#' @param return.sigma_alpha Return the value of the variance of the distribution where the alpha_i come from
#' 
#' @param return.beta Return the value for each sensitivity to environments (beta_i)
#' @param return.sigma_beta Return the value of the variance of the distribution where the beta_i come from
#' 
#' @param return.theta Return the value for each environment main effect (theta_j)
#' @param return.sigma_theta Return the value of the variance of the distribution where the theta_j come from
#'
#' @param return.epsilon Return the value of the residuals of the model (epsilon_ij)
#' @param return.sigma_epsilon Return the value of the variance of the distribution where the epsilon_ij come from 
#' 
#' @param return.DIC Return the DIC value of the model. See details for more informations.
#' 
#' @details
#' 
#' Model 2 estimates germplasm (alpha_i), environment (theta_j) and sensitivity to interaction (beta_i) effects. 
#' An environment is a combinaison of a location and a year.
#' 
#' The different effects are taken in different distributions of respective variances sigma_alpha, sigma_theta and sigma_beta. 
#' This model takes into acount all the information on the network in order to cope with the high disequilibrium in the dataset (i.e. high percentage of missing GxE combinaisons on the network). 
#' 
#' First, the additive model is done. This model gives intitial values of some parameters of the FWH model which is done next.
#' 
#' The model is run on data set where germplasms are on at least two environments
#' 
#' More informations can be found in the vignette (type vignette("PPBstats")).
#' 
#' For DIC value, see ?\code{dic.samples} from the \code{rjags} package for more information.
#' 
#' @return The function returns a list with 
#' 
#' \itemize{
#' \item "data.presence.abscence.matrix": a matrix germplasm x environment with the number of occurence in the data
#' \item "model.presence.abscence.matrix": a matrix germplasm x environment with the number of occurence in the data used for the model (i.e. with at least two germplasm by environments.)
#' \item "germplasm.not.used": the vector of germplasms not used in the analysis because they were not on at least two environments. If NULL, all the germplasms were used in the analysis.
#' \item "MCMC": a list with the two MCMC chains (mcmc object) from the FWH model
#' \item "epsilon": a vector with the median value of the epsilon_ijk
#' \item "DIC": the DIC value of the FWH model
#' }
#' 
#' @author Pierre Riviere for R code and Olivier David for JAGS code
#' 
#' @references
#' P. Riviere, J.C. Dawson, I. Goldringer, and O. David. Hierarchical multiplicative modeling of genotype x environment interaction for flexible experiments in decentralized participatory plant breeding. In prep, 2015.
#' 
#' @seealso \code{\link{analyse.outputs}}, \code{\link{cross.validation.FWH}}, \code{\link{predict.the.past}}
#' 
FWH = function(
  data,
  variable,
  nb_iterations = 100000,
  thin = 10,
  return.alpha = TRUE,
  return.sigma_alpha = TRUE,
  return.beta = TRUE,
  return.sigma_beta = TRUE,
  return.theta = TRUE,
  return.sigma_theta = TRUE,
  return.epsilon = FALSE,
  return.sigma_epsilon = TRUE,
  return.DIC = FALSE
)
# let's go !!! ----------
{
  # 1. Error message and update arguments ----------
  check_data_vec_variables(data, variable)

  if(nb_iterations < 20000) { warning("nb_iterations is below 20 000, which seems small to get convergence in the MCMC.")  }
  
  # Get the parameters to estimate
  parameters = NULL # be careful, the parameters should be in the alphabetic order
  
  if(return.epsilon) { return.sigma_epsilon = TRUE } # Useful to standardized residuals in analyse.outputs
  
  if(return.alpha) {parameters = c(parameters, "alpha")}  
  if(return.beta) {parameters = c(parameters, "beta")}  
  if(return.sigma_alpha) {parameters = c(parameters, "sigma_alpha")}
  if(return.sigma_beta) {parameters = c(parameters, "sigma_beta")}
  if(return.sigma_epsilon) {parameters = c(parameters, "sigma_epsilon")}
  if(return.sigma_theta) {parameters = c(parameters, "sigma_theta")}
  if(return.theta) {parameters = c(parameters, "theta")}  
  if(is.null(parameters) & return.epsilon == FALSE) { stop("You should choose at least one parameter to return: y, alpha, sigma_alpha, beta, sigma_beta, theta, sigma_theta, espilon or sigma_epsilon.") }  
  
  # 2. Data formating ----------
  
  # 2.1. Get the environments ----------
  environment = paste(as.character(data$location), as.character(data$year), sep = ":")
  
  D = cbind.data.frame(
    germplasm = as.factor(as.character(data$germplasm)),
    environment = as.factor(environment),
    variable = as.numeric(as.character(data[,variable]))
  )
  
  formule = as.formula("variable ~ germplasm + environment")
  D = droplevels(aggregate(formule, FUN = mean, data = D))
  
  # 2.2. Get only germplasm that are on at least two environments ----------
  data.presence.abscence.matrix = with(D, table(germplasm, environment))
  t = apply(data.presence.abscence.matrix, 1, sum)
  germ.to.get = names(t[which(t>=2)])
  germplasm.to.get = is.element(D$germplasm, germ.to.get)
  germplasm.not.used = D$germplasm[!is.element(D$germplasm, germ.to.get)]
  if( length(germplasm.not.used) == 0 ) { germplasm.not.used = NULL }
  D = droplevels(D[germplasm.to.get,])

  model.presence.abscence.matrix = with(D, table(germplasm, environment))
  
  # 3. Get the informations for the model ----------
  D = D[order(D$germplasm, D$environment),]
  
  germplasm = as.character(D$germplasm)
  nb_germplasm = length(unique(germplasm))
  
  environment = as.character(D$environment)
  nb_environment = length(unique(environment))
  
  y = as.numeric(as.character(D$variable))
  nu = mean(y, na.rm = TRUE)
  
  # Transform names with numbers to be ok with jags
  
  l = unique(environment)
  environment.names.data = l; names(environment.names.data) = seq(1, length(l), 1)
  environment.names.jags = seq(1, length(l), 1); names(environment.names.jags) = l
  environment = as.factor(environment.names.jags[environment])
  
  g = unique(germplasm)
  germplasm.names.data = g; names(germplasm.names.data) = seq(1, length(g), 1)
  germplasm.names.jags = seq(1, length(g), 1); names(germplasm.names.jags) = g
  germplasm = as.factor(germplasm.names.jags[germplasm])
  
  d <- list(y = y, germplasm = germplasm, nb_germplasm = nb_germplasm, environment = environment, nb_environment = nb_environment, nu = nu)
  
  # 4. Write and run the additive model to get initial value for mu (mean of alpha_i), sigma_alpha and sigma_theta of the FWH model ----------
  message("Run additive model ...")
  
  model_add_jags ="
  model
  {
  # vraisemblance
  for (i in 1:length(y))
    {
    y[i] ~ dnorm(y_hat[i],tau_epsilon)
    y_hat[i] <- alpha[germplasm[i]] + theta[environment[i]]
    }
  
  # prior
  for (i in 1:nb_germplasm) { alpha[i] ~ dnorm(mu,tau_alpha) }
  
  for (i in 1:nb_environment) { theta[i] ~ dnorm(0,tau_theta) }
  
  tau_epsilon ~ dgamma(1.0E-6,1.0E-6)
  sigma_epsilon <- 1/sqrt(tau_epsilon)
  tau_alpha <- 1/pow(sigma_alpha,2)
  sigma_alpha ~ dunif(0,nu)
  tau_theta <- 1/pow(sigma_theta,2)
  sigma_theta ~ dunif(0,nu)
  mu ~ dnorm(nu,1/pow(nu,2))
  }"
  
  # Initial values
  init1 <- list(".RNG.name"="base::Mersenne-Twister",".RNG.seed"=1234)  
  init2 <- list(".RNG.name"="base::Wichmann-Hill",".RNG.seed"=5678)
  init <- list(init1,init2)
  
  # Model
  model <- jags.model(file = textConnection(model_add_jags), data = d, inits = init, n.chains = 2)    
  
  update(model,1000) # Burn-in
  
  # Run the model
  mcmc_add <- coda.samples(model, c("mu", "sigma_alpha", "sigma_theta"), n.iter = nb_iterations, thin = thin)
  
  # Rename the parameters
  # one again, the name of the parameters must be in the alphabetic order
  n = colnames(mcmc_add[[1]])
  
  para.name = NULL
  para.name = c(para.name, "mu")  
  para.name = c(para.name, "sigma_alpha")
  para.name = c(para.name, "sigma_theta")
  
  colnames(mcmc_add[[1]]) = colnames(mcmc_add[[2]]) = para.name    
  
  # Get the initial value for the FWH model
  mcmc_add_tmp = rbind.data.frame(mcmc_add[[1]], mcmc_add[[2]])
  
  mu.init = median(mcmc_add_tmp[,grep("mu", colnames(mcmc_add_tmp))], na.rm = TRUE)
  sigma_alpha.init = median(mcmc_add_tmp[,grep("sigma_alpha", colnames(mcmc_add_tmp))], na.rm = TRUE)
  sigma_theta.init = median(mcmc_add_tmp[,grep("sigma_theta", colnames(mcmc_add_tmp))], na.rm = TRUE)
  
  # 5. Write and run the Finlay Wilkinson Hierarchical (FWH) model ----------
  message("Run FWH model ...")
  
  model_FWH_jags ="
  model
  {
  # vraisemblance
  for (i in 1:length(y))
    {
    y[i] ~ dnorm(y_hat[i],tau_epsilon)
    y_hat[i] <- alpha[germplasm[i]] + beta[germplasm[i]]*theta[environment[i]]
    epsilon[i] <- y[i] - y_hat[i]
    }
  
  # prior
  for (i in 1:nb_germplasm)
    {
    alpha[i] ~ dnorm(mu,tau_alpha)
    beta[i] ~ dnorm(1,tau_beta)
    }
  
  for (i in 1:nb_environment) { theta[i] ~ dnorm(0,tau_theta) }
  
  tau_epsilon ~ dgamma(1.0E-6,1.0E-6)
  sigma_epsilon <- 1/sqrt(tau_epsilon)
  
  tau_alpha <- 1/pow(sigma_alpha,2)
  sigma_alpha ~ dunif(0,nu)
  
  tau_theta <- 1/pow(sigma_theta,2)
  sigma_theta ~ dunif(0,nu)
  
  tau_beta <- 1/pow(sigma_beta,2)
  sigma_beta ~ dunif(0,1)
  
  mu ~ dnorm(nu,1/pow(nu,2))
  }
  "
  
  # Initial values with outputs frmo the additive model
  init1 <- list(".RNG.name"="base::Mersenne-Twister", ".RNG.seed" = 1234, sigma_alpha = sigma_alpha.init, mu = mu.init, sigma_theta = sigma_theta.init)  
  init2 <- list(".RNG.name"="base::Wichmann-Hill", ".RNG.seed" = 5678, sigma_alpha = sigma_alpha.init, mu = mu.init, sigma_theta = sigma_theta.init)
  init <- list(init1, init2)
  
  # Model
  model <- jags.model(file = textConnection(model_FWH_jags), data = d, inits = init, n.chains = 2)
  
  # DIC for the FWH model
  if(return.DIC) {
    message("Calculation of DIC for FWH model ...")
    DIC_FW = dic.samples(model, n.iter = nb_iterations, thin = 10, type = "pD")
  } else {DIC_FW = NULL}
  
  if( !is.null(parameters) ){
    update(model,1000) # Burn-in
    
    # Run the model
    mcmc_fwh <- coda.samples(model, parameters, n.iter = nb_iterations, thin = thin)
    
    # Rename the parameters
    # one again, the name of the parameters must be in the alphabetic order
    n = colnames(mcmc_fwh[[1]])
    
    para.name = NULL
    
    if(return.alpha) {
      alpha = n[grep("alpha\\[", n)]
      alpha = sub("\\]", "", sub("alpha\\[", "", alpha))
      alpha = paste("alpha[", germplasm.names.data[as.character(alpha)], "]", sep = "") # be careful, it must be as.character(alpha) to grep the right column name, otherwise, it is the nth position instead! 
      para.name = c(para.name, alpha)
    }  
    
    if(return.beta) {
      beta = n[grep("beta\\[", n)]
      beta = sub("\\]", "", sub("beta\\[", "", beta))
      beta = paste("beta[", germplasm.names.data[as.character(beta)], "]", sep = "") # be careful, cf up 
      para.name = c(para.name, beta)
    }  
    
    if(return.sigma_alpha) { para.name = c(para.name, "sigma_alpha") }
    
    if(return.sigma_beta) { para.name = c(para.name, "sigma_beta") }
    
    if(return.sigma_epsilon) { para.name = c(para.name, "sigma_epsilon") }
    
    if(return.sigma_theta) { para.name = c(para.name, "sigma_theta") }
    
    if(return.theta) {
      theta = n[grep("theta\\[", n)]
      theta = sub("\\]", "", sub("theta\\[", "", theta))
      theta = paste("theta[", environment.names.data[as.character(theta)], "]", sep = "") # be careful, cf up 
      para.name = c(para.name, theta)
    } 
    
    colnames(mcmc_fwh[[1]]) = colnames(mcmc_fwh[[2]]) = para.name
    
    # Correct the value of alpha, see vignette("PPBstats") for more details.
    if( is.element("alpha", parameters) & is.element("theta", parameters)) {
      MCMC1_fwh = as.data.frame(mcmc_fwh[[1]])
      MCMC2_fwh = as.data.frame(mcmc_fwh[[2]])
      
      vec_alpha = colnames(MCMC1_fwh)[grep("alpha\\[", colnames(MCMC1_fwh))]
      vec_beta = colnames(MCMC1_fwh)[grep("beta\\[", colnames(MCMC1_fwh))]
            
      theta1_bar = apply(MCMC1_fwh[,grep("theta", colnames(MCMC1_fwh))], 1, median)
      MCMC1_fwh[,vec_alpha] = MCMC1_fwh[,vec_alpha] + MCMC1_fwh[,vec_beta]*theta1_bar
      
      theta2_bar = apply(MCMC2_fwh[,grep("theta", colnames(MCMC2_fwh))], 1, median)
      MCMC2_fwh[,vec_alpha] = MCMC2_fwh[,vec_alpha] + MCMC2_fwh[,vec_beta]*theta2_bar
      
      mcmc_fwh = mcmc.list(as.mcmc(MCMC1_fwh), as.mcmc(MCMC2_fwh))
    }
  } else { mcmc_fwh = NULL }
  
  
  # 6. For epsilon, it is done alone otherwise the memory do not manage with too big MCMC data frame. It is also useful for the cross validation study  ----------  
  if(return.epsilon) { 
    mcmc_epsilon <- coda.samples(model, "epsilon", n.iter= nb_iterations, thin = thin)
    MCMC_epsilon = rbind.data.frame(as.data.frame(mcmc_epsilon[[1]]), as.data.frame(mcmc_epsilon[[2]]))
    epsilon = apply(MCMC_epsilon, 2, median, na.rm = TRUE)
  } else {epsilon = NULL}
  
  OUT = list(
    "data.presence.abscence.matrix" = data.presence.abscence.matrix,
    "model2.presence.abscence.matrix" = model.presence.abscence.matrix,
    "germplasm.not.used" = germplasm.not.used,
    "MCMC" = mcmc_fwh,
    "epsilon" = epsilon,
    "DIC" = DIC_FW
  )
  
  attributes(OUT)$PPBstats.object = "model2"
  return(OUT)
}
