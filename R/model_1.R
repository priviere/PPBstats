# 0. help ----------
#' Run model 1 to get mean comparisons on each environment of the network.
#'
#' @description
#' \code{model_1} runs model 1 to get mean comparisons on each environment of the network. See details for more information.
#'
#' @param data The data frame on which the model will be run. It should have at least the following columns : c("year", "germplasm", "location", "block", "X", "Y", "..."), with "..." the variables.
#'  
#' @param variable The variable on which runs the model
#' 
#' @param nb_iterations Number of iterations of the MCMC
#' 
#' @param thin thinning interval to reduce autocorrelations between samples of the MCMC
#' 
#' @param return.mu Return the value for each entry in each environment (mu_ij)
#' 
#' @param return.beta Return the value for each block in each environment (beta_jk)
#' 
#' @param return.sigma Return the value for each within-environment variance (sigma_j)
#'
#' @param return.nu Return the value of nu
#'
#' @param return.rho Return the value of rho
#' 
#' @param return.epsilon Return the value of all residuals in each environment (epsilon_ijk)
#' 
#' @param return.DIC Return the DIC value of the model. See details for more information.
#' 
#' @param nu.max Set the nu.max. It is 10 by default
#' @details
#' 
#' Model 1 estimates entry effects (mu_ij), block effects (beta_jk), residuals (epsilon_ijk) and within-environment variance (sigma_j) on each environment. 
#' An environment is a combinaison of a location and a year.
#' 
#' The variance are taken in an inverse Gamma distribution of parameters nu and rho. 
#' This model takes into acount all the information on the network in order to cope wit
#' h the high disequilibrium within each environment (i.e. low degree of freedom at the residual in each environment). 
#' More informations can be found in the vignette (type vignette("PPBstats")).
#' 
#' For DIC value, see ?\code{dic.samples} from the \code{rjags} package for more informations.
#' 
#' @return The function returns a list with 
#' 
#' \itemize{
#' \item "data.model1": the dataframe used to run model 1
#' \item "presence.abscence.matrix": a matrix entry x environment with the number of occurence
#' \item "vec_env_with_no_data": a vector with the environments without data for the given variable
#' \item "vec_env_with_no_controls": a vector with the environments with no controls
#' \item "data_env_with_no_controls": a dataframe with the data from environments without controls.
#' \item "vec_env_with_controls": a vector with the environments with controls
#' \item "vec_env_RF": a vector with the environments as regional farms (i.e. with at least two blocks)
#' \item "vec_env_SF": a vector with the environments as satellite farms (i.e. with one block)
#' \item "MCMC": a list with the two MCMC chains (mcmc object)
#' \item "epsilon": a vector with the median value of the epsilon_ijk
#' \item "DIC": the DIC value of the model
#' }
#' 
#' @author Pierre Riviere for R code and Olivier David for JAGS code
#' 
#' @references
#' P. Riviere, J.C. Dawson, I. Goldringer, and O. David. Hierarchical Bayesian Modeling for Flexible Experiments in Decentralized Participatory Plant Breeding. Crop Science, 55, 2015.
#' 
#' @seealso \code{\link{analyse.outputs}}
#' 
#'   
model_1 = function(
  data,
  variable,
  nb_iterations = 100000,
  thin = 10,
  return.mu = TRUE,
  return.beta = TRUE,
  return.sigma = TRUE,
  return.nu = TRUE,
  return.rho = TRUE,
  return.epsilon = FALSE,
  return.DIC = FALSE,
  nu.max = 10
)
# let's go !!! ----------
{
  # 1. Error message and update arguments ----------
  check_data_vec_variables(data, variable)
  
  if(nb_iterations < 20000) { warning("nb_iterations is below 20 000, which seems small to get convergence in the MCMC.")  }
  
  # Get the parameters to estimate
  parameters = NULL # be careful, the parameters should be in the alphabetic order
  
  if(return.epsilon) { return.sigma = TRUE } # Useful to standardized residuals in analyse.outputs
  
  if(return.beta) {parameters = c(parameters, "beta")}
  if(return.mu) {parameters = c(parameters, "mu")}
  if(return.nu) {parameters = c(parameters, "nu")}
  if(return.rho) {parameters = c(parameters, "rho")}
  if(return.sigma) {parameters = c(parameters, "sigma")}
  if(is.null(parameters) & return.epsilon == FALSE) { stop("You should choose at least one parameter to return: mu, beta, nu, rho, sigma or epsilon.") }
  
  # 2. Data formating ----------
  environment = paste(as.character(data$location), as.character(data$year), sep = ":")
  
  D = cbind.data.frame(
    germplasm = as.factor(as.character(data$germplasm)),
    environment = as.factor(environment),
    entry = as.factor(paste(as.character(data$germplasm), environment, sep = ",")),
    block = as.factor(as.character(data$block)),
    X = as.factor(as.character(data$X)),
    Y = as.factor(as.character(data$Y)),
    variable = as.numeric(as.character(data[,variable]))
  )

  # Mean for each entry (i.e. each germplasm in each environment in each block)
  D$ID = paste(D$entry, D$germplasm, D$environment, D$block, D$X, D$Y, sep = ":")
  formule = as.formula("variable ~ entry + germplasm + environment + block + X + Y + ID")
  DD = droplevels(aggregate(formule, FUN = mean, data = D, na.action = na.omit))

  data.model1 = DD
  data.model1$parameter = paste("[", data.model1$germplasm, ",", data.model1$environment, "]", sep = "") # To have a compatible format for get.ggplot
  attributes(data.model1)$PPBstats.object = "data.model1"
  
  # Get regional farms (RF) and satellite farms (SF)
  out = get.env.info(DD, nb_ind = 1)
  vec_env_with_no_data = out$vec_env_with_no_data

  vec_env_with_no_controls = out$vec_env_with_no_controls
  data_env_with_no_controls = droplevels(filter(DD, environment %in% vec_env_with_no_controls))
  data_env_with_no_controls$parameter = paste("[", data_env_with_no_controls$germplasm, ",", data_env_with_no_controls$environment, "]", sep = "") # To have a compatible format for get.ggplot
  data_env_with_no_controls$location = sapply(data_env_with_no_controls$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
  data_env_with_no_controls$year = sapply(data_env_with_no_controls$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
  data_env_with_no_controls = data_env_with_no_controls[,-which(colnames(data_env_with_no_controls) == "ID")]
  
  
  attributes(data_env_with_no_controls)$PPBstats.object = "data_env_with_no_controls.model1"
  
  vec_env_with_controls = out$vec_env_with_controls
  vec_env_RF = out$vec_env_RF
  vec_env_SF = out$vec_env_SF
  D_RF = out$D_RF
  D_SF = out$D_SF
  
  presence.abscence.matrix = with(rbind.data.frame(D_RF, D_SF), table(entry, environment))
  
  if( length(vec_env_with_controls) == 0 ) { stop("There are no controls on any environment so the model can not be run.") }
  
  # 3. Get the informations for the model ----------
  if( !is.null(D_RF) ) {
    environment_RF = as.character(D_RF$environment)
    block.temp = as.character(D_RF$block)
    block_RF = paste(environment_RF, block.temp, sep = ",")
    entry_RF = as.character(D_RF$entry) 
    y_RF = D_RF$variable
  } else { environment_RF = block_RF = entry_RF = y_RF = NULL }
  
  if( !is.null(D_SF) ) {
    environment_SF = as.character(D_SF$environment)
    block.temp = as.character(D_SF$block)
    block_SF = paste(environment_SF, block.temp, sep = ",")
    entry_SF = as.character(D_SF$entry) 
    y_SF = D_SF$variable
  } else { environment_SF = block_SF = entry_SF = y_SF = NULL }
  
  environment = c(environment_RF, environment_SF)
  block = c(block_RF, block_SF)
  entry = c(entry_RF, entry_SF)  
  y = c(y_RF, y_SF)
    
  # Transform names with numbers to be ok with jags
  
  l = unique(environment)
  environment.names.data = l; names(environment.names.data) = seq(1, length(l), 1)
  environment.names.jags = seq(1, length(l), 1); names(environment.names.jags) = l
  environment = as.factor(environment.names.jags[environment])
  
  b = unique(block)
  block.names.data = b; names(block.names.data) = seq(1, length(b), 1)
  block.names.jags = seq(1, length(b), 1); names(block.names.jags) = b
  block = as.factor(block.names.jags[block])
  
  e = entry
  entry.names.data = e ; names(entry.names.data) = as.numeric(factor(entry))
  entry.names.jags = as.numeric(factor(entry)); names(entry.names.jags) = e
  entry = as.factor(entry.names.jags[e])
  
  mean_prior_mu = tapply(y, environment, mean, na.rm = TRUE) # mean of y in each environment to be in the prior
  mean_prior_mu = mean_prior_mu[environment]
  
  nb_env = nlevels(environment)
  nb_entry = nlevels(entry)
  
  if( !is.null(D_RF) ) { nb_RF = nrow(D_RF) } else { nb_RF = 0 }
  if( !is.null(D_SF) ) { nb_SF = nrow(D_SF) } else { nb_SF = 0 }
  
  # The data for the model are concatenate in the next part according to nb_RF and nb_SF
  
  # 4. Write and run the model ----------
  
  # 4.1. likelyhood ----------
  # mean : the mean of the observations in the Normal distribution
  # mu : the parameter mu_ij  
  
  likelyhood_model_jags_RF = "
  for (i in 1:nb_RF) # regional farm
    {
    y[i] ~ dnorm(mean[i],tau[environment[i]]) # data y[i] of mean mean[i] and a variance depending of the trial (tau[environment[i]])
    mean[i] <- mu[entry[i]]+beta[block[i]]
    epsilon[i] <- (y[i] - mean[i])
    }
  "
  
  likelyhood_model_jags_SF = "
  for (i in (nb_RF+1):length(environment)) # satellite farms
    {
    y[i] ~ dnorm(mean[i],tau[environment[i]])
    mean[i] <- mu[entry[i]]
    epsilon[i] <- (y[i] - mean[i])
    }
  "
  
  # 4.2. priors. when E-6, it is E6 in jags ----------
  # For each environment, get the number of blocks
  a = unique(cbind.data.frame(environment, block))
  
  BETA = NULL
  for(e in unique(a$environment)) {
    
    ddd = filter(a, environment == e)
    b = ddd[,"block"]
    
    if(length(b) > 1) { # For environments with blocks
      BETA = c(BETA,
               paste("for(i in ", b[1], ":", b[length(b) - 1], "){beta[i] ~ dnorm(0,1.0E-6)} \n", sep = ""),
               paste("beta[", b[length(b)], "] <- -sum(beta[", b[1], ":", b[length(b) - 1], "]) \n", sep = "")
      )
    }
  }
  
  BETA = paste(BETA, collapse = " ")
  
  priors_model_jags = paste("
                            for (i in 1:nb_entry) { mu[i] ~ dnorm(mean_prior_mu[i],1.0E-6)} # distribution of the entry in each environment \n",
                            BETA, 
                            "for (i in 1:nb_env) { tau[i] ~ dgamma(nu,rho) } # distribution of the variances in each environment
                            
                            nu ~ dunif(2,",nu.max,")
                            rho ~ dgamma(1.0E-6,1.0E-6)
                            
                            # sqrt(within environmental variance) = standard deviation
                            for (i in 1:nb_env) { sigma[i] <- 1/sqrt(tau[i]) }
                            ")
  
  if( nb_RF > 0 & nb_SF > 0) {
    d_model <- list(environment = environment, block = block, entry = entry, y = y, nb_env = nb_env, nb_entry = nb_entry, nb_RF = nb_RF, mean_prior_mu = mean_prior_mu)    
    model_jags = paste("model {", likelyhood_model_jags_RF, likelyhood_model_jags_SF, priors_model_jags, "}")    
  }
  
  if( nb_RF > 0 & nb_SF == 0) {
    d_model <- list(environment = environment, block = block, entry = entry, y = y, nb_env = nb_env, nb_entry = nb_entry, nb_RF = nb_RF, mean_prior_mu = mean_prior_mu)
    model_jags = paste("model {", likelyhood_model_jags_RF, priors_model_jags, "}")
  }
  
  if( nb_RF == 0 & nb_SF > 0) {
    d_model <- list(environment = environment, entry = entry, y = y, nb_env = nb_env, nb_entry = nb_entry, nb_RF = nb_RF, mean_prior_mu = mean_prior_mu)
    model_jags = paste("model {", likelyhood_model_jags_SF, priors_model_jags, "}")
  }
  
  
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
  
  
  if( !is.null(parameters) ){
    update(model, 1000) # Burn-in
    
    # run the model
    mcmc = coda.samples(model, parameters, n.iter = nb_iterations, thin = thin)
    
    # 5. Rename the parameters ----------
    # once again, the name of the parameters must be in the alphabetic order
    n = colnames(mcmc[[1]])
    
    para.name = NULL
    
    if(return.beta) {
      beta = n[grep("beta", n)]
      beta = sub("beta\\[", "", beta)
      beta = sub("\\]", "", beta)
      beta = paste("beta[", block.names.data[as.character(beta)], "]", sep = "") # be careful, it must be as.character(beta) to grep the right column name, otherwise, it is the nth position instead! 
      para.name = c(para.name, beta)
    }
    
    if(return.mu) {
      mu = n[grep("mu", n)]
      mu = sub("mu\\[", "", mu)
      mu = sub("\\]", "", mu)
      mu = paste("mu[", entry.names.data[as.character(mu)], "]", sep = "") # be careful, cf up
      para.name = c(para.name, mu)
    }
    
    if(return.nu) { para.name = c(para.name, "nu") }
    if(return.rho) { para.name = c(para.name, "rho") }
    
    if(return.sigma) {
      sigma = n[grep("sigma", n)]
      sigma = sub("sigma\\[" ,"", sigma)
      sigma=sub("\\]", "", sigma)
      sigma = paste("sigma[", environment.names.data[as.character(sigma)], "]", sep = "")  # be careful, cf up
      para.name = c(para.name, sigma)
    }
    
    colnames(mcmc[[1]]) = colnames(mcmc[[2]]) = para.name

  }
  
  # 6. For residuals, it is done alone otherwise the memory do not manage with too big MCMC data frame ----------
  if(return.epsilon) {
    mcmc_res <- coda.samples(model, "epsilon", n.iter= nb_iterations, thin = thin)
    
    mcmc1_res = mcmc_res[[1]]
    mcmc2_res = mcmc_res[[2]]
    MCMC_res = rbind.data.frame(as.data.frame(mcmc1_res), as.data.frame(mcmc2_res))
    
    MCMCres = MCMC_res[,grep("epsilon", colnames(MCMC_res))]
    epsilon = apply(MCMCres, 2, median, na.rm = TRUE)
    
    names(epsilon) = paste("epsilon[", names(entry.names.jags), "]", sep = "") # not really rigorous but ok for analysis.outputs (it misses the k in epsilon_ijk)
    
  } else {epsilon = NULL}
  
  # 7. Get the outptus ----------
  OUT = list(
    "data.model1" = data.model1,
    "data.presence.abscence.matrix" = presence.abscence.matrix,
    "vec_env_with_no_data" = vec_env_with_no_data,
    "vec_env_with_no_controls" = vec_env_with_no_controls,
    "data_env_with_no_controls" = data_env_with_no_controls,
    "vec_env_with_controls" = vec_env_with_controls,
    "vec_env_RF" = vec_env_RF,
    "vec_env_SF" = vec_env_SF,
    "MCMC" = mcmc,
    "epsilon" = epsilon,
    "DIC"= DIC
  )
  
  attributes(OUT)$PPBstats.object = "model_1"
  return(OUT)
}

