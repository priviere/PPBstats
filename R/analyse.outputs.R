# 0. help ----------
#' Check with plots if the model went well based on the Gelman-Rubin test and plots of posteriors distributions 
#'
#' @description
#' \code{analyse.outputs} displays plots to see if the model went well based ont the Gelman-Rubin test and plots of posteriors distributions. It is important to run this step before going ahead in the analysis otherwise you may make mistakes in the interpretation of the results.
#'
#' @param out.model outputs from model 1 (\code{MC}) or model 2 (\code{FWH})
#'  
#' @param analysis "experimental.design", convergence" or "posteriors". If NULL, the three are done.
#' 
#' @param nb_parameters_per_plot The number of parameters per plot to facilitate the visualisation
#' 
#' @details
#' For analyse = "convergence", the test used is the Gelman-Rubin test. 
#' It may take some times to run.
#' More details with ?\code{gelman.diag} from the \code{coda} package. 
#' Note that for \code{gelman.diag}, the argument \code{multivariate = FALSE} is used.
#' If you wish exhaustive information, looked at \code{ggmcmc::ggmcmc} with \code{ggmcmc(out.model$MCMC)}. 
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
#'    \item "parameter_posteriors" : a caterpillar plot is display for each mu_ij, beta_jk for each environment and for sigma_j 
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
analyse.outputs = function(
out.model,
analysis = NULL,
nb_parameters_per_plot = 10
)
# let's go !!! ----------
{
# 1. Error message and update arguments ----------
if( is.null(attributes(out.model)$PPBstats.object) ) { stop("out.model should be an output from model 1 (PPBstats::MC) or model 2 (PPBstats::FWH).") } 


if(!is.null(analysis)) { 
  if( !is.element(analysis, c("experimental_design", "convergence", "posteriors")) ){ stop("analysis must be \"experimental_design\", \"convergence\" or \"posteriors\".") }  
  if( !is.element(analysis, c("convergence")) ){ warning("\"convergence\" is not chosen! You may make mistakes in the interpretation of the results !!!") }  
} else { analysis = "all" }

# Default settings
model1.data_env_whose_param_did_not_converge = NULL
model2.presence.abscence.matrix = out.model$model2.presence.abscence.matrix

# 1. experimental design ----------
out.experimental.design = NULL
if(analysis == "all" | analysis == "experimental_design") {

  m = out.model$data.presence.abscence.matrix
  
  if(attributes(out.model)$PPBstats.object == "model1"){
    d <- data.frame(germplasm = rep(row.names(m), ncol(m)), 
                    environment = rep(colnames(m), each=nrow(m)),
                    score = as.factor(as.character(as.vector(m)))
    )
    
  }

  if(attributes(out.model)$PPBstats.object == "model2"){
    d <- data.frame(germplasm = rep(row.names(m), ncol(m)), 
                    environment = rep(colnames(m), each=nrow(m)),
                    score = as.factor(as.character(as.vector(m)))
    )
    
  }
  
  nb_NA = round(length(which(d$score == 0)) / ( length(which(d$score == 0)) + length(which(d$score != 0)) ), 2) * 100
  p = ggplot(d, aes(x = germplasm, y = environment))  
  p = p + geom_raster(aes(fill = score)) + ggtitle(paste("GxE combinaisons (",  nb_NA, "% of 0)", sep = ""))
  out.experimental.design = list("plot" = p, "data.presence.abscence.matrix" = m)
  message("The experimental design plot is done.")
}


# 2. Get MCMC data frame ----------
MCMC = out.model$MCMC
MCMC = rbind.data.frame(MCMC[[1]], MCMC[[2]])
if(attributes(out.model)$PPBstats.object == "model1") { attributes(MCMC)$model = "model1" }
if(attributes(out.model)$PPBstats.object == "model2") { attributes(MCMC)$model = "model2" }

# 3. convergence ----------
out.convergence = NULL
if(analysis == "all" | analysis == "convergence") {
  message("The Gelman-Rubin test is running for each parameter ...")
  test = gelman.diag(out.model$MCMC, multivariate = FALSE)$psrf[,1]
  conv_ok = names(which(test < 1.05))
  conv_not_ok = names(which(test > 1.05))
      
  if( length(conv_not_ok) > 0 ) {
    message("The two MCMC of the following parameters do not converge thanks to the Gelman-Rubin test: ", paste(conv_not_ok, collapse = ", ") ,". Therefore, they are not present in MCMC output.")
    mcmc = MCMC[,is.element(colnames(MCMC), conv_not_ok)]
    
    out.convergence = NULL
    for (para in conv_not_ok) {
    
    D = cbind.data.frame(Iteration = rep(c(1:(nrow(MCMC)/2)), 2), 
                         Chain = factor(rep(c(1,2), each = (nrow(MCMC)/2))), 
                         Parameter = para, 
                         value = as.vector(MCMC[,para])
                         )
        
    traceplot = ggplot(D, aes(x = Iteration, y = value, color = Chain)) + geom_line() + ggtitle(para) # cf ggmcmc:ggs_traceplot
    density = ggplot(D, aes(x = value, fill = Chain, color = Chain)) + geom_density() + ggtitle(para) # cf ggmcmc:ggs_density
    plot = list(list("traceplot" = traceplot, "density" = density))
    names(plot) = para
    out.convergence = c(out.convergence, plot)
    }
  } else { message("The two MCMC for each parameter converge thanks to the Gelman-Rubin test."); out.convergence = NULL }
}


# 4. posteriors ----------

get.caterpillar.plot = function(x){ # cf ggmcmc:ggs_caterpillar
  p = ggplot(x, aes(x = q3, y = reorder(parameter, q3))) 
  p = p + geom_point(size = 3) # median 
  p = p + geom_segment(aes(x = q2, xend = q4, yend = reorder(parameter, q3)), size = 1.5) # 25%-75%
  p = p + geom_segment(aes(x = q1, xend = q5, yend = reorder(parameter, q3)), size = 0.5) # 2.5%-25% and 75%-97.5%
  p = p + ylab("parameter") + xlab("value") + ggtitle(x[1, "environment"])
  return(p)
}

add_split_col = function(x, each){ rep(c(1:nrow(x)), each = each)[1:nrow(x)] } 

out.posteriors = NULL
if(analysis == "all" | analysis == "posteriors") {

  s = summary(out.model$MCMC)
  sq_MCMC = as.data.frame(s$quantiles)
  sq_MCMC$parameter = as.factor(rownames(sq_MCMC))
  colnames(sq_MCMC) = c("q1", "q2", "q3", "q4", "q5", "parameter")
  
  # 4.1. model 1 ----------
  
  if(attributes(out.model)$PPBstats.object == "model1") {
    # 4.1.1. Update MCMC and get data frame with environments where some parameters did not converge ----------
    if(analysis == "all" | analysis == "convergence") {
      if( length(conv_not_ok) > 0 ) {
        
        mu_not_ok = conv_not_ok[grep("mu\\[", conv_not_ok)]
        if( length(mu_not_ok) > 0 ) {
          env_not_ok_mu = unique(sub("\\]", "", sub("mu\\[", "", sapply(mu_not_ok, function(x){unlist(strsplit(as.character(x), ","))[2]}))))
        } else { env_not_ok_mu = NULL }

        beta_not_ok = conv_not_ok[grep("beta\\[", conv_not_ok)]
        if( length(beta_not_ok) > 0 ) {
          env_not_ok_beta = unique(sub("\\]", "", sub("beta\\[", "", sapply(beta_not_ok, function(x){unlist(strsplit(as.character(x), ","))[1]}))))
        } else { env_not_ok_beta = NULL }

        sigma_not_ok = conv_not_ok[grep("sigma\\[", conv_not_ok)]
        if( length(beta_not_ok) > 0 ) {
          env_not_ok_sigma = unique(sub("\\]", "", sub("sigma\\[", "", sigma_not_ok)))
        } else { env_not_ok_sigma = NULL }
        
        env_not_ok = unique(c(env_not_ok_mu, env_not_ok_beta, env_not_ok_sigma))
        if( length(env_not_ok) > 0 ) {
        model1.data_env_whose_param_did_not_converge = droplevels(filter(out.model$data.model1, environment %in% env_not_ok))
        attributes(model1.data_env_whose_param_did_not_converge)$PPBstats.object = "model1.data_env_whose_param_did_not_converge"
        
        # Update MCMC, delete all environments where at least one parameter do not converge
        message("MCMC are updated, the following environment were deleted :", paste(env_not_ok, collapse = ", "))
        message("model1.data_env_whose_param_did_not_converge contains the raw data for these environments.")
        m1 = unlist(sapply(paste("sigma\\[", env_not_ok, sep = ""), function(x){grep(x, colnames(MCMC))} ))
        m2 = unlist(sapply(paste("beta\\[", env_not_ok, sep = ""), function(x){grep(x, colnames(MCMC))} ))
        m3 = grep("mu\\[", colnames(MCMC))
        m3 = colnames(MCMC)[m3][unlist(sapply(paste(",", env_not_ok, "]", sep = ""), function(x){grep(x, colnames(MCMC)[m3])} ))]
        m3 = c(1:ncol(MCMC))[is.element(colnames(MCMC), m3)]
        
        mcmc_to_delete = c(m1, m2, m3)
        MCMC = MCMC[,-mcmc_to_delete] 
        if(attributes(out.model)$PPBstats.object == "model1") { attributes(MCMC)$model = "model1" }
      }
    }
    }
    
    # 4.1.2. Format MCMC for further use ----------
    sq_MCMC$entry_mu = sub("mu\\[", "", sapply(sq_MCMC$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
    
    env_beta = sub("\\]", "", sub("beta\\[", "", sapply(sq_MCMC$parameter[grep("beta\\[", sq_MCMC$parameter)], function(x){unlist(strsplit(as.character(x), ","))[1]})))
    env_mu = sub("\\]", "", sub("mu\\[", "", sapply(sq_MCMC$parameter[grep("mu\\[", sq_MCMC$parameter)], function(x){unlist(strsplit(as.character(x), ","))[2]})))
    env_nu = env_rho = NA
    env_sigma = sub("\\]", "", sub("sigma\\[", "", sq_MCMC$parameter[grep("sigma\\[", sq_MCMC$parameter)]))
    sq_MCMC$environment =  c(env_beta, env_mu, env_nu, env_rho, env_sigma)
    
    sq_MCMC$location = sapply(sq_MCMC$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
    sq_MCMC$year = sapply(sq_MCMC$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
    
    # 4.1.3. sigma_j distribution ----------   
    out_sigma_distribution = NULL
    if( length(grep("nu", rownames(sq_MCMC))) > 0 & length(grep("rho", rownames(sq_MCMC))) > 0 & length(grep("sigma", rownames(sq_MCMC))) > 0 ) {
      nu = sq_MCMC["nu", "q3"]
      rho = sq_MCMC["rho", "q3"]
      d_sigma_distribution = cbind.data.frame(sigma_distribution = sqrt(1/rgamma(10000, nu, rho)))
      
      p = ggplot()
      p = p + geom_density(data = d_sigma_distribution, aes(x = sigma_distribution) )
      
      sigma = sq_MCMC[grep("sigma", sq_MCMC$parameter), "q3"]; names(sigma) = sq_MCMC$parameter[grep("sigma", sq_MCMC$parameter)]
      sigma = sort(sigma)
      d_sigma = cbind.data.frame(sigma = as.factor(names(sigma)), value = sigma)
      
      p.tmp = p + geom_vline(data = d_sigma, aes(xintercept = value, color = sigma))
      out = list(p.tmp)
      
      SEQ = unique(c(seq(0, nrow(d_sigma), 5), nrow(d_sigma)))
      for(s in 1:(length(SEQ) - 1)) {
        d_sigma_tmp = d_sigma[c((SEQ[s]+1):SEQ[s+1]),]
        p.tmp = p + geom_vline(data = d_sigma_tmp, aes(xintercept = value, color = sigma), show_guide = TRUE)
        out = c(out, list(p.tmp))
      }
      out_sigma_distribution = out
      message("The values of sigma in the inverse Gamme distribution are done.")
    }
    
    # 4.1.4. mu_ij, beta_jk and sigma_j caterpillar plot distribution ----------
    out_para_posteriors = NULL
        
    if ( length(grep("mu", rownames(sq_MCMC))) > 0  ) {
      sq_MCMC_mu = droplevels(sq_MCMC[grep("mu", rownames(sq_MCMC)),])
      xmin = min(sq_MCMC_mu$q1); xmax = max(sq_MCMC_mu$q5)
      
      sq_MCMC_mu_env = plyr:::splitter_d(sq_MCMC_mu, .(environment))
      out = lapply(sq_MCMC_mu_env, function(x){ get.caterpillar.plot(x) }) # + xlim(xmin, xmax)
      out = list("mu_posteriors" = out)
      out_para_posteriors = c(out_para_posteriors, out)
      message("The mu_ij posterior distributions are done.")
    }
    
    if ( length(grep("beta", rownames(sq_MCMC))) > 0  ) {
      sq_MCMC_beta = droplevels(sq_MCMC[grep("beta", rownames(sq_MCMC)),])   
      xmin = min(sq_MCMC_beta$q1); xmax = max(sq_MCMC_beta$q5)
      
      sq_MCMC_beta_env = plyr:::splitter_d(sq_MCMC_beta, .(environment))
      out = lapply(sq_MCMC_beta_env, function(x){ get.caterpillar.plot(x) }) # + xlim(xmin, xmax)
      out = list("beta_posteriors" = out)
      out_para_posteriors = c(out_para_posteriors, out)
      message("The beta_jk posterior distributions are done.")      
    }
    
    if ( length(grep("sigma", rownames(sq_MCMC))) > 0  ) {
      sq_MCMC_sigma = droplevels(sq_MCMC[grep("sigma", rownames(sq_MCMC)),])    
      xmin = min(sq_MCMC_sigma$q1); xmax = max(sq_MCMC_sigma$q5)
      
      sq_MCMC_sigma$split = add_split_col(sq_MCMC_sigma, nb_parameters_per_plot)
      sq_MCMC_sigma_split = plyr:::splitter_d(sq_MCMC_sigma, .(split))

      out = lapply(sq_MCMC_sigma_split, function(x){ get.caterpillar.plot(x) + ggtitle("") } ) # + xlim(xmin, xmax) 
      out = list("sigma_posteriors" = out)
      out_para_posteriors = c(out_para_posteriors, out)
      message("The sigma_j posterior distributions are done.")
    }
    
    # 4.1.5. standardized epsilon_ijk distribution ----------
    out_stand_res = NULL
    
    if ( !is.null(out.model$epsilon)  ) {      
      epsilon_ijk = out.model$epsilon
      
      sigma_j = sq_MCMC[grep("sigma", sq_MCMC$parameter), "q3"]
      names(sigma_j) = sq_MCMC$parameter[grep("sigma", sq_MCMC$parameter)]
      
      env = sub("\\]", "", sapply(names(epsilon_ijk), function(x) { sub("epsilon\\[", "", sapply(x, function(x){unlist(strsplit(as.character(x), ","))[2]})) }))
      sigma_j = sigma_j[paste("sigma[", env, "]", sep="")]
      
      d_std_res = cbind.data.frame(x = c(1:length(sigma_j)), std_res = epsilon_ijk / sigma_j)
      out_stand_res = ggplot(d_std_res, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
      message("The standardised residuals distributions are done.")
    }

    out.posteriors = list("sigma_distribution" = out_sigma_distribution, "parameter_posteriors" = out_para_posteriors, "standardized_residuals" = out_stand_res)
  }
  
  # 4.2. model 2 ----------
  if(attributes(out.model)$PPBstats.object == "model2") {
    
    # 4.2.1. Update MCMC and model2.presence.abscence.matrix ----------
    if(analysis == "all" | analysis == "convergence") {
      if( length(conv_not_ok) > 0 ) {
  
        MCMC = MCMC[,!is.element(colnames(MCMC), conv_not_ok)] 
        if(attributes(out.model)$PPBstats.object == "model2") { attributes(MCMC)$model = "model2" }
        
            alpha_not_ok = conv_not_ok[grep("alpha\\[", conv_not_ok)]
            if( length(alpha_not_ok) > 0 ) {
              germ_not_ok_alpha = sub("\\]", "", sub("alpha\\[", "", alpha_not_ok))
            } else { germ_not_ok_alpha = NULL }
        
            beta_not_ok = conv_not_ok[grep("beta\\[", conv_not_ok)]
            if( length(beta_not_ok) > 0 ) {
              germ_not_ok_beta = sub("\\]", "", sub("beta\\[", "", beta_not_ok))
            } else { germ_not_ok_beta = NULL }
            
            theta_not_ok = conv_not_ok[grep("theta\\[", conv_not_ok)]
            if( length(theta_not_ok) > 0 ) {
              env_not_ok = sub("\\]", "", sub("theta\\[", "", theta_not_ok))
            } else { env_not_ok = NULL }

            germ_not_ok = unique(c(germ_not_ok_alpha, germ_not_ok_beta))
            
            mat = out.model$model2.presence.abscence.matrix
            if( !is.null(germ_not_ok) ) { mat = mat[!is.element(rownames(mat), germ_not_ok),] }
            if( !is.null(env_not_ok) ) { mat = mat[,!is.element(colnames(mat), env_not_ok)] }
            model2.presence.abscence.matrix = mat
      }
    }
    
    # 4.2.2. alpha_i, beta_i, theta_j caterpillar plot distribution ----------
    out_para_posteriors = NULL
    
    if ( length(grep("alpha\\[", rownames(sq_MCMC))) > 0  ) {      
      sq_MCMC_alpha = droplevels(sq_MCMC[grep("alpha\\[", rownames(sq_MCMC)),]) 
      xmin = min(sq_MCMC_alpha$q1); xmax = max(sq_MCMC_alpha$q5)
      
      sq_MCMC_alpha$split = add_split_col(sq_MCMC_alpha, each = nb_parameters_per_plot)
      sq_MCMC_alpha_split = plyr:::splitter_d(sq_MCMC_alpha, .(split))      
            
      out = lapply(sq_MCMC_alpha_split, function(x){ get.caterpillar.plot(x) } ) # + xlim(xmin, xmax) 
      out = list("alpha_posteriors" = out)
      out_para_posteriors = c(out_para_posteriors, out)
      message("The alpha_i posterior distributions are done.")
    }
    
    if ( length(grep("beta\\[", rownames(sq_MCMC))) > 0  ) {
      sq_MCMC_beta = droplevels(sq_MCMC[grep("beta\\[", rownames(sq_MCMC)),])    
      xmin = min(sq_MCMC_beta$q1); xmax = max(sq_MCMC_beta$q5)
      
      sq_MCMC_beta$split = add_split_col(sq_MCMC_beta, each = nb_parameters_per_plot)
      sq_MCMC_beta_split = plyr:::splitter_d(sq_MCMC_beta, .(split))      
      
      out = lapply(sq_MCMC_beta_split, function(x){ get.caterpillar.plot(x) } ) # + xlim(xmin, xmax) 
      out = list("beta_posteriors" = out)
      out_para_posteriors = c(out_para_posteriors, out)
      message("The beta_i posterior distributions are done.")      
    }
    
    if ( length(grep("theta\\[", rownames(sq_MCMC))) > 0  ) {
      sq_MCMC_theta = droplevels(sq_MCMC[grep("theta\\[", rownames(sq_MCMC)),])    
      xmin = min(sq_MCMC_theta$q1); xmax = max(sq_MCMC_theta$q5)
      
      sq_MCMC_theta$split = add_split_col(sq_MCMC_theta, each = nb_parameters_per_plot)
      sq_MCMC_theta_split = plyr:::splitter_d(sq_MCMC_theta, .(split))      
      
      out = lapply(sq_MCMC_theta_split, function(x){ get.caterpillar.plot(x) } ) # + xlim(xmin, xmax)
      out = list("theta_posteriors" = out)
      out_para_posteriors = c(out_para_posteriors, out)
      message("The theta_j posterior distributions are done.")
    }
    
    # 4.2.3. standardized epsilon_ijk distribution ----------
    out_stand_res = NULL
    
    if ( !is.null(out.model$epsilon)  ) {      
      
      epsilon_ijk = out.model$epsilon     
      sigma_epsilon = sq_MCMC[grep("sigma_epsilon", sq_MCMC$parameter), "q3"]
      std_res = epsilon_ijk / sigma_epsilon
      
      d_std_res = cbind.data.frame(x = c(1:length(epsilon_ijk)), std_res)
      out_stand_res = ggplot(d_std_res, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
      message("The standardised residuals distributions are done.")
    }
    
    out.posteriors = list("parameter_posteriors" = out_para_posteriors, "standardized_residuals" = out_stand_res)    
    }

} 


# 5. Return outptus ----------
out = list("data.experimental_design" = out.experimental.design,
           "model2.presence.abscence.matrix" = model2.presence.abscence.matrix,
           "convergence" = out.convergence, 
           "posteriors" = out.posteriors, 
           "MCMC" = MCMC,
           "model1.data_env_whose_param_did_not_converge" = model1.data_env_whose_param_did_not_converge)
return(out)
}

