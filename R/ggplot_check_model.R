ggplot_check_model = function(data_check_model){
  
  get.caterpillar.plot = function(x){ # cf ggmcmc:ggs_caterpillar
    p = ggplot(x, aes(x = q3, y = reorder(parameter, q3))) 
    p = p + geom_point(size = 3) # median 
    p = p + geom_segment(aes(x = q2, xend = q4, yend = reorder(parameter, q3)), size = 1.5) # 25%-75%
    p = p + geom_segment(aes(x = q1, xend = q5, yend = reorder(parameter, q3)), size = 0.5) # 2.5%-25% and 75%-97.5%
    p = p + ylab("parameter") + xlab("value") + ggtitle(x[1, "environment"])
    return(p)
  }
  
  # model 1, sigma_j ----------
  data_ggplot_model_1_sigma_j = list(d_sigma_distribution = d_sigma_distribution, d_sigma = d_sigma)  
  p = ggplot()
  p = p + geom_density(data = d_sigma_distribution, aes(x = sigma_distribution) )
  p.tmp = p + geom_vline(data = d_sigma, aes(xintercept = value, color = sigma))
  out = list(p.tmp)
  
  SEQ = unique(c(seq(0, nrow(d_sigma), 5), nrow(d_sigma)))
  for(s in 1:(length(SEQ) - 1)) {
    d_sigma_tmp = d_sigma[c((SEQ[s]+1):SEQ[s+1]),]
    p.tmp = p + geom_vline(data = d_sigma_tmp, aes(xintercept = value, color = sigma), show.legend = TRUE)
    out = c(out, list(p.tmp))
  }
  out_sigma_distribution = out
  message("Distribution of sigma_j in the inverse Gamme distribution are done.")
  
  # model 1, mu_ij caterpillar plot
  out = lapply(data_ggplot_model_1_mu_ij, function(x){ get.caterpillar.plot(x) }) # + xlim(xmin, xmax)
  out = list("mu_posteriors" = out)
  out_para_posteriors = c(out_para_posteriors, out)
  message("The mu_ij posterior distributions are done.")
  
  # model 1, beta_jk caterpillar plot ----------
  out = lapply(data_ggplot_model_1_beta_jk, function(x){ get.caterpillar.plot(x) }) # + xlim(xmin, xmax)
  out = list("beta_posteriors" = out)
  out_para_posteriors = c(out_para_posteriors, out)
  message("The beta_jk posterior distributions are done.")      
  
  
  # model 1, sigma_j caterpillar plot ----------
  out = lapply(data_ggplot_model_1_sigma_j, function(x){ get.caterpillar.plot(x) + ggtitle("") } ) # + xlim(xmin, xmax) 
  out = list("sigma_posteriors" = out)
  out_para_posteriors = c(out_para_posteriors, out)
  message("The sigma_j posterior distributions are done.")
  
  
  # model 1, standardized epsilon_ijk distribution ----------
  out_stand_res = ggplot(d_std_res, aes(x = x, y = std_res)) + geom_point() + xlab("") + ylab("standardised residuals")
  message("The standardised residuals distributions are done.")
  
  
}