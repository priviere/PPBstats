ggplot_check_model = function(data_check_model){
  
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
  
  
}