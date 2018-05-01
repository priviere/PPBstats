biplot_data = function(x){
  if( is(x, "check_model_napping") ) { 
    out = x 
    class(out) <- c("PPBstats", "biplot_napping", "MFA")
  }
  return(out)
}
