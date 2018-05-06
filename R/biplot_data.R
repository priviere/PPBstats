biplot_data = function(x){
  if( is(x, "check_model_napping") ) { 
    out = x 
    class(out) <- c("PPBstats", "biplot_napping", "MFA")
  }
  if( is(x, "check_model_hedonic") ) { 
    out = x$hedonic$CA 
    class(out) <- c("PPBstats", "biplot_hedonic", "CA")
  }
  return(out)
}
