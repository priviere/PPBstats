plot.check_model_napping <- function(
  x
){
  out = fviz_eig(x) + ggtitle("")
  return(out)
}