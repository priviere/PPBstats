plot.biplot_hedonic = function(x){
  # see http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_ca.html
  
  out = list("ca_biplot" = fviz_ca_biplot(x))
  return(out)
}