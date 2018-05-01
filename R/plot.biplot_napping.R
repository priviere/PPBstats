plot.biplot_napping = function(x){
  # see http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_mfa.html
  out = list(
    "partial_axes" = fviz_mfa_axes(x),
    "ind" = fviz_mfa_ind(x),
    "var" = fviz_mfa_var(x)
  )
  return(out)
}