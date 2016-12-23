ggplot_LSDbarplot = function(model, variable, fac, p.adj){
  fac = "germplasm"
  LSD = LSD.test(model, fac, p.adj = p.adj)
  LSD$groups$trt = factor(LSD$groups$trt, levels = LSD$groups$trt)
  d_LSD = LSD$groups
  d_LSD$ymean = d_LSD$means / 2
  p = ggplot(data = d, aes(x = trt, y = means, label = M)) + geom_bar(stat = "identity")
  p = p + geom_text(aes(y = ymean), angle = 90, color = "white")
  p = p + theme(legend.position = "none", axis.text.x = element_text(angle = 90))
  p = p + xlab(fac) + ylab(variable)
  return(p)
}