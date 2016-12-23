gLSDplot = function(model, x, variable, adjust){
  #model : modèle complet, x : facteur étudié, y : variable étudiée, adjust : ajustement ("bonferroni" / "holm" / "none" / "hochberg" / "BH" / "BY")
  
  LSD = LSD.test(model, x, p.adj = adjust) #pareil pour p.adj : "bonferroni" / "holm" / "none" / "hochberg" / "BH" / "BY"
  LSD$groups$trt = factor(LSD$groups$trt, levels = LSD$groups$trt)
  
  if (x == "germplasm"){
    d = LSD$groups
    d$germplasm = d$trt
    d$ymean = d$means / 2
    p = ggplot(data = d, aes(x = germplasm, y = means, label = M)) + geom_bar(stat = "identity")
    p = p + geom_text(aes(y = ymean), angle = 90, color = "white")
  }
  else if (x == "location"){
    d = LSD$groups
    d$location = d$trt
    d$ymean = d$means / 2
    p = ggplot(data = d, aes(x = location, y = means, label = M)) + geom_bar(stat = "identity")
    p = p + geom_text(aes(y = ymean), angle = 90, color = "white")
  }
  
  p = p + theme(legend.position = "none", axis.text.x = element_text(size = 15, angle = 90), plot.title = element_text(lineheight = .8, face = "bold")) + labs(title =variable)

  return(p)
}




gResidualplot = function(model, variable){
  #data : data contenant les données, x : facteur étudié, y : variable étudiée
  
  d = data.frame(x = model$model$germplasm, y = model$residuals)
  p = ggplot(d, aes(x = x, y = y)) 
  p = p + geom_boxplot(shape = 20, aes(color = x))
  p = p + labs(title = paste("Residus bruts du modele en fonction des differents germplasmes pour la variable :", variable))
  p = p + theme(legend.position = "none", axis.text.x = element_text(size = 15, angle = 90), plot.title = element_text(lineheight = .8, face = "bold"))
  
  return(p)
}

