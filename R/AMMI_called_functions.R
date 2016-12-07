gboxplot = function(data, x, variable){
  if (x == "germplasm"){
    p = ggplot(data = data, aes(x = germplasm, y = variable, color = germplasm))
    p = p + labs(title = paste("Boite a moustache de la variable :" , variable,", en fonction du germplasme"))
  }
  else if (x == "location"){
    p = ggplot(data = data, aes(x = location, y = variable, color = location))
    p = p + labs(title = paste("Boite a moustache de la variable :" , variable,", en fonction de l'environnement"))
  }
  
  p = p + theme(legend.position = "none", axis.text.x = element_text(size = 15, angle = 90), plot.title = element_text(lineheight = .8, face = "bold"))
  p = p + scale_y_continuous(name = variable)
  p = p + geom_boxplot()
  
  return(p)
}




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



gqqplot=function(y, distribution = qnorm, xlab, ylab){
  
  x = distribution(ppoints(y))
  d = data.frame(x = x, y = sort(y))
  p = ggplot(d, aes(x = x, y = y))
  p = p + geom_point()
  p = p + geom_line(aes(x = x, y = x))
  p = p + xlab(xlab) + ylab(ylab)
  
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



gscatterplot = function(x, y, xlab, ylab){
  
  d = data.frame(x=x,y=y)
  p = ggplot(d, aes(x=x,y=y)) + geom_point() + xlab(xlab) + ylab(ylab)
  
  return(p)
}


gverifResidualsnormality = function(model){
  # model : modèle complet
  
  r = residuals(model)
  datasim = data.frame(r)
  plotRes0 = ggplot(datasim, aes(x = r), binwidth = 2)
  plotRes0 = plotRes0 + labs(title = paste("Skewness:",signif(skewness(r),3),", indicator of asymmetry and deviation from a normal distribution. (normal=0)\nKurtosis:",signif(kurtosis(r),3),", indicator of flattening or \"peakedness\" of a distribution. (normal=3)"))
  
  #   Skewness: indicator used in distribution analysis as a sign of asymmetry and deviation from a normal distribution. 
  #   
  #   Interpretation: 
  #   Skewness > 0 - Right skewed distribution - most values are concentrated on left of the mean, with extreme values to the right.
  #   Skewness < 0 - Left skewed distribution - most values are concentrated on the right of the mean, with extreme values to the left.
  #   Skewness = 0 - mean = median, the distribution is symmetrical around the mean.
  #   
  #   
  #   Kurtosis - indicator used in distribution analysis as a sign of flattening or "peakedness" of a distribution. 
  #   
  #   Interpretation: 
  #   Kurtosis > 3 - Leptokurtic distribution, sharper than a normal distribution, with values concentrated around the mean and thicker tails. This means high probability for extreme values.
  #   Kurtosis < 3 - Platykurtic distribution, flatter than a normal distribution with a wider peak. The probability for extreme values is less than for a normal distribution, and the values are wider spread around the mean.
  #   Kurtosis = 3 - Mesokurtic distribution - normal distribution for example.
  
  plotRes0 = plotRes0 + geom_histogram(aes(y = ..density..), fill = '#000099', alpha = 0.5)
  plotRes0 = plotRes0 + geom_density(colour = 'red', size=1)
  plotRes0 = plotRes0 + ylab(expression(bold('Density'))) + xlab(expression(bold('Residuals')))
  plotRes0 = plotRes0 + geom_vline(xintercept=0, size=1.5) + geom_hline(yintercept=0, size=1.5) 
  
  yh = predict(model)
  
  plotRes1 = gscatterplot(yh,r,xlab = "Fitted values", ylab = "Residuals")
  plotRes1 = plotRes1 + geom_hline(yintercept = 0) + geom_smooth()
  
  s = sqrt(deviance(model)/df.residual(model))
  rs = r/s
  
  plotRes2 = gqqplot(rs, xlab = "Theoretical Quantiles", ylab = "Standardized residuals")
  
  sqrt.rs = sqrt(abs(rs))
  plotRes3 = gscatterplot(yh, sqrt.rs, xlab = "Fitted values", ylab = expression(sqrt("Standardized residuals")))
  plotRes3 = plotRes3 + geom_smooth()
  
  out = list(plotRes0, plotRes1, plotRes2, plotRes3)
  names(out) = c("plotRes0", "plotRes1", "plotRes2", "plotRes3")
  
  return(out)
}







