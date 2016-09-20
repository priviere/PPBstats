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


ecovalence = function(matrix, variable){
  #matrix : matrice ou data contenant les termes d'interaction
  
  matrix = matrix^2
  
  melted_matrix = melt(data = matrix)
  matrix_inter_plot = ggplot(data = melted_matrix, aes(x = Var2, y = Var1, fill = value)) + geom_tile()
  matrix_inter_plot = matrix_inter_plot + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) 
  matrix_inter_plot = matrix_inter_plot + theme_minimal() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), panel.grid=element_blank(), axis.ticks = element_blank())
  matrix_inter_plot = matrix_inter_plot + labs(title = paste("Representation de la matrice des termes d'interaction du modele pour la variable :",variable,",\nla valeur entre parenthese correspond a l'ecovalence de Wricke du facteur"))
  matrix_inter_plot = matrix_inter_plot + theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  return(matrix_inter_plot)
}



gpieplot = function(anov, var){
  #anov : anova du modèle complet, var : nom de la variable étudiée
  total_Sum_Sq = sum(anov$"Sum Sq")
  Sum_sq = anov$"Sum Sq"
  pvalue = anov$"Pr(>F)"
  percentage_Sum_sq = Sum_sq/total_Sum_Sq*100
  factor = rownames(anov)
  
  DFtemp = cbind.data.frame(factor, pvalue, Sum_sq, percentage_Sum_sq)

  pie = ggplot(data = DFtemp, aes(x = "", y = percentage_Sum_sq, fill = factor)) 
  pie = pie + theme_minimal() + theme(axis.title.x = element_blank(), 
                                      axis.title.y = element_blank(), 
                                      panel.border = element_blank(), 
                                      panel.grid=element_blank(), 
                                      axis.ticks = element_blank(), 
                                      axis.text.x =element_blank(), 
                                      plot.title = element_text(lineheight=.8, face="bold"))
  pie = pie + ggtitle(paste("Distribution de la variance totale pour \n",var))
  pie = pie + geom_bar(width = 1, stat = "identity")
  #pie = pie + geom_text(data=DFtemp, aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = paste("  ",round(valuep*100), "%")))
  pie = pie + coord_polar("y", start = 0)
  return(pie)
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


dographPCA=function(pca){
  
  # déjà un graph PCA dans PPBstats::get.ggplot, voir à le compléter avec ce code ?
  
  #pca : ACP produite par le logiciel R, var : nom du facteur étudié (germplasme ou environnement)
  
  gACPplot=function(X,Y,labs){
    # X : coordonée sur la première CP, Y : coordonnée sur la seconde CP, labs : vecteur contenant les noms des variables 
    
    pdata=data.frame(X,Y,labs)
    
    p=ggplot(pdata, aes(X,Y, label=labs,hjust=0.5, vjust=-1)) 
    p=p + geom_line(aes(x=0),size=1,color = "#666666")
    p=p + geom_line(aes(y=0),size=1,color = "#666666")
    p=p + geom_point(color="green",size=5)
    p=p + geom_text(color="darkgreen")
    
    return(p)
  }
  
  gvACPplot=function(X,Y,labs){
    # X : coordonée sur la première CP, Y : coordonnée sur la seconde CP, labs : vecteur contenant les noms des variables 
    
    pdata=data.frame(X,Y,labs)  
    limite=0.75 #on affiche en bleu les variables bien représentées par les CP. limite fixe le pourcentage minimal de représentation de la variable.
    compteur=0
    
    #on créer ci-dessous deux nouvelles colonnes dans le data qui contiendront des 1-2 pour l'une et des 0-3 pour l'autre
    #les valeurs sont choisies suivant la réprésentation de la variable par les CP en question
    #si elle est bien représentée elle sera affichée en bleu, sinon en rouge
    if(sqrt((pdata[1,1]*pdata[1,1])+(pdata[1,2]*pdata[1,2]))>limite){
      color=c(1)
      color1=c(2)
      compteur=compteur+1
    }
    else{
      color=c(0)
      color1=c(3)
    }
    for(i in 2:nrow(pdata)){
      if(sqrt((pdata[i,1]*pdata[i,1])+(pdata[i,2]*pdata[i,2]))>limite){
        color=c(color,1)
        color1=c(color1,2)
        compteur=compteur+1
      }
      else{
        color=c(color,0)
        color1=c(color1,3)
      }
    }
    pdata$color=color
    pdata$color1=color1
    pdata$color=as.factor(pdata$color)
    pdata$color1=as.factor(pdata$color1)
    
    p=ggplot()
    angle=seq(-pi, pi, length = 50) 
    df=data.frame(x = (sin(angle)*4), y = (cos(angle)*4)) 
    p=p + geom_path(aes(x, y), data = df, colour="#666666",size=1) 
    p=p + geom_segment(aes(x = -4, y = 0, xend = 4, yend = 0), color = "#666666", size=1)
    p=p + geom_segment(aes(x = 0, y = -4, xend = 0, yend = 4), color = "#666666", size=1)
    p=p + geom_segment(data=pdata, aes(x = 0, y = 0, xend = X*3.95, yend = Y*3.95, color=color), size=1) + scale_color_manual(values=c("2"="#000099","3"="#990000","1"="#66CCFF","0"="#FF3366"))
    p=p + theme(legend.position="none")
    p=p + geom_text(data=pdata, aes(x=(X*4),y=(Y*4),label=labs,color=color1), size=6) 
    
    return(p)
  }
  
  
  
  vec_Var=pca$eig$"percentage of variance"
  CP=c(1)
  for(i in 2:length(vec_Var)){CP=c(CP,i)}
  
  screedata=data.frame(vec_Var,CP)
  gscree_plot=ggplot(data=screedata,aes(x=CP,y=vec_Var))
  gscree_plot=gscree_plot + geom_point(size=4)
  gscree_plot=gscree_plot + geom_line(size=1,linetype = "dotted")
  gscree_plot=gscree_plot + geom_text(label=paste("CP",CP,":",round(vec_Var),"%"), vjust=-1, hjust=-0.25)
  gscree_plot=gscree_plot + labs(title = "Pourcentage de la variance expliquee par chaque composante principale, ACP")
  gscree_plot=gscree_plot + theme(plot.title = element_text(lineheight=.8, face="bold"))
  gscree_plot=gscree_plot + ylab("Variance expliquee (en % de la Variance totale)")
  
  CP1=pca$ind$coord[,1]
  CP2=pca$ind$coord[,2]
  CP3=pca$ind$coord[,3]
  CP4=pca$ind$coord[,4]
  labs_ind=rownames(pca$ind$coord)
  
  ACPplot1=gACPplot(CP1,CP2,labs_ind) + xlab("CP1") + ylab("CP2")
  ACPplot2=gACPplot(CP1,CP3,labs_ind) + xlab("CP1") + ylab("CP3")
  ACPplot3=gACPplot(CP1,CP4,labs_ind) + xlab("CP1") + ylab("CP4")
  ACPplot4=gACPplot(CP2,CP3,labs_ind) + xlab("CP2") + ylab("CP3")
  ACPplot5=gACPplot(CP2,CP4,labs_ind) + xlab("CP2") + ylab("CP4")
  ACPplot6=gACPplot(CP3,CP4,labs_ind) + xlab("CP3") + ylab("CP4")
  
  vCP1=pca$var$coord[,1]
  vCP2=pca$var$coord[,2]
  vCP3=pca$var$coord[,3]
  vCP4=pca$var$coord[,4]
  labs_var=rownames(pca$var$coord)
  
  vACPplot1=gvACPplot(vCP1,vCP2,labs_var) + xlab("CP1") + ylab("CP2")
  vACPplot2=gvACPplot(vCP1,vCP3,labs_var) + xlab("CP1") + ylab("CP3")
  vACPplot3=gvACPplot(vCP1,vCP4,labs_var) + xlab("CP1") + ylab("CP4")
  vACPplot4=gvACPplot(vCP2,vCP3,labs_var) + xlab("CP2") + ylab("CP3")
  vACPplot5=gvACPplot(vCP2,vCP4,labs_var) + xlab("CP2") + ylab("CP4")
  vACPplot6=gvACPplot(vCP3,vCP4,labs_var) + xlab("CP3") + ylab("CP4")
  
  out=list(gscree_plot,ACPplot1,ACPplot2,ACPplot3,ACPplot4,ACPplot5,ACPplot6,vACPplot1,vACPplot2,vACPplot3,vACPplot4,vACPplot5,vACPplot6)
  names(out)=c("screeplot","ind1_2","ind1_3","ind1_4","ind2_3","ind2_4","ind3_4","var1_2","var1_3","var1_4","var2_3","var2_4","var3_4")
  
  return(out)
}

build_interaction_matrix = function(model, data){
  
  # In agricolae::AMMI, the interaction matrc is done base on the additive model, i.e. in the interaction matrix there are inteaction terms + residuals. Here, there is only interaction terms.
  #MEDIAS <- data.frame(ENV = x, GEN = y, Y = z)
  #modelo1 <- lm(Y ~ ENV + GEN, data = MEDIAS)
  #residual <- modelo1$residuals
  #MEDIAS <- data.frame(MEDIAS, RESIDUAL = residual)
  #mlabel <- names(MEDIAS)
  #names(MEDIAS) <- c(mlabel[1:2], name.y, mlabel[4])
  #OUTRES <- MEDIAS[order(MEDIAS[, 1], MEDIAS[, 2]), ]
  #OUTRES2 <- by(OUTRES[, 4], OUTRES[, c(2, 1)], function(x) sum(x,na.rm = TRUE))
  
  c = model$coefficients
  
  # germplasm ----------
  coef_germ = c[grep("germplasm", names(c))]
  todel = grep(":", names(coef_germ))
  if(length(todel) > 0) { coef_germ = coef_germ[-todel] }
  coef_germ = c(coef_germ, - sum(coef_germ))
  names(coef_germ) = model$xlevels$germplasm
  
  # variance intra germplasm
  var_intra = tapply(model$residuals, model$model$germplasm, var, na.rm = TRUE)
  
  # location ----------
  coef_env = c[grep("location", names(c))]
  todel = grep(":", names(coef_env))
  if(length(todel) > 0) { coef_env = coef_env[-todel] }
  coef_env = c(coef_env, - sum(coef_env))
  names(coef_env) = model$xlevels$location
  
  # germplasm x location ----------
  coef_gxe = c[grep("germplasm", names(c))]
  coef_gxe = coef_gxe[grep(":", names(coef_gxe))]
  coef_gxe[which(is.na(coef_gxe))] = 0 # If NA, it means that coef is not defined because of singularities, then it means no interaction, so 0 based on the construction of the model.
  
  Mgxe = as.data.frame(matrix(NA, ncol = length(model$xlevels$location), nrow = length(model$xlevels$germplasm)))
  colnames(Mgxe) = paste("env", c(1:length(model$xlevels$location)), sep="")
  rownames(Mgxe) = paste("germ", c(1:length(model$xlevels$germplasm)), sep="")
  
  compteur = 1
  for(i in 1:(length(model$xlevels$location)-1)){
    for(j in 1:(length(model$xlevels$germplasm)-1)){
      Mgxe[j,i] = coef_gxe[compteur]
      compteur = compteur + 1
    }
  }
  
  rownames(Mgxe) = model$xlevels$germplasm
  colnames(Mgxe) = model$xlevels$location
  
  # Get element of the interaction matrix that must not be estimated because the data do no exist
  w = as.data.frame(with(data, table(year, location, germplasm)))
  
  a = w$Freq == 0
  b = as.character(w$germplasm) == as.character(model$xlevels$germplasm[length(model$xlevels$germplasm)])
  c = as.character(w$location) == as.character(model$xlevels$location[length(model$xlevels$location)])
  w = w[which(a & (b|c)),]
  
  if(nrow(w)>0){
    for(i in 1:nrow(w)){ Mgxe[as.character(w[i,"germplasm"]), as.character(w[i, "location"])] = 0 }
    
    # Knowing this, correct the col or row to set the constrains sum = 0
    a = Mgxe[as.character(w[i,"germplasm"]), 1:(ncol(Mgxe) - 1)]
    x = 0 - sum(a)
    Mgxe[as.character(w[i,"germplasm"]), 1:(ncol(Mgxe)-1)] = a + x/3
  }
  # quid si 0 aussi dans SH JEV et AML? ... A tester ! ----------
  
  for(i in 1:nrow(Mgxe)){
    if(is.na(Mgxe[i, ncol(Mgxe)])) { Mgxe[i, ncol(Mgxe)] = 0 - sum(Mgxe[i, 1:(ncol(Mgxe)-1)])}
  }
  
  for(j in 1:ncol(Mgxe)){
    if(is.na(Mgxe[nrow(Mgxe),j])) { Mgxe[nrow(Mgxe), j] = 0 - sum(Mgxe[1:(nrow(Mgxe)-1), j])}
  }
  
  # outputs ----------
  out = list(coef_env, coef_germ, var_intra, Mgxe)
  names(out)=c("vec_E", "vec_G", "vec_var_G_intra", "data_interaction")
  
  return(out)
}







