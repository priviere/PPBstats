GxE_build_interaction_matrix = function(data, gxe_analysis){
  
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


