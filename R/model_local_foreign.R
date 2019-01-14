model_local_foreign = function(){
  
  cat("modèle migrant vs resident \n")
  #      mod_MR <- lm(temp ~ environnement + germplasme  + block %in% environnement + annee + MR %in% environnement + MR %in% annee + annee:germplasme + MR %in% annee:environnement, data=dataf)
  
#  local foreign  model <- lm(variable ~ location + germplasm + location/block + location/MR, data = data)
  
  mod_MR <- lm(temp ~ environnement + germplasme  + annee + environnement/block  +  
                 environnement/MR + annee/MR + 
                 annee:germplasme + annee:environnement/MR, data=dataf)
  
  anov = Anova(mod_MR, type="III")
  print(shapiro.test(residuals(mod_MR)))
  qqnorm(residuals(mod_MR), main = paste(variable, " : qqplot Migrant-Resident", sep=""))
  qqline(residuals(mod_MR))
  Lsmeans = list("environnement" = lsmeans(mod_MR,"environnement"),
                 "germplasme" = lsmeans(mod_MR,"germplasme"),
                 "annee" = lsmeans(mod_MR,"annee"),
                 "block:environnement" = lsmeans(mod_MR, pairwise~block|environnement),
                 "MR:environnement" = lsmeans(mod_MR, pairwise~MR|environnement),
                 "germplasme:annee" = lsmeans(mod_MR, pairwise~annee|germplasme),
                 "MR:annee" = lsmeans(mod_MR, pairwise~MR|annee),
                 "environnement:annee" = lsmeans(mod_MR, pairwise~annee|environnement),
                 "MR:annee:environnement" = lsmeans(mod_MR, pairwise~MR|environnement:annee)
  )
  
  
  
  
}



