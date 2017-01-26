mean_comparisons_GxE = function(
  out_check_model, 
  alpha = 0.05,
  p.adj = "none"
  ){
  # 1. Error message
  if( attributes(out_check_model)$PPBstats.object != "check_model_GxE" ) { stop("data must come from check_model and GxE") }
  
  model = out_check_model$GxE$ANOVA$model
  variable = out_check_model$GxE$variable
  
  data_ggplot_LSDbarplot = function(model, fac, p.adj){
    lsd = LSD.test(model, fac, alpha = alpha, p.adj = p.adj)
    
    parameter = factor(lsd$groups$trt, levels = lsd$groups$trt)
    means = lsd$groups$means
    groups = lsd$groups$M
    alpha = rep(alpha, length(parameter))
    alpha.correction = rep(p.adj, length(parameter))
    
    out_LSD = data.frame(parameter, means, groups, alpha, alpha.correction)
    if( nrow(out_LSD) == 0 ) { out_LSD = NULL }
    return(out_LSD)
  }
  
  # 2. Germplasm
  data_ggplot_LSDbarplot_germplasm = data_ggplot_LSDbarplot(model, fac = "germplasm", p.adj)
  
  # 3. Location
  data_ggplot_LSDbarplot_location = data_ggplot_LSDbarplot(model, fac = "location", p.adj)
  
  # 4. Year
  data_ggplot_LSDbarplot_year = data_ggplot_LSDbarplot(model, fac = "year", p.adj)
  
  # 5. return results
  out = list(
    "variable" = variable,
    "data_ggplot_LSDbarplot_germplasm" = data_ggplot_LSDbarplot_germplasm,
    "data_ggplot_LSDbarplot_location" = data_ggplot_LSDbarplot_location,
    "data_ggplot_LSDbarplot_year" = data_ggplot_LSDbarplot_year
  )
  
  attributes(out)$PPBstats.object = "mean_comparisons_GxE"
  
 return(out)
}