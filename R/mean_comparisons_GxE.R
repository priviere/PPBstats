mean_comparisons_GxE = function(
  out_check_model, 
  p.adj = "none"
  ){
  # 1. Error message
  if( attributes(out_check_model)$PPBstats.object != "check_model_GxE" ) { stop("data must come from check_model and GxE") }
  
  model = out_check_model$GxE$ANOVA$model
  
  data_ggplot_LSDbarplot = function(model, fac, p.adj){
    fac = "germplasm"
    LSD = LSD.test(model, fac, p.adj = p.adj)
    LSD$groups$trt = factor(LSD$groups$trt, levels = LSD$groups$trt)
    d_LSD = LSD$groups
    d_LSD$ymean = d_LSD$means / 2
    return(d_LSD)
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