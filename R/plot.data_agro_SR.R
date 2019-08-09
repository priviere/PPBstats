#' Plot agro object from format_data_PPBstats.data_agro_SR()
#'
#' @description
#' \code{plot.data_agro_SR} gets ggplot to describe the data
#' 
#' @param x The data frame. It should come from \code{\link{format_data_PPBstats.data_agro_SR}}
#' 
#' @param mean_comparisons Output from \code{\link{mean_comparisons}}.
#' 
#' @param plot_type the type of plot you wish. It can be :
#' \itemize{
#'  \item "barplot", where sd error are displayed
#'  \item "boxplot"
#'  \item "interaction"
#' }
#' 
#' @param vec_variables vector of variables to display
#' 
#' @param nb_parameters_per_plot_x_axis the number of parameters per plot on x_axis arguments
#' 
#' @param nb_parameters_per_plot_in_col the number of parameters per plot for in_col arguments
#' 
#' @param ... further arguments passed to or from other methods
#' 
#' @return 
#' The function returns a list with ggplot objects for each variable of vec_variables divided then for each element expe_id.
#' When argument mean_comparison is not NULL, it returns a plot with stars of significant differences.
#' 
#' @author Pierre Riviere
#' 
#' @details 
#' S3 method.
#' See the book for more details \href{https://priviere.github.io/PPBstats_book/response-to-selection.html}{here}.
#' 
#' @seealso 
#' \itemize{
#'  \item \code{\link{format_data_PPBstats}}
#'  \item \code{\link{format_data_PPBstats.data_agro}}
#' }
#' 
#' @export
#' 
plot.data_agro_SR = function(
  x,
  mean_comparisons = NULL,
  plot_type = "boxplot",
  vec_variables = NULL,
  nb_parameters_per_plot_x_axis = 5,
  nb_parameters_per_plot_in_col = 5, ...
){
  match.arg(plot_type, c("barplot", "boxplot", "interaction"), several.ok = FALSE)
  stars = group = expe_id = NULL # to avoid no visible binding for global variable
  
  d = droplevels(x[which(!is.na(x$expe_id)),])
  d$x_axis = paste(d$seed_lot, d$group, sep = " | ")
  
  fun = function(variable, d, mean_comparisons, plot_type, nb_parameters_per_plot_x_axis, nb_parameters_per_plot_in_col){
    s = splitter_d(d, .(expe_id))
    p_out = lapply(s,
                   plot_descriptive_data,
                   plot_type,
                   x_axis = "group",
                   in_col = "version",
                   vec_variables = variable,
                   nb_parameters_per_plot_x_axis,
                   nb_parameters_per_plot_in_col
    )
    p_out = lapply(p_out, function(x){ x[[1]] = x[[1]][[1]] }) # remove variable name in list
    
    for(i in 1:length(p_out)){
      Rbouquet = as.character(dplyr::filter(s[[i]], group == "R" & version == "bouquet")$seed_lot[1])
      Rvrac = as.character(dplyr::filter(s[[i]], group == "R" & version == "vrac")$seed_lot[1])
      Sbouquet = as.character(dplyr::filter(s[[i]], group == "S" & version == "bouquet")$seed_lot[1])
      Svrac = as.character(dplyr::filter(s[[i]], group == "S" & version == "vrac")$seed_lot[1])
      lab = na.omit(c(Rbouquet, Rvrac, Sbouquet, Svrac))
      p_out[[i]] = p_out[[i]] + geom_text(label = lab,
                                          position = position_dodge(0.9),
                                          angle = 90, hjust = 1)
      } # add seed lot name on bar
    
    
    get_stars = function(pvalue){
      if(is.null(pvalue)) { stars = " "} else {
        if(pvalue < 0.001) { stars = "***" }
        if(pvalue > 0.001 & pvalue < 0.05) { stars = "**" }
        if(pvalue > 0.05 & pvalue < 0.01) { stars = "*" }
        if(pvalue > 0.01) { stars = "." }
      }
      return(stars)
    }
    
      
    if( !is.null(mean_comparisons) ){
      
      if( is(mean_comparisons, "mean_comparisons_model_bh_intra_location") ) {
        
        for(i in 1:length(p_out)){
          locyear = paste(s[[i]][1, "location"], s[[i]][1, "year"], sep = ":")
          
          mc_mcmc = mean_comparisons$data_mean_comparisons
          to_get = which(names(mc_mcmc) == locyear)
          if( length(to_get) > 0 ){
            mc_mcmc = mean_comparisons$data_mean_comparisons[to_get][[1]]$Mpvalue # MCMC
          } else { mc_mcmc = NULL }

          mc_ttest_1 = mean_comparisons$data_env_with_no_controls
          to_get_1 = which(names(mc_ttest_1) == locyear)
          if( length(to_get_1) > 0 ){
            mc_ttest = mc_ttest_1[to_get_1][[to_get_1]]$mean.comparisons # t.test to perform on pairs 
          } else { mc_ttest = NULL }

          mc_ttest_2 = mean_comparisons$data_env_whose_param_did_not_converge
          to_get_2 = which(names(mc_ttest_2) == locyear)
          if( length(to_get_2) > 0 ){
            mc_ttest = mc_ttest_2[to_get_2][[to_get_2]]$mean.comparisons # t.test to perform on pairs 
          } else { mc_ttest = NULL }
          
          title = NULL
          
          if( !is.null(mc_mcmc) ) {
            title = "Significant differences with Hierarchical Bayesian model intra location"
            mc = mc_mcmc
                        
            R = dplyr::filter(s[[i]], group == "R")
            sl1_R = paste("mu[", R[1, "germplasm"], ",", locyear, "]", sep = "")
            sl2_R = paste("mu[", R[2, "germplasm"], ",", locyear, "]", sep = "")
            if( is.element(sl1_R, colnames(mc)) & is.element(sl2_R, colnames(mc)) ) {
              pvalue_R = max(mc[sl1_R, sl2_R], mc[sl2_R, sl1_R]) # One is 0 (cf triangular matrix)
              stars_R = get_stars(pvalue_R)
            } else { stars_R = NULL }
            
            S = dplyr::filter(s[[i]], group == "S")
            sl1_S = paste("mu[", S[1, "germplasm"], ",", locyear, "]", sep = "")
            sl2_S = paste("mu[", S[2, "germplasm"], ",", locyear, "]", sep = "")
            if( is.element(sl1_S, colnames(mc)) & is.element(sl2_S, colnames(mc)) ) {
              pvalue_S = max(mc[sl1_S, sl2_S], mc[sl2_S, sl1_S]) # One is 0 (cf triangular matrix)
              stars_S = get_stars(pvalue_S)
            } else { stars_S = NULL }
          }
          
          if( !is.null(mc_ttest) ) {
            title = "Significant differences with t test"
            mc = mc_ttest
            
            R = dplyr::filter(s[[i]], group == "R")
            sl1_R = dplyr::filter(R, version == "bouquet")[, variable]
            sl2_R = dplyr::filter(R, version == "vrac")[, variable]
            if( length(sl1_R) > 1 & length(sl2_R) > 1) {
              pvalue_R = t.test(sl1_R, sl2_R)$p.value
              stars_R = get_stars(pvalue_R)
            } else {
              stars_R = NULL
              warning("No t.test are done as there are not enough observations.")
            }
            
            S = dplyr::filter(s[[i]], group == "S")
            sl1_S = dplyr::filter(S, version == "bouquet")[, variable]
            sl2_S = dplyr::filter(S, version == "vrac")[, variable]
            if( length(sl1_S) > 1 & length(sl2_S) > 1) {
              pvalue_S = t.test(sl1_S, sl2_S)$p.value
              stars_S = get_stars(pvalue_S)
            } else {
              stars_S = NULL
              warning("No t.test are done as there are not enough observations.")
            }
          }
          
          if( !is.null(stars_R) & !is.null(stars_S) ){
            label_stars = data.frame(x_axis = c("R", "S"), mean = c(20, 20) , stars = c(stars_R, stars_S)) 
            p_out[[i]] = p_out[[i]] + geom_label(data = label_stars, aes(label = stars)) + ggtitle(title)
          }
          if( is.null(stars_R) & !is.null(stars_S) ){
            label_stars = data.frame(x_axis = c("S"), mean = c(20) , stars = c(stars_S)) 
            p_out[[i]] = p_out[[i]] + geom_label(data = label_stars, aes(label = stars)) + ggtitle(title)
          }
          if( !is.null(stars_R) & is.null(stars_S) ){
            label_stars = data.frame(x_axis = c("R"), mean = c(20) , stars = c(stars_R)) 
            p_out[[i]] = p_out[[i]] + geom_label(data = label_stars, aes(label = stars)) + ggtitle(title)
          }
          if( !is.null(stars_R) & !is.null(stars_S) ){ "nothing" }
          }
      }
    }
    
    return(p_out)
  }
  
  out = lapply(vec_variables, 
               fun, 
               d, 
               mean_comparisons,
               plot_type, 
               nb_parameters_per_plot_x_axis, 
               nb_parameters_per_plot_in_col
               )
  names(out) = vec_variables
  
  return(out)
}


