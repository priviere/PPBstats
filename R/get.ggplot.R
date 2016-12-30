# 0. help ----------
#' Get ggplot objects to visualize output from the analysis
#'
#' @description
#' \code{get.ggplot} returns ggplot objects to visualize outputs from the analysis
#'
#' @param data The data to plot. It can come from \code{get.mean.comparison}, \code{get.parameter.groups}, \code{predict.the.past}, \code{MC}$data_env_with_no_controls or \code{analye.outputs$model1.data_env_whose_param_did_not_converge}.
#' 
#' @param data_2 Outputs from \code{get.mean.comparisons} from model 2.For ggplot.type = "biplot-alpha-beta"
#' 
#' @param data_version data set with the following columns: "year", "location", "germplasm", "group", "version". The group refers to an id that contains two different versions. For example for group 1, there is version 1 and 2. See data(data_version) for an example.
#'
#' @param ggplot.type The type of plot you wish:
#' \itemize{
#'  \item from \code{get.mean.comparison}
#'  \itemize{
#'    \item from model 1 (\code{MC}) :  "barplot", "interaction", "score"
#'    \item from model 2 (\code{FWH}): "barplot", "biplot-alpha-beta"
#'    }
#'      
#'  \item from \code{get.parameter.groups} only from model 2 (\code{FWH}): "PCA"
#'  
#'  \item from \code{predict.the.past}$predict.the.past : "barplot", "interaction"
#'  
#'  \item from \code{MC}$data_env_with_no_controls : "barplot", "interaction"
#'  
#'  \item from \code{analye.outputs$model1.data_env_whose_param_did_not_converge} : "barplot", "interaction"
#'  
#'  }
#' 
#' @param nb_parameters_per_plot The number of parameters per plot to facilitate the visualization
#' 
#' @details
#' \itemize{
#' \item From \code{get.mean.comparison} and from model 1 (\code{MC}) there are one plot per environment.
#' For ggplot.type = "interaction" and ggplot.type = "score", nb_parameters_per_plot is the number of entries (mu_ij).
#' This is meaningful if you have data for several years.
#' 
#' ggplot.type = "score" display a plot with a score according to which group the entry was allocated.
#' An high score means that the entry was in a group with an high mean.
#' A low score means that the entry was in a group with an low mean.

#' \item From \code{get.parameter.groups} from model 2 (\code{FWH}) "PCA" display the PCA and the groups of parameters.
#' 
#' On each plot, the alpha value and the alpha correction are displayed.
#' alpha = Imp means that no differences were possible to find.
#' For ggplot.type = "interaction" and ggplot.type = "score", it is display under the form: alpha | alpha correction
#' 
#' \item ggplot.type = "biplot-alpha-beta" display the biplot with alpha_i on the x axis and beta_i on the y axis.
#' 
#' \item When using data_version, and ggplot.type = "barplot"; the pvalue is computed based on the MCMC.
#' For data that did not converge or without environments, it is a \code{t.test} which is perform.
#' }
#' 
#' @return 
#' The function returns a list of ggplot objects
#'  
#' @author Pierre Riviere
#' 
#' @seealso \code{\link{get.mean.comparisons}}, \code{\link{get.parameter.groups}}
#' 
#' 
get.ggplot = function(
  data,
  data_2 = NULL,
  data_version = NULL,
  ggplot.type = "interaction",
  nb_parameters_per_plot = 8
)
  # let's go !!! ----------
{
  if( attributes(data)$PPBstats.object == "check_model_model_1" ) { ggplot_check_model_model_1(data, nb_parameters_per_plot = nb_parameters_per_plot) }

  if( attributes(data)$PPBstats.object == "check_model_model_2" ) { ggplot_check_model_model_2(data, nb_parameters_per_plot = nb_parameters_per_plot) }
  
  if( attributes(data)$PPBstats.object == "check_model_GxE" ) { ggplot_check_model_GxE(data, nb_parameters_per_plot = nb_parameters_per_plot) }
  
  
  
  
  # 1. Error message and update arguments ----------
  if (is.null(attributes(data)$PPBstats.object)) { stop("data must come from functions get.parameter.groups, get.mean.comparisons, predict.the.past or analyse.outputs. See ?get.ggplot for more details.") }
  
  if( !is.element(ggplot.type, c("barplot", "biplot-alpha-beta", "interaction", "score", "PCA"))) { stop("ggplot.type must be either \"barplot\", \"biplot-alpha-beta\", \"interaction\", \"score\" or \"PCA\".") }
  
  if( attributes(data)$PPBstats.object == "parameter.groups.model2" & ggplot.type != "PCA" ) { stop("ggplot.type = \"PCA\" must be used with data from get.parameter.groups") }
  
  if( (attributes(data)$PPBstats.object == "mean.comparisons.model1" | attributes(data)$PPBstats.object == "mean.comparisons.model2") & ggplot.type == "PCA" ) { stop("With data coming from get.mean.comparisons, you must use ggplot.type = \"barplot\", \"biplot-alpha-beta\", \"interaction\" or \"score\".") }
  
  if( attributes(data)$PPBstats.object == "data_env_with_no_controls.model1" & is.element(ggplot.type, c("score", "PCA") ) ) { stop("With data coming from PPBstats::MC$data_env_with_no_controls, you must use ggplot.type = \"barplot\", \"interaction\".") }
  
  if( attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge" & is.element(ggplot.type, c("score", "PCA") ) ) { stop("With data coming from PPBstats::analyse.outputs$model1.data_env_whose_param_did_not_converge, you must use ggplot.type = \"barplot\", \"interaction\".") }

  if( attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge" & is.null(data) ) { stop("model1.data_env_whose_param_did_not_converge is NULL : no ggplot can be done ! ") }
  
  if( attributes(data)$PPBstats.object == "predict.the.past" & is.element(ggplot.type, c("score", "PCA") ) ) { stop("With data coming from predict.the.past, you must use ggplot.type = \"barplot\" or \"interaction\".") }

  if( ggplot.type == "biplot-alpha-beta" ) {
    if( !is.null(data) ) { if( attributes(data_2)$PPBstats.object != "mean.comparisons.model2") {
      stop("With gplot.type = \"biplot-alpha-beta\", data must come from get.mean.comparisons from model 2.")
    } }
    if( !is.null(data_2) ) { if( attributes(data_2)$PPBstats.object != "mean.comparisons.model2") {
      stop("data_2 must come from get.mean.comparisons from model 2.")
    } }
  }
  
  if( !is.null(data_version) ){
    mess = "The following column are compulsory in data_version : c(\"year\", \"germplasm\", \"location\", \"group\", \"version\"."
    if(!is.element("year", colnames(data_version))) { stop(mess) }
    if(!is.element("germplasm", colnames(data_version))) { stop(mess) }
    if(!is.element("location", colnames(data_version))) { stop(mess) }
    if(!is.element("group", colnames(data_version))) { stop(mess) }
    if(!is.element("version", colnames(data_version))) { stop(mess) }
    
    # delete version where there are not v1 AND v2
    vec_group = unique(data_version$group)
    for(gp in vec_group){
      d_tmp = droplevels(filter(data_version, group == gp))
      if(nlevels(d_tmp$version) != 2 ){ stop("There must be 2 levels per group in data_version. This is not the case for group ", gp) }
    }
  }
  

  
  
  
  
# Following to cut and paste
  
  
  
  if( attributes(data)$PPBstats.object == "mean.comparisons.model1" ) {
#    data_Mpvalue = data$Mpvalue
#    data = data$mean.comparisons
#    attributes(data)$PPBstats.object = "mean.comparisons.model1"
#
#    test.mu.m1 = length(grep("mu\\[", data$parameter)) > 0
#    test.beta.m1 = length(grep("beta\\[", data$parameter)) > 0  
  } else { test.mu.m1 = test.beta.m1 = FALSE }
  
  if( attributes(data)$PPBstats.object == "data_env_with_no_controls.model1" | 
      attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge" |
      attributes(data)$PPBstats.object == "predict.the.past" 
  ) {
    test.mu.m1 = TRUE # Just to pass the error message
  }
  
  if( attributes(data)$PPBstats.object == "mean.comparisons.model2" ){
    # data_Mpvalue = data$Mpvalue
    # data = data$mean.comparisons
    # attributes(data)$PPBstats.object = "mean.comparisons.model2"
    # 
    # test.alpha.m2 = length(grep("alpha\\[", data$parameter)) > 0
    # test.beta.m2 = length(grep("beta\\[", data$parameter)) > 0
    # test.theta.m2 = length(grep("theta\\[", data$parameter)) > 0  
  } else { test.alpha.m2 = test.beta.m2 = test.theta.m2 = FALSE}
  
  
    if(attributes(data)$PPBstats.object == "predict.the.past") { # To have the same format for next steps
      data = plyr::rename(data, replace = c("50%" = "median"))
    }
  
  # 2. Display ggplots ----------
  
  
  # 2.1. barplot ----------
  
  if( (attributes(data)$PPBstats.object == "mean.comparisons.model1" | 
      attributes(data)$PPBstats.object == "data_env_with_no_controls.model1" |
      attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge" |
      attributes(data)$PPBstats.object == "predict.the.past")
       & ggplot.type == "barplot"
  ) {  
    d_env = plyr:::splitter_d(data, .(environment))
    
    if(!is.null(data_version)) {
      
      # data_version$group = factor(rep(unlist(tapply(data_version$germplasm, data_version$group, function(x){paste(as.character(x), collapse = " | ")})), each = 2))
      
      data_version$environment = paste(data_version$location, ":", data_version$year, sep = "")
      data_version$mu = paste("mu[", data_version$germplasm, ",", data_version$environment, "]", sep = "")
      vec_env = unique(data_version$environment)
      vec_env_to_get = vec_env[is.element(vec_env, names(d_env))]
      vec_env_not_to_get = vec_env[!is.element(vec_env, names(d_env))]
      if( length(vec_env_not_to_get) > 0 ){ warning("The following environments in data_version are not taken: ", paste(vec_env_not_to_get, collapse = ", "),".") }
      
      if( length(vec_env_to_get) == 0 ) { stop("There are no environment to display.") }
      
        d_env = d_env[vec_env_to_get]

        fun = function(x, data_version){
          test = grep("mu", x$parameter)
          if( length(test) == 0 ) { x$parameter = paste("mu", x$parameter, sep = "") } # i.e. "data_env_with_no_controls.model1" | "model1.data_env_whose_param_did_not_converge")
          p_to_get = filter(data_version, environment == x$environment[1])$mu
          x = filter(x, parameter %in% p_to_get)
          x$max = max(x$median, na.rm = TRUE)
          x = arrange(x, parameter)
          x$split = add_split_col(x, nb_parameters_per_plot)
          x_split = plyr:::splitter_d(x, .(split))
          return(x_split)
        }
        
        d_env_b = lapply(d_env, fun, data_version)

        OUT = lapply(d_env_b, function(x){
          out = lapply(x, function(dx){
            if(attributes(data)$PPBstats.object == "mean.comparisons.model1") { # Add letters of significant groups
              
              env = dx$environment[1]
              data_Mpvalue_env = data_Mpvalue[[env]]
              data_version_tmp = droplevels(filter(data_version, environment == env))
              gp = unique(data_version_tmp$group)
              STARS = NULL
              for(g in gp){
                dtmp = droplevels(filter(data_version_tmp, group == g))
                vec_version = levels(dtmp$version)
                v1 = unique(as.character(filter(dtmp, version == vec_version[1])$mu))
                v2 = unique(as.character(filter(dtmp, version == vec_version[2])$mu))
                for (i in 1:ncol(data_Mpvalue_env)) { 
                  if (colnames(data_Mpvalue_env)[i] == v1) {c1 = i}
                  if (colnames(data_Mpvalue_env)[i] == v2) {c2 = i}
                }
                pvalue = data_Mpvalue_env[min(c1,c2), max(c1,c2)]

                if(is.null(pvalue)) { stars = " "} else {
                  if(pvalue < 0.001) { stars = "***" }
                  if(pvalue > 0.001 & pvalue < 0.05) { stars = "**" }
                  if(pvalue > 0.05 & pvalue < 0.01) { stars = "*" }
                  if(pvalue > 0.01) { stars = "." }
                }
                names(stars) = g
                STARS = c(STARS, stars)
              }

              colnames(dx)[which(colnames(dx) == "parameter")] = "mu"
              d = join(data_version_tmp, dx, by = "mu")
              
              # delete version where there are v1 AND v2
              group_to_keep = NULL
              vec_group = unique(d$group)
              
              for(gp in vec_group){
                d_tmp = droplevels(d[d$group %in% gp,])
                t = tapply(d_tmp$median, d_tmp$version, mean, na.rm = TRUE)
                if(!is.na(t[1]) & !is.na(t[2])){ group_to_keep = c(group_to_keep, gp)} 
              }
              
              d = droplevels(d[d$group %in% group_to_keep,])
              STARS = STARS[is.element(names(STARS), group_to_keep)]

              p = ggplot(d, aes(x = group, y = median)) + geom_bar(aes(fill = version), stat = "identity", position = "dodge")
              y = tapply(d$median, d$group, mean, na.rm = TRUE)
              y = y + (max(y) * 0.2)
              label_stars = data.frame(group = names(STARS), median = y[names(STARS)], STARS = STARS)
              p = p + geom_text(data = label_stars, aes(label = STARS))
              p = p + xlab("") + theme(axis.text.x = element_text(angle = 90))
              }
          
          if(attributes(data)$PPBstats.object == "data_env_with_no_controls.model1" |
             attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge") {
            
            env = dx$environment[1]
            data_version_tmp = droplevels(filter(data_version, environment == env))
            
            gp = unique(data_version_tmp$group)
            STARS = NULL
            for(g in gp){
              dtmp = droplevels(filter(data_version_tmp, group == g))
              vec_version = levels(dtmp$version)
              v1 = unique(as.character(filter(dtmp, version == vec_version[1])$median))
              v2 = unique(as.character(filter(dtmp, version == vec_version[2])$median))

              if( length(v1) > 1 & length(v2) > 1) {
                pvalue = t.test(v1, v2)$p.value
              } else { pvalue = NULL; warning("No t.test are done as there are not enough observations.") }
              
              if(is.null(pvalue)) { stars = " "} else {
                if(pvalue < 0.001) { stars = "***" }
                if(pvalue > 0.001 & pvalue < 0.05) { stars = "**" }
                if(pvalue > 0.05 & pvalue < 0.01) { stars = "*" }
                if(pvalue > 0.01) { stars = "." }
              }
              names(stars) = g
              STARS = c(STARS, stars)
            }
            
            colnames(dx)[which(colnames(dx) == "parameter")] = "mu"
            d = join(data_version_tmp, dx, "mu")

            p = ggplot(d, aes(x = group, y = median)) + geom_bar(aes(fill = version), stat = "identity", position = "dodge")
            
            y = tapply(d$median, d$group, mean, na.rm = TRUE)
            y = y + (max(y) * 0.2)
            label_stars = data.frame(group = names(STARS), median = y[names(STARS)], STARS = STARS)
            p = p + geom_text(data = label_stars, aes(label = STARS))
            p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, dx[1,"max"])
          }
            
          return(p)
        })
        return(out)
      })
        names(OUT) = names(d_env_b)
        
    
        } else {

      d_env_b = lapply(d_env, function(x){
        x = arrange(x, median)
        x$max = max(x$median, na.rm = TRUE)
        x$split = add_split_col(x, nb_parameters_per_plot)
        x_split = plyr:::splitter_d(x, .(split))
        return(x_split)
      } )
      
      OUT = lapply(d_env_b, function(x){
        out = lapply(x, function(dx){
          p = ggplot(dx, aes(x = reorder(parameter, median), y = median)) + geom_bar(stat = "identity")

          if(attributes(data)$PPBstats.object == "mean.comparisons.model1") { # Add letters of significant groups
            p = p + geom_text(data = dx, aes(x = reorder(parameter, median), y = median/2, label = groups), angle = 90, color = "white")
            p = p + ggtitle(paste(dx[1, "environment"], "\n alpha = ", dx[1, "alpha"], "; alpha correction :", dx[1, "alpha.correction"])) + ylab("")
          }
          
          if(attributes(data)$PPBstats.object == "data_env_with_no_controls.model1" |
             attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge") {
            p = p + ggtitle(dx[1, "environment"]) + ylab("")
          }
          
          if(attributes(data)$PPBstats.object == "predict.the.past") {
            p = p + ggtitle(dx[1, "environment"]) + ylab("predicted value")
          }
          
          p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, dx[1,"max"])
          return(p)
        })
        return(out)
      })
      names(OUT) = names(d_env_b)
    
    }
     
  }
  
  
  
  # 2.4. PCA ----------
  if(ggplot.type == "PCA"){  
    
    obj = data$obj.pca
    clust = data$clust$clust; names(clust) = rownames(data$clust)
    
    # Based on the code of Ben Marwick
    # https://gist.github.com/benmarwick/2139672
    
    # Individuals
    PC1 <- obj$ind$coord[,1]
    PC2 <- obj$ind$coord[,2]
    PCs <- cbind.data.frame(PC1,PC2)
    PCs$labs = rownames(obj$ind$coord)
    PCs$cluster = clust[PCs$labs]
    
    pind = ggplot(data = PCs, aes(PC1, PC2, label = labs, colour = cluster)) + geom_text()
    pind = pind + xlab(paste("PC1 (", round(obj$eig[1,2], 2), "%)", sep="")) + ylab(paste("PC2 (", round(obj$eig[2,2], 2), "%)", sep="") )
    
    # Variables
    vPC1 <- obj$var$coord[,1]
    vPC2 <- obj$var$coord[,2]
    vlabs <- rownames(obj$var$coord)
    vPCs <- data.frame(cbind(vPC1,vPC2))
    vPCs$labs = vlabs
    
    # put a faint circle there, as is customary
    angle <- seq(-pi, pi, length = 50)
    df <- data.frame(x = sin(angle), y = cos(angle))
    pvar <- ggplot() + geom_path(aes(x, y), data = df, colour="grey70")
    
    # add on arrows and variable labels
    pvar <- pvar + geom_text(data=vPCs, aes(x=vPC1,y=vPC2,label=labs), size=4) + xlab(paste("PC1 (", round(obj$eig[1,2], 2), "%)", sep="")) + ylab(paste("PC2 (", round(obj$eig[2,2], 2), "%)", sep="") )
    pvar <- pvar + geom_segment(data=vPCs, aes(x = 0, y = 0, xend = vPC1*0.9, yend = vPC2*0.9), arrow = arrow(length = unit(0.1, "cm")), color = "grey30")
    
    OUT = list("ind" = pind, "var" = pvar)
    return(OUT)
  }
  

  return(OUT)
}

