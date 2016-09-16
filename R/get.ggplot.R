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
#' @param data_version data set with the following columns: "year", "location", "germplasm", "group", "version". The group refers to an id that contains the different versions. For example for group 1, there is version 1 and 2. See data(data_version) for an example.
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
#' For data that did not converge or without environments, it is a \texttt{t.test} which is perform.
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
  }
  
  
  if( attributes(data)$PPBstats.object == "mean.comparisons.model1" ) {
    data_Mpvalue = data$Mpvalue
    data = data$mean.comparisons
    attributes(data)$PPBstats.object = "mean.comparisons.model1"

    test.mu.m1 = length(grep("mu\\[", data$parameter)) > 0
    test.beta.m1 = length(grep("beta\\[", data$parameter)) > 0  
  } else { test.mu.m1 = test.beta.m1 = FALSE }
  
  if( attributes(data)$PPBstats.object == "data_env_with_no_controls.model1" | 
      attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge" |
      attributes(data)$PPBstats.object == "predict.the.past" 
  ) {
    test.mu.m1 = TRUE # Just to pass the error message
  }
  
  if( attributes(data)$PPBstats.object == "mean.comparisons.model2" ){
    data_Mpvalue = data$Mpvalue
    data = data$mean.comparisons
    attributes(data)$PPBstats.object = "mean.comparisons.model2"

    test.alpha.m2 = length(grep("alpha\\[", data$parameter)) > 0
    test.beta.m2 = length(grep("beta\\[", data$parameter)) > 0
    test.theta.m2 = length(grep("theta\\[", data$parameter)) > 0  
  } else { test.alpha.m2 = test.beta.m2 = test.theta.m2 = FALSE}
  
  
  if( (!test.mu.m1 & !test.beta.m1) & ggplot.type == "interaction") { stop("ggplot.type == \"interaction\" is possible only with output from model1 (MC).") }
  
  if( (!test.mu.m1 & !test.beta.m1) & ggplot.type == "score") { stop("ggplot.type == \"score\" is possible only with output from model1 (MC).") }
  
  
  if( attributes(data)$PPBstats.object == "mean.comparisons.model1" | 
      attributes(data)$PPBstats.object == "data_env_with_no_controls.model1" |
      attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge" |
      attributes(data)$PPBstats.object == "predict.the.past"
  ) {
    
    para = unlist(strsplit(as.character(data$parameter)[1], "\\["))[1]
    
    data$entry = sub(paste(para, "\\[", sep=""), "", sapply(data$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
    data$environment =  sub("\\]", "", sapply(data$parameter, function(x){unlist(strsplit(as.character(x), ","))[2]}))
    data$location = sapply(data$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
    data$year = sapply(data$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
    
    if(attributes(data)$PPBstats.object == "data_env_with_no_controls.model1" | 
       attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge") { # To have the same format for next steps
      data = plyr::rename(data, replace = c("variable" = "median"))
    }
    if(attributes(data)$PPBstats.object == "predict.the.past") { # To have the same format for next steps
      data = plyr::rename(data, replace = c("50%" = "median"))
    }
  }
  
  add_split_col = function(x, each){ rep(c(1:nrow(x)), each = each)[1:nrow(x)] } 
  
  # 2. Display ggplots ----------
  
  get.loc.year = function(data, nb_parameters_per_plot){
    
    d_loc = plyr:::splitter_d(data, .(location))
    
    d_loc_b = lapply(d_loc, function(x){
      x = arrange(x, entry)
      x$split = as.numeric(factor(x$entry))
      seq_nb_para = unique(c(seq(1, max(x$split), nb_parameters_per_plot), max(x$split)*2))
      for(i in 1:(length(seq_nb_para) - 1) ) { x$split[seq_nb_para[i] <= x$split & x$split < seq_nb_para[i+1]] = i }
      x_split = plyr:::splitter_d(x, .(split))
      return(x_split)
    } )
    
    return(d_loc_b)
  }
  
  # 2.1. barplot ----------
  
  if( (attributes(data)$PPBstats.object == "mean.comparisons.model1" | 
      attributes(data)$PPBstats.object == "data_env_with_no_controls.model1" |
      attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge" |
      attributes(data)$PPBstats.object == "predict.the.past")
       & ggplot.type == "barplot"
  ) {  
    
    d_env = plyr:::splitter_d(data, .(environment))
    
    if(!is.null(data_version)) {
      
      data_version$group = factor(rep(unlist(tapply(data_version$germplasm, data_version$group, function(x){paste(x, collapse = " | ")})), each = 2))
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
          x = arrange(x, median)
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
                v1 = as.character(filter(dtmp, version == "v1")$mu)
                v2 = as.character(filter(dtmp, version == "v2")$mu)
                pvalue = data_Mpvalue_env[v1, v2]
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
              v1 = as.character(filter(dtmp, version == "v1")$median)
              v2 = as.character(filter(dtmp, version == "v2")$median)
              
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
  
  
    if(attributes(data)$PPBstats.object == "mean.comparisons.model2" & ggplot.type == "barplot") {  
    data = arrange(data, median)  
    data$max = max(data$median, na.rm = TRUE)
    data$split = add_split_col(data, nb_parameters_per_plot)
    data_split = plyr:::splitter_d(data, .(split))  
    
    para.name = unlist(strsplit(as.character(data[1, "parameter"]), "\\["))[1]
    
    OUT = lapply(data_split, function(dx){
      p = ggplot(dx, aes(x = reorder(parameter, median), y = median)) + geom_bar(stat = "identity")
      p = p + geom_text(data = dx, aes(x = reorder(parameter, median), y = median/2, label = groups), angle = 90, color = "white")
      p = p + ggtitle(paste(para.name, "\n alpha = ", dx[1, "alpha"], "; alpha correction :", dx[1, "alpha.correction"]))
      p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, data[1,"max"]) + ylab("")
      return(p)
    })
    
    OUT = list(OUT)
    names(OUT) = para.name
  }
  
  
  # 2.2. interaction ----------
  if(ggplot.type == "interaction") {
    
    d_loc_out = get.loc.year(data, nb_parameters_per_plot)
    
    OUT = lapply(d_loc_out, function(x){
      out = lapply(x, function(x_loc) {
        # Same scale for all the plots
        ymin = min(x_loc$median, na.rm = TRUE)
        ymax = max(x_loc$median, na.rm = TRUE)  
        
        if( attributes(data)$PPBstats.object == "mean.comparisons.model1" ){
          alpha.info = paste(x_loc$alpha, "|", x_loc$alpha.correction)
          x_loc$alpha.info_year = as.factor(paste(x_loc$year, alpha.info, sep = " - "))      
        }
        
        if( attributes(data)$PPBstats.object == "data_env_with_no_controls.model1" | 
            attributes(data)$PPBstats.object == "model1.data_env_whose_param_did_not_converge" |
              attributes(data)$PPBstats.object == "predict.the.past"
        ) {
          x_loc$alpha.info_year = x_loc$year
        }
        
        p = ggplot(x_loc, aes(y = median, x = alpha.info_year, colour = entry, group = entry))
        p = p + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line") + ggtitle(x_loc[1, "location"])
        p = p + xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank())
        
        if( attributes(data)$PPBstats.object == "predict.the.past" ) {
          p = p + ggtitle(x_loc[1, "environment"]) + ylab("predicted value")      
        }
        
        x_loc_year = plyr:::splitter_d(x_loc, .(year))    
        
        # Put lines for significant groups
        if( attributes(data)$PPBstats.object == "mean.comparisons.model1" ){
          SEG = NULL
          
          letters = c(letters, paste(rep(LETTERS, times = 15), rep(c(1:5), each = 15), sep = ""))
          gp_letters = letters[1:max(x_loc$nb_group)]
          xadjust = seq(0.05, 0.9, length = 25) # it is assumed max 25 letters (i.e. groups)
          xadjust = xadjust[c(1:length(gp_letters))]
          names(xadjust) = gp_letters
          
          for(i in 1:length(x_loc_year)) {
            subx = droplevels(x_loc_year[[i]])
            
            # get rid of non sens information
            # This is useful if a germplasm is in group 'a' and 'b' alone. This is possible because other germplasm in groups 'a' and 'b' are in another year
            subx = arrange(subx, median) # To get the letter in the right order
            groups = as.character(subx$groups)
            
            a = lapply(groups, function(x) { unlist(strsplit(x, "")) } )
            row = unique(unlist(a))
            m = matrix(0, ncol = length(groups), nrow = length(row))
            rownames(m) = row
            for(jj in 1:length(a)) { m[a[[jj]], jj] = 1 }
            m = unique(m)
            if( is.vector(m) ) { m = as.data.frame(matrix(m, nrow = 1)) }
            
            if( nrow(m) > 1) {
              todelete = NULL
              for(jj in 1:ncol(m)) {
                toget = which(m[,jj] == 1)
                if( length(toget) >  1 ) {    
                  for(ii in toget) {
                    if( sum(m[ii,]) == 1 ) { todelete = c(todelete, ii) }
                  } 
                } 
              }
              
              if (!is.null(todelete)) { m = m[-todelete,] }
              if( is.vector(m) ) { m = as.data.frame(matrix(m, nrow = 1)) }
            }
            
            
            # Following code to discard redondant informations
            # For example
            # 1 1 0 0
            # 0 1 1 1
            # 0 1 1 0
            # will give
            # 1 1 0 0
            # 0 1 1 1
            # indeed, the last row brings no informations
            
            if( nrow(m) > 1) {
              todelete = NULL
              for(ii in 1:(nrow(m)-1) ) { 
                w1 = which( m[ii,] == 1 )
                w2 = which( m[ii+1,] == 1 )
                t = which(is.element(w1, w1[is.element(w1, w2)]))
                test = length(w2) == length(t)
                
                if( test ) { todelete = c(todelete, ii+1)}
              }
              if( !is.null(todelete) ) { m = m[-todelete,] }
              if( is.vector(m) ) { m = as.data.frame(matrix(m, nrow = 1)) }
            }
            
            
            # Initialize the letters
            if( nrow(m) > 1) {
              for(ii in 1:nrow(m)) { m[ii, which(m[ii,] == 1)] = letters[ii]; m[ii, which(m[ii,] == 0)] = ""  }
            } else { m[1, which(m[1,] == 1)] = letters[1]  }
            
            groups = apply(m, 2, function(x){paste(x, collapse="")})
            
            subx$groups = factor(groups)
            
            for(l in gp_letters) {
              togrep = grep(l, subx[,"groups"])
              
              if(length(togrep) > 0) {
                # check if it is continuous
                vec.togrep = NULL
                
                a = togrep
                if( length(a) > 1 ){ 
                  test = a[1:(length(a)-1)] + 1 == a[2:length(a)]
                  t = c(FALSE, test)
                  t[which(t)] = 1; t[which(!t)] = 0
                  b = paste("0",unlist(strsplit(paste(as.character(t[2:length(t)]),collapse=""),"0")),sep="")
                  if(t[length(t)] == 0) { b = c(b, 0) }
                  c = c(0, cumsum(sapply(b, function(x){nchar(x)})))
                  for(k in 1:(length(c)-1)) { vec.togrep = c(vec.togrep, (a[(c[k]+1):c[k+1]])) }
                } else { vec.togrep = c(vec.togrep, togrep) }
                
                toto = subx[vec.togrep, "median"]
                
                y.seg = range(toto)
                x.seg = i + xadjust[l]
                alpha.info = paste(as.character(subx[1, "alpha"]),"|", as.character(subx[1, "alpha.correction"]))
                seg = cbind.data.frame(ymin = y.seg[1], ymax = y.seg[2], x = x.seg, groups = paste("group", l), alpha.info, year = names(x_loc_year)[i])
                SEG = rbind.data.frame(SEG, seg)
              }
            }
          }
          
          # For germplasm alone in one group, change ymax in order to see it
          for(i in 1:nrow(SEG)){
            if( SEG$ymin[i] == SEG$ymax[i] ) { SEG$ymax[i] = SEG$ymax[i] + min(SEG$ymax)/30 }
          }
          
          
          if(!is.null(SEG)) {
            p = p + geom_segment(aes(x = x, y = ymin, xend = x, yend = ymax, group = NULL), colour =  as.numeric(as.factor(SEG$groups)), data = SEG)
            p = p + ylim(ymin - ymin/10, ymax + ymax/10) # To be sure to see the line of the groups
          }      
        }
        
        return(p)        
      })
      return(out)
    } )
    
    names(OUT) = names(d_loc_out)
    
  }
  
  # 2.3. score ----------
  if(ggplot.type == "score") {  
    
    d_loc_out = get.loc.year(data, nb_parameters_per_plot)
    OUT = lapply(d_loc_out, function(x){
      out = lapply(x, function(env){
        # assign a number according to the group
        vec_letters = sort(unique(unlist(sapply(as.character(env[,"groups"]), function(x){unlist(strsplit(x, ""))}))), decreasing = TRUE)
        
        SCORE = c(1:1000)
        SEQ = sort(unique(c(seq(1, length(SCORE), floor(length(SCORE)/length(vec_letters))), max(SCORE))), decreasing = TRUE)
        GP = as.character(env[,"groups"])
        gp_nb = rep(0, length(GP))
        
        for(l in 1:length(vec_letters)) {
          b = grep(vec_letters[l], GP)
          GP[b] = "" 
          gp_nb[b] = SEQ[l]
        }
        env$score = gp_nb  
        
        alpha.info = paste(env$alpha, "|", env$alpha.correction)
        env$alpha.info_year = paste(env$year, alpha.info, sep = " - ")
        
        p = ggplot(env, aes(y = entry, x = alpha.info_year)) + geom_tile(aes(fill = score))
        p = p + scale_fill_gradient(low = "blue",high = "red")
        p = p + xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(env[1, "location"])
        return(p)
      })
      return(out)
    })
    
    names(OUT) = names(d_loc_out)
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
  

  # 2.5. biplot-alpha-beta ----------
  if(ggplot.type == "biplot-alpha-beta"){
    
    a = data$mean.comparisons
    test_a = unlist(strsplit(as.character(a[1,"parameter"]), "\\["))[1]
    if( test_a != "alpha" ){ stop("With ggplot.type = \"biplot-alpha-beta\", data must come from get.mean.comparisons with paramater = \"alpha\".") }
    a$germplasm = gsub("alpha", "", a$parameter)
    colnames(a)[which(colnames(a) == "parameter")] = "parameter_a"
    colnames(a)[which(colnames(a) == "median")] = "alpha_i"
    
    b = data_2$mean.comparisons
    test_b = unlist(strsplit(as.character(b[1,"parameter"]), "\\["))[1]
    if( test_b != "beta" ){ stop("With ggplot.type = \"biplot-alpha-beta\", data_2 must come from get.mean.comparisons with paramater = \"beta\".") }
    b$germplasm = gsub("beta", "", b$parameter)
    colnames(b)[which(colnames(b) == "parameter")] = "parameter_b"
    colnames(b)[which(colnames(b) == "median")] = "beta_i"
    
    ab = join(a, b, "germplasm")
    ab$germplasm = gsub("\\[", "", ab$germplasm)
    ab$germplasm = gsub("\\]", "", ab$germplasm)
    
    p = ggplot(ab, aes(x = alpha_i, y = beta_i, label = germplasm)) 
    p = p + geom_text() + geom_hline(yintercept = 0)
    OUT = list("biplot-alpha-beta" = p)
  }
  
  return(OUT)
}

# A faire :
#
# 1. utiliser get.ggplot de shinemas2R pour faire les plots sans stats, et réfléchir à ajouter une layer pour ajouter les stats 
# truc du genre : shinemas2R::get.ggplot + stats_info()
#
# 2. ajouter AMMI avec tout les graphs ici, faire la meme logique avec model, check model puis graph
#
# 3. La vignette sera en fait le doc de formation pour l'analyse des données: faire dans vignette
# Analyse descriptive des données (tout avec shinemas2R::get.ggplot mais dans ce cas revoir les atributes pour que ça soit plus générique ?!? A creuser ... avoir une fonction qui permet de checker les attribus avant de les rentrer dans get.ggplot, fonction qui check si on a bien les sorties de type BDD? Mettre un argument data.from.shinemas2R pour vérifier les attributes?
#
#et du coup que get.data dans shinemas2R? Qui finalement, pourrait être directement dans la BDD avec juste une fonction dans R qui demande à la base de sortir les infos? donc shinemas2R aurait une seule fonction de 1 ligne !? A rediscuter avec Yannick pour caler un stage là dessus)
#
# Faire des graphs de relation entre fonctions pour chaque niveau d'analyse (decriptif, balance, unbalanced)
# Ajouter une fonction qui te dit si c'est balance ou unbalanced? Cf experimental_design plot
# 
# Analyse statistique des données
#
# ########################################################################
# # get.groups pour récupérer les groupes de significativité
# ########################################################################
# get.groups = function(variable, donnees, data.info, out, x.axis, in.col, data.display, titre, typeI.error)
# {
# get.stars = function(pvalue)
# {
# if(is.null(pvalue)) { stars = " "} else {
#     								if(pvalue < 0.001) { stars = "***" }
# 										if(pvalue > 0.001 & pvalue < 0.05) { stars = "**" }
# 										if(pvalue > 0.05 & pvalue < 0.01) { stars = "*" }
# 										if(pvalue > 0.01) { stars = "." }
# 										}
# return(stars)
# }
# 
# tab = NULL
# analysis.stats = data.info$analysis.stats
# if(is.null(data.display)) { data.display = "NONE"}
# if(is.null(x.axis)) { x.axis = "NONE"}
# if(is.null(in.col)) { in.col = "NONE"}
# 
# # faire une liste avec un tab par y:p, ensuite, on chope le bon ou alors un grep(p, list des tab) pour interaction, dansplot.it, on rentre le bon tab
# if(analysis.stats == "bayes-mean.comparison.on.farm")
# 	{
# 	TAB = get.means.comparison(out.model = out, parameter = "alpha", alpha = 0.05, p.adj = "soft.bonf", get.at.least.X.groups = 2, by = 0.005)
# 	TAB = TAB[grep(titre, names(TAB))]
# 	}
# 
# if((analysis.stats == "S-SandR" | analysis.stats == "SandR-SandR") & data.display == "p:y")
# 	{
# 	Mpvalue = out[["Liste_Mpvalue"]][[titre]]
# 	
# 	test = unique(donnees$sl_statut)
# 	type = grep("R", test)
# 	if(length(type) == 0 ) { type = "S" } else { type = "SandR" }
# 		
# 	donnees$toto = paste(donnees[,"p:y"], donnees[,"g"], sep=",")
# 		
# 	liste_toto = unique(donnees$exp)
# 	pvalue.S = pvalue.R = NULL
# 	for(i in liste_toto)
# 		{
# 		l = which(donnees$exp == i)
# 		d = donnees[l,]
# 			
# 		# pour S : t.test
# 		dbouq = d[grep("bouquetS", d[,"sl_statut"]), variable]
# 		dvrac = d[grep("vracS", d[,"sl_statut"]), variable]
# 		if(length(dbouq) > 1 & length(dvrac) > 1) { pvalue = t.test(dbouq, dvrac)$p.value } else { pvalue = NULL }
# 		pvalue.S = c(pvalue.S, get.stars(pvalue))
# 		
# 		# pour R : bayes ou t.test si rien
# 		if(type == "SandR")
# 			{
# 			if(!is.null(Mpvalue)) # est ce que ça sert à qqch ?!? Si FS, ya qqch ?!?
# 				{	
# 				toget = d[grep("R", d[,"sl_statut"]),"toto"]
# 				a = paste("alpha[", toget, "]", sep="")			
# 				pvalue.R = c(pvalue.R, get.stars(max(Mpvalue[a,a])))
# 				} else {
# 						dbouq = d[grep("bouquetR", d[,"sl_statut"]), variable]
# 						dvrac = d[grep("vracR", d[,"sl_statut"]), variable]
# 						if(length(dbouq) > 1 & length(dvrac) > 1) { pvalue = t.test(dbouq, dvrac)$p.value } else { pvalue = NULL }
# 						pvalue.R = c(pvalue.R, get.stars(pvalue))
# 						}
# 			} else { pvalue.R = c(pvalue.R, NA) }
# 		}
# 		
# 	means = tapply(donnees[,variable], donnees$exp, mean, na.rm = TRUE)
# 	max  = tapply(donnees[,variable], donnees$exp, max, na.rm = TRUE)
# 	tab = cbind.data.frame(label = names(means), means = means, max = max, group.S = pvalue.S, group.R = pvalue.R)	
# 	TAB = list(tab); names(TAB) = titre
# 	}
# 
# if(analysis.stats == "FWH-gxe" & x.axis == "g" & in.col == "NONE" & data.display == "NONE")
# 	{
# 	Mpvalue = out[["Mpvalue_alpha"]]
# 	if(!is.null(Mpvalue))
# 		{
# 		MCMC = out[["FW.mcmc"]]
# 		MCMC = MCMC[,grep("alpha\\[", colnames(MCMC))]
# 		tab = get.mean.comparisons(Mpvalue, MCMC, alpha = typeI.error)$Comparaison
# 		tab[,"label"] = sub("alpha\\[", "", tab[,"label"]); tab[,"label"] = sub("]", "", tab[,"label"])
# 		} else { cat("No Mpvalue_alpha, check that comp_alpha = TRUE in finlay.wilkinson.bayes")}
# 	}
# 	
# if(analysis.stats == "FWH-gxe" & x.axis == "p" & is.null(in.col) & is.null(data.display))
# 	{
# 	Mpvalue = out[["Mpvalue_theta"]]
# 	if(!is.null(Mpvalue))
# 		{
# 		MCMC = out[["MCMC"]][,grep("theta\\[", colnames(MCMC))]
# 		tab = get.mean.comparisons(Mpvalue, MCMC, alpha = typeI.error)$Comparaison
# 		tab[,"label"] = sub("theta\\[", "", tab[,"label"]); tab[,"label"] = sub("]", "", tab[,"label"])
# 		} else { cat("No Mpvalue_theta, check that comp_theta = TRUE in finlay.wilkinson.bayes")}
# 	}
# 	
# return(TAB)
# }
# 
# 
# 
# # list.data issu de reshape.data...
# 
# l = unlist(list.data); l = l[grep("truc", names(l))]; l = paste(l[grep("groups", names(l))], collapse = "")
# gp_letters = unique(unlist(strsplit(l, "")))
# 
# #si on prend que means.comparison ou data, il n'y a plus de pb ici
# l = unlist(list.data); l = l[grep("truc", names(l))]; l = as.numeric(l[grep("means", names(l))])
# if(is.null(tab.groups)){ ymax = MAX; ymin = MIN} else { ymax = max(l); ymin = min(l) }
# 
# 
# 
# 
# 		
# 
# graph get significant groups à partir de Mpvalue (à faire si le temps pas urgent, commet pour grpah comp moy avec la pvalue en couleur, cf les deux distributions qui se chevauchent)
# graph_test=FALSE
# if(graph_test)
# {
#   # On regarde si plus on avance et plus ça décroit
#   # Plus on avance et plus la moyenne augmente et plus pvalue entre les deux est petite
#   par(mar=c(10,2,2,13),xpd=TRUE)
#   C = as.data.frame(Mpvalue)
#   
#   # On met que des 0.5 sous la diagonale
#   for (j in 1:ncol(C)) {for (i in j:nrow(C)) {C[i,j]=0.5}} 
#   
#   plot(as.numeric(C[1,]),type="l",col=1,ylim=c(0,0.53),xaxt="n",xlab="",ylab="")
#   for (i in 2:nrow(C)) {lines(as.numeric(C[i,]),col=i)}
#   legend(1.1*ncol(C),0.5,legend=c("value for i =",colnames(C)),col=c(-1,seq(1,ncol(C),1)),lty=c(-1,rep(1,ncol(C))))
#   
#   for(i in 1: ncol(C)){mtext(colnames(C)[i],1,line=1,at = i,las=3)}
#   
#   title("Pr{d >= 0 | y} ")
#   
#   par(xpd=FALSE)
#   abline(h=alpha,lwd=2,lty=2,col="red")
#   legend("topleft",bty="n",legend="typeI = alpha/nbr_pop",col="red",lty=2,lwd=2)
#   
#   
#   par(mar=c(10,5,5,5))
# }
