# 0. help -----------------------------------------------------------------
#' Select parts of models results from PPBstats::analyse.outputs (eigher parameters comparisons or MCMC).
#' 
#' @param res_model output from \code{model_1}, \code{model_2} or \code{model_varintra} function
#' 
#' @param data output from the \code{shinemas2R::get.data} function
#' 
#' @param person the person wanted
#' 
#' @param type_result the results wanted : parameters comparison ("comparison") or MCMC ("MCMC")
#
#' @param variable the variable
#' 
#' @param model the model from which the results were obtained : model_1, model_2, model_variance_intra
#
#' @param param The parameter wanted ("mu","beta","sigma" if model_1 is used, "alpha","beta","theta" if model_2 is used, "sigma" if model_variance_intra model is used)
#' 
#' @param year the year wanted
#' 
#' @return the model results for the germplasm, environment and year wanted
#' 
#' @author Gaelle Van Frank
#' 
#' @seealso \code{\link{PPBstats::analyse.outputs}}, \code{\link{shinemas2R::get.data}}
#' 
#' 
get_result_model = function(res_model, data, person = NULL,  type_result = "comparison", variable, model, param = NULL, year = NULL) 
{


# 1. Check parameters -------------
	if ( type_result %in% c("comparison","MCMC") == FALSE ) {stop("Type_result must be comparison or MCMC." )}
	if ( type_result == "comparison" & is.null(param) ) {stop("If type_result == comparison then param must be mu, beta, sigma if model 1 is used, alpha, beta, theta if model 2 is used, or  sigma if variance_intra model is used." )}
  if (type_result == "comparison" ){
    if (model == "model_1"){
      if (!(param %in% c("mu","beta","sigma"))){stop("If type_result == comparison and model == model_1 then param must be mu, beta or sigma")}
      if (!(param %in% unlist(lapply(names(res_model[[variable]]$comp.par), function(x){return(strsplit(x,"[.]")[[1]][2])})))){stop(paste(param," is not in the data",sep=""))}
    }
    if (model == "model_2"){
      if (!(param %in% c("alpha","beta","theta"))){stop("If type_result == comparison and model == model_2 then param must be alpha, beta or theta")}
      if (!(param %in% unlist(lapply(names(res_model[[variable]]$comp.par), function(x){return(strsplit(x,"[.]")[[1]][2])})))){stop(paste(param," is not in the data",sep=""))}
    }
    if (model == "model_variance_intra"){
      if (!(param %in% "sigma")){stop("If type_result == comparison and model == model_variance_intra then param must be sigma")}
      if (!(param %in% unlist(lapply(names(res_model[[variable]]$comp.par), function(x){return(strsplit(x,"[.]")[[1]][2])})))){stop(paste(param," is not in the data",sep=""))}
    }
  }
  #	if ( "comp.mu" %in% names(res_model[[variable]]) ) {modele = 1}
	if(is.null(data) & is.null(person) | is.null(year)){stop("If there is no data provided, person or year must not be NULL")}
  
  if(!is.null(data)){
    # 2. Get ID ------------------
    if ( "data" %in% names(data) == TRUE ) {data = data$data}
    
    noms=unique(data$son)
    noms = unique(gsub("^([^_]*_[^_]*_[^_]*)_.*$", "\\1", noms))
    
    germplasm = unlist(lapply(as.character(noms),function(x){strsplit(x,"_")[[1]][1]}))
    env = unlist(lapply(as.character(noms),function(x){strsplit(x,"_")[[1]][2]}))
    block = data$block
    if(is.null(year)){year = unlist(lapply(as.character(noms),function(x){return(strsplit(x,"_")[[1]][3])}))}
    
    if (type_result == "comparison") {	
      comp.param = paste("comp",param,sep=".")
      comp = res_model[[variable]]$comp.par[[comp.param]]$data_mean_comparisons 
      if(param != "sigma") {comp = comp[grep(paste(paste(env,year,sep=":"),collapse="|"),names(comp))]}
      COMP = NULL
      for (i in 1:length(comp)){
        COMP=rbind(COMP,comp[[i]]$mean.comparisons)
      }
      COMP = unique(COMP)
    }
    
    if (model == "model_1"){
      if (param == "mu") {	ID = as.data.frame(cbind(as.character(noms),paste(param,"[",paste(germplasm,paste(env,year,sep=":"), sep=","),"]",sep=""))) }
      if (param == "beta") { ID = unique(as.data.frame(cbind(paste("[",paste(paste(env,year,sep=":"), block, sep=","),"]",sep=""),paste(param,"[",paste(paste(env,year,sep=":"), block, sep=","),"]",sep="")))) }
      if (param == "sigma") {	ID = unique(as.data.frame(cbind(paste(param,"[",paste(env,year,sep=":"),"]",sep=""),paste(param,"[",paste(env,year,sep=":"),"]",sep="")))) }
    }
    
    if (model == "model_2"){
      if (param == "alpha") {	ID = as.data.frame(cbind(as.character(noms),paste(param,"[",germplasm,"]",sep=""))) }
      if (param == "beta") { ID = as.data.frame(cbind(as.character(noms),paste(param,"[",germplasm,"]",sep="")))  }
      if (param == "theta") {	ID = unique(as.data.frame(cbind(paste(param,"[",paste(env,year,sep=":"),"]",sep=""),paste(param,"[",paste(env,year,sep=":"),"]",sep="")))) }
    }
    
    if (model == "model_varintra"){
      if (param == "sigma") {		ID = as.data.frame(cbind(as.character(noms),paste(param,"[",paste(germplasm,paste(env,year,sep=":"), sep=","),"]",sep=""))) }
    }
    
    if (!is.null(ID)) { colnames(ID) = c("ID","parameter")}
    
    
    # 3. Get model results -------------
    if (type_result == "comparison") {	
      D=merge(COMP,ID, by="parameter")
      D$entry = unlist(lapply(as.character(D$ID), function(x){strsplit(x,"_")[[1]][1]}))
      D$environment = paste(unlist(lapply(as.character(D$ID), function(x){strsplit(x,"_")[[1]][2]})),unlist(lapply(as.character(D$ID), function(x){strsplit(x,"_")[[1]][3]})),sep=":")
      D$location = unlist(lapply(as.character(D$ID), function(x){strsplit(x,"_")[[1]][2]}))
      D$year = unlist(lapply(as.character(D$ID), function(x){strsplit(x,"_")[[1]][3]}))
    }
    if (type_result == "MCMC") {	
      MCMC = res_model[[variable]]$model.outputs$MCMC
      D = MCMC[,colnames(MCMC) %in% ID$parameter]
      if(class(D)=="numeric"){D=as.data.frame(D) ; names(D) = unique(ID[ID$parameter %in% colnames(MCMC),"parameter"])}
      #		colnames(D) = ID[ID$parameter %in% colnames(MCMC),1]
      if (is.null(ncol(D)) & !is.null(D)) { D = as.data.frame(matrix(D,ncol=1))} #; colnames(D) = ID[ID %in% colnames(MCMC)]}
    }
    
    D=unique(D)
  }else{ # Get all data from this person and/or this year
    if (type_result == "comparison") {	
      comp.param <- paste("comp",param,sep=".")
      comp <- res_model[[variable]]$comp.par[[comp.param]]$data_mean_comparisons 
      if(!is.null(person)){comp <- comp[grep(person, names(comp))]}
      if(!is.null(year)){comp <- comp[grep(year, names(comp))]}
      COMP <- NULL
      for (i in 1:length(comp)){
        COMP=rbind(COMP,comp[[i]]$mean.comparisons)
      }
      COMP = unique(COMP)
    }
    
    if (type_result == "MCMC") {	
      MCMC = res_model[[variable]]$model.outputs$MCMC
      if(!is.null(person)){MCMC <- MCMC[,grep(person, colnames(MCMC))]}
      if(!is.null(year)){MCMC <- MCMC[,grep(year, colnames(MCMC))]}
      D <- MCMC
      if(class(D)=="numeric"){D <- as.data.frame(D)}
      #		colnames(D) = ID[ID$parameter %in% colnames(MCMC),1]
      if (is.null(ncol(D)) & !is.null(D)) { D = as.data.frame(matrix(D,ncol=1))} #; colnames(D) = ID[ID %in% colnames(MCMC)]}
    }
  }

return(D)
}
