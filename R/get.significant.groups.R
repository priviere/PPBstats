#' Get significant groups of means that are significantly different from each others
#'  for a set of parameters based on MCMC outputs
#'
#' @description
#' \code{get.significant.groups} gets significant groups of means that are significantly different 
#' from each others for a set of parameters based on MCMC outputs
#' 
#' @param Mpvalue Square matrix with the probability of having a common distribution for each pair of parameter. It comes from \code{\link{comp.parameters}}
#'
#' @param MCMC MCMC. It is a data frame.
#'  
#' @param alpha The level of type one error. 0.05 (5\%) by default
#' 
#' @param p.adj NULL For no adjustement of the pvalue. "soft.bonf" for a soft bonferonni correction to take into account multiple comparisons (alpha / nb of parameters).
#' 
#' 
#' @return The function returns a data frame of four columns and (nb of parameters) rows. Columns are parameters, mean, median and groups 
#' 
#' @author Pierre Riviere
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{mean_comparisons_model_1}}
#' \item \code{\link{mean_comparisons_model_2}}
#' } 
#' 
get.significant.groups = function(
  Mpvalue,
  MCMC,
  alpha = 0.05,
  p.adj = NULL
)
{
  # 1. Error message and update arguments ----------
  if( !is.null(p.adj) ) { 
    if( p.adj != "soft.bonf") { stop("p.adj must be NULL or \"soft.bonf\".") } 
    if( p.adj == "soft.bonf") { alpha = alpha / ncol(Mpvalue) }
  }
  
  L=rep(LETTERS, times = 30)
  letters = c(letters, paste(L, rep(c(1:(length(L)/26)), each = 26), sep = ""))

  
  # 2. For each parameter, put in a list the one that are not different at alpha ----------
  M = Mpvalue
  GP = NULL
  
  for (i in 1:nrow(M)) {
    g = rownames(M)[which(M[i,] > alpha)]
    gp = list(c(rownames(M)[i],g))
    names(gp) = rownames(M)[i]
    GP = c(GP,gp)
  }
  
  # 3. Transforme this list to a matrix with letters ----------
  letter = matrix(" ",ncol = length(GP), nrow = nrow(Mpvalue))
  rownames(letter) = rownames(Mpvalue)
  colnames(letter) = colnames(Mpvalue)
  
  for (i in 1:length(GP)) {
    liste_pop = unlist(GP[[i]])
    for (j in 1:length(liste_pop)) {
      letter[liste_pop[j],i] = letters[i]
    }
  }
  
  # 4. Clean the matrix ----------
  
  # 4.a. First, get rid of redondancies ----------
  clean = function(L) {
    go = TRUE
    while(go) {
      toto = NULL
      for (i in ncol(L):2)
      {
        a = which(L[,i] != " ")
        b = which(L[,i-1] != " ")
        
        test = unique(is.element(names(a),names(b)))	
        if(length(test) == 0) { test = c(TRUE, FALSE) }		
        if(length(test) == 1 & test[1]==TRUE) {L[,i] = " "}
        toto = c(toto, "")
        
        if( length(toto) == (ncol(L)-1) ) { go = FALSE }
        # Delete the column where there are no more informations
        J=NULL
        for (j in ncol(L):2) {
          test = unique(L[,j])
          if(length(test) == 1 & test[1] == " ") {J=c(J,j)}
        }
        if(!is.null(J)) {L = L[,-J]}
      }
      if( is.vector(L) ) { go = FALSE }                
    }
    return(L)
  }
  
  L = letter
  
  L = clean(L)
  
  # 4.b. If there is an empty element in the matrix while it should be full  ---------- 
  # (cf problems in probabilities) then we add the letter of the element on its right
  # Be careful to add all the column (and not only the element alone) in order to not lose informations 
  
  if(!is.vector(L)) {
    LL = L
    I = NULL # to store the column with the letter from the right
    for (i in ncol(LL):1) {
      a = which(LL[,i] != " ")
      theorie = seq(a[1], a[length(a)], 1) # if pvelue were ok
      
      if(length(theorie)!=length(a)) {		
        # fuse the two columns
        L[theorie,i] = paste(L[theorie,i], L[theorie, i + 1], sep = "_")
      }
    }
    
    L = clean(L)
    if(is.vector(L)){ L = matrix(L, ncol = 1) }
    
    # 5. Replace the letters by the letters from the alphabetic order ----------
    a = as.vector(L)
    a = unique(unlist(strsplit(a, "_")))
    a[which(a==" ")] = NA
    a = na.omit(a)
    
    letter = letters[seq(1,length(a),1)]
    names(letter) = a
    
    
    LL=L
    
    for (i in 1:nrow(L)) {
      for (j in 1:ncol(L)) {
        b = L[i,j]
        if(b != " ") {
          a = unlist(strsplit(LL[i,j],"_"))
          a[which(a==" ")] = NA
          a = na.omit(a)
          LL[i,j] = paste(letter[a],collapse="")
        }
      }
    }
    L = LL
    
    # 6. Fuse the letters ----------
    A = matrix("", ncol = 1, nrow = nrow(L))
    for (i in 1:nrow(L)) {
      a = unlist(strsplit(L[i,],""))
      b = which(a==" ")
      if(length(b)!=0){
        A[i,1] = unique(paste(unique(a[-b]),collapse=""))} else {A[i,1] = unique(paste(unique(a),collapse=""))}
    }
  } else { A = as.matrix(L) }
  
  
  # 7. Put parameters, mean, median and groups ----------
  means = apply(MCMC, 2, mean, na.rm = TRUE)[colnames(Mpvalue)] # colnames(Mpvalue) to keep the right order
  medianes = apply(MCMC, 2, median, na.rm = TRUE)[colnames(Mpvalue)]
  
  Comparaison = cbind.data.frame("parameter" = names(means), "mean" = means, "median" = medianes, "groups" = as.character(A[,1]))
  Comparaison$parameter = as.factor(as.character(Comparaison$parameter))
  Comparaison$groups = as.character(Comparaison$groups)
  rownames(Comparaison) = NULL
  
  return(Comparaison)
}
