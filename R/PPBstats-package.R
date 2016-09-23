# PPBstats ----------
#' PPBstats
#'
#' @name PPBstats
#' @docType package
#' @description
#' The R package \code{PPBstats} analyzes unbalanced trials from decentralized participatory plant breeding programs.
#' 
#' @author Pierre Riviere and Olivier David
#' 
#' @details
#' How to use \code{PPBstats} is explained in the vignette: type vignette("PPBstats").
#' 
NULL


# PPBdata ----------
#' Simulated values of thousand kernel weight (tkw) of bread wheat in a PPB programme
#'
#' @description
#' A dataset containing one variable (tkw) and values for the following factors: germplasm, location, year, and coordinates in the trial (block, X and Y). 
#' The different values of the parameters to create the dataset are also in columns: mu_ij, beta_jk, epsilon_ijk and sigma_j
#'
#' @usage
#' data(PPBdata)
#'
#' @format
#' A data frame with 1574 rows and 11 columns
#'  
#' @details
#' tkw is in grams
#' 
#' @author Pierre Rivière
"PPBdata"
NULL


# PPBdata2 ----------
#' Simulated values for three variables
#'
#' @description
#' A dataset containing three variables (y1, y2 and y3) and values for the following factors: germplasm, location, year and coordinates in the trial (block, X and Y). 
#' The different values of the parameters to create y1 are also in columns: alpha_i-1, beta_i-1, and theta_j-1.
#' 
#' @usage
#' data(PPBdata2)
#'
#' @format
#' A data frame with 2430 rows and 12 columns
#' 
#' @author Pierre Rivière
"PPBdata2"
NULL


# data_version ----------
#' Data with group of entry separeted in different versions
#'
#' @description
#' A dataset containing values for the following factors: year, location, germplasm, group and version. 
#' There are twi versions per group.
#'
#' @usage
#' data(data_version)
#'
#' @format
#' A data frame with 1574 rows and 5 columns
#' 
#' @author Pierre Rivière
"data_version"
NULL
