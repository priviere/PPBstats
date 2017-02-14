# PPBstats ----------
#' PPBstats
#'
#' @name PPBstats
#' @docType package
#' @description
#' \code{PPBstats} is an R package for statistical analysis of balanced and unbalanced trials in decentralized participatory plant breeding programmes
#' 
#' @author Pierre Riviere, Gaelle Van Franck and Olivier David
#' 
#' @details
#' How to use \code{PPBstats} is explained in the vignette: type vignette("PPBstats").
#' 
NULL


# data_model_1 ----------
#' Simulated data set for housand kernel weight (tkw) of bread wheat to test model_1 model
#'
#' @description
#' A dataset containing one variable (tkw) and values for the following factors: location, year, germplasm, block, X and Y. 
#' The different values of the parameters to create the dataset are also in columns: mu_ij, beta_jk, epsilon_ijk and sigma_j
#'
#' @usage
#' data(data_model_1)
#'
#' @format
#' A data frame with 1574 rows and 11 columns
#'  
#' @details
#' tkw is in grams
#' 
#' @author Pierre Rivière
"data_model_1"
NULL


# data_model_2 ----------
#' Simulated data set for three variables to test model_2 model
#'
#' @description
#' A dataset containing three variables (y1, y2 and y3) and values for the following factors: location, year, germplasm, block, X and Y. 
#' The different values of the parameters to create y1 are also in columns: alpha_i-1, beta_i-1, and theta_j-1.
#' 
#' @usage
#' data(data_model_2)
#'
#' @format
#' A data frame with 2430 rows and 12 columns
#' 
#' @author Pierre Rivière
"data_model_2"
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


# data_GxE ----------
#' Simulated data set for three variables to test GxE model
#'
#' @description
#' A dataset containing three variables (y1, y2 and y3) and values for the following factors: location, year, germplasm, block, X and Y. 
#'
#' @usage
#' data(data_GxE)
#'
#' @format
#' A data frame with 180 rows and 9 columns
#' 
#' @author Pierre Rivière
"data_GxE"
NULL

