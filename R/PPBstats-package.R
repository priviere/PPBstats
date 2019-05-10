# PPBstats ----------
#' PPBstats
#'
#' @name PPBstats
#' @docType package
#' @description
#' \code{PPBstats} is an R package to perform analysis found within PPB programmes
#' 
#' @author Pierre Riviere, Gaelle Van Franck, Olivier David and Facundo Muñoz
#' 
#' @details
#' A website dedicated to PPBstats and a exhaustive tutorial 
#' to collaborate and use the package can be found here : 
#' https://priviere.github.io/PPBstats_web_site
#' 
#' To cite PPBstats, type citation(\"PPBstats\")
#' 
NULL


# data_model_bh_intra_location ----------
#' Simulated data set for thousand kernel weight (tkw) of bread wheat to test model_bh_intra_location model
#'
#' @description
#' A dataset containing one variable (tkw) and its date (tkw$date) 
#' and values for the following factors: seed_lot, location, year, germplasm, block, X and Y. 
#' The different values of the parameters to create the dataset are also in columns: mu_ij, beta_jk, epsilon_ijk and sigma_j
#'
#' @usage
#' data(data_model_bh_intra_location)
#'
#' @format
#' A data frame with 1574 rows and 13 columns
#'  
#' @details
#' tkw is in grams
#' 
#' @author Pierre Rivière
"data_model_bh_intra_location"
NULL


# data_model_bh_GxE ----------
#' Simulated data set for three variables to test model_bh_GxE model
#'
#' @description
#' A dataset containing three variables (y1, y2 and y3) 
#' with their respective dates (y1$date, y2$date and y3$date)
#' and values for the following factors: seed_lot, location, year, germplasm, block, X and Y. 
#' The different values of the parameters to create y1 are also in columns: alpha_i-1, beta_i-1, and theta_j-1.
#' 
#' @usage
#' data(data_model_bh_GxE)
#'
#' @format
#' A data frame with 2430 rows and 16 columns
#' 
#' @author Pierre Rivière
"data_model_bh_GxE"
NULL


# data_version ----------
#' Data with group of entry separeted in different versions
#'
#' @description
#' A dataset containing values for the following factors: year, location, germplasm, group and version. 
#' There are two versions per group.
#'
#' @usage
#' data(data_version)
#'
#' @format
#' A data frame with 12 rows and 5 columns
#' 
#' @author Pierre Rivière
"data_version"
NULL


# data_version_SR ----------
#' Data to study response to selection based on data_version format
#'
#' @description
#' A dataset containing values for the following factors: year, location, germplasm, group and version.
#' group represents differential selection (S) or reponse to selection (R)
#' version represents bouquet or vrac.
#'
#' @usage
#' data(data_version_SR)
#'
#' @format
#' A data frame with 20 rows and 5 columns
#' 
#' @author Pierre Rivière
"data_version_SR"
NULL


# data_version_HA ----------
#' Data to study home and away
#'
#' @description
#' A dataset containing values for the following factors: year, location, germplasm, group and version.
#' group represents home or away
#' version represents the location where come the germplasm from.
#'
#' @usage
#' data(data_version_HA)
#'
#' @format
#' A data frame with 25 rows and 5 columns
#' 
#' @author Pierre Rivière
"data_version_HA"
NULL


# data_model_GxE ----------
#' Simulated data set for three variables to test GxE model
#'
#' @description
#' A dataset containing five variables (y1, y2, y3, desease, vigor) 
#' with their respective dates for the first three (y1$date, y2$date and y3$date)
#' and values for the following factors: seed_lot, location, year, germplasm, block, X and Y. 
#' As well as vlues of lat and long in order to get map.
#'
#' @usage
#' data(data_model_GxE)
#'
#' @format
#' A data frame with 180 rows and 17 columns
#' 
#' @author Pierre Rivière
"data_model_GxE"
NULL


# data_model_bh_variance_intra ----------
#' Simulated data set for spike weight of bread wheat to test model_variance_intra
#'
#' @description
#' A dataset containing one variable (spike weight) 
#' and values for the following factors: seed_lot, location, year, germplasm, block, X and Y. 
#' The different values of the parameters to create the dataset are also in columns: mu_ijk, sigma_ij and epsilon_ijkl
#'
#' @usage
#' data(data_model_bh_variance_intra)
#'
#' @format
#' A data frame with 1182 rows and 11 columns
#'  
#' @details
#' spike weight is in grams
#' 
#' @author Gaelle Van Frank
"data_model_bh_variance_intra"
NULL


# data_network_unipart_sl ----------
#' Simulated data set for unipart network for seed lots which represents relation between between seed lots
#' 
#' @description
#' A dataset containing four locations, 15 germplasms and 5 years.
#' Relation between seed lots are diffusion, mixture, reproduction and selection.
#' The data set contain the following variables: seed_lot_parent, seed_lot_child, 
#' relation_type, relation_year_start, relation_year_end, 
#' germplasm_parent, location_parent, year_parent, alt_parent, long_parent, lat_parent, 
#' germplasm_child, location_child, year_child, alt_child, long_child, lat_child
#'
#' See the vignette for more information.
#'
#' @usage
#' data(data_network_unipart_sl)
#'
#' @format
#' A data frame with 104 rows and 17 columns
#'  
#' @author Pierre Rivière
"data_network_unipart_sl"
NULL


# data_network_unipart_location ----------
#' Simulated data set for unipart network which represents relation of germplasm diffusion between location
#'
#' @description
#' A dataset containing four locations, 15 germplasms and 5 years.
#' Relation between seed lots concern diffusion.
#' The data set contain the following variables: location_parent, location_child, 
#' relation_year_start, relation_year_end, 
#' germplasm_parent, year_parent, alt_parent, long_parent, lat_parent, 
#' germplasm_child, year_child, alt_child, long_child, lat_child
#'
#' The data set have been generated from data_network_unipart_sl
#'
#' See the vignette for more information.
#'
#' @usage
#' data(data_network_unipart_location)
#'
#' @format
#' A data frame with 22 rows and 14 columns
#'  
#' @author Pierre Rivière
"data_network_unipart_location"
NULL


# data_network_bipart ----------
#' Simulated data set for bipart network which represents "which location has which germplasm which year": 
#'
#' @description
#' A dataset containing 3 locations, 11 germplasms and 5 years.
#' The data set contain the following variables: germplasm, location, year, alt, long, lat
#'
#' The data set have been generated from data_network_unipart_sl with relation diffusion or reproduction
#'
#' See the vignette for more information.
#'
#' @usage
#' data(data_network_bipart)
#'
#' @format
#' A data frame with 73 rows and 14 columns
#'  
#' @author Pierre Rivière
"data_network_bipart"
NULL


# data_hedonic ----------
#' data coming from tomatoes hedonic test:
#'
#' @description
#' A dataset containing 1 location and 6 germplasms.
#' The data set contain the following variables: sample, germplasm, location, juges, note, descriptors, Age, Sexe, 
#' Bio.Non.Bio, Circuit, Departement
#'
#' @usage
#' data(data_hedonic)
#'
#' @format
#' A data frame with 244 rows and 11 columns
#'  
#' @author Pierre Rivière
"data_hedonic"
NULL


# data_napping ----------
#' data coming from tomatoes hedonic test:
#'
#' @description
#' A dataset containing 1 location and 9 germplasms.
#' The data set contain the following variables: juges, X, Y, descriptors, germplasm, location
#'
#' @usage
#' data(data_napping)
#'
#' @format
#' A data frame with 63 rows and 6 columns
#'  
#' @author Pierre Rivière
"data_napping"
NULL


# data_model_spatial ----------
#' Data set for one variable to test spatial model
#'
#' @description
#' A dataset containing one variable y1 and 
#' the following factors: seed_lot, location, year, germplasm, block, X and Y. 
#' y1 refers to real value of interval in day degree of flowering between male and female flower of maize.
#' @usage
#' data(data_model_spatial)
#'
#' @format
#' A data frame with 211 rows and 8 columns
#' 
#' @author Pierre Rivière
"data_model_spatial"
NULL

