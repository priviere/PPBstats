# **PPBstats 0.25 under process**

### update fonctions
- check and format code and roxygen part for each function, DESCRIPTION and NAMESPACE cf #97
- plot_descriptive_data in common_function regarding data_version_HA and data_version_LF
- hedonic workflow, cf #72
    - add PCA + HCPC on juges
        - format_data
        - function in the workflow
        - Rd + book
- format_data_PPBstats.data_network: take into accuont when there are not lat and long columns

### add new functions
- HA_to_LF, cf #25
- create workflow for home away model, cf #25
    - model_home_away
    - check_model.fit_model_home_away
    - plot.check_model_home_away
    - mean_comparisons.check_model_home_away
    - plot.mean_comparisons_model_home_away


### add RData
- data_version_HA, cf #25


### update vignette (inst/bookdown)
- some typo here and there


# **PPBstats 0.24**

### update fonctions
- model_bh_intra_location
    - deal with vec_env_with_no_controls empty
- format_data_PPBstats.data_network : 
    - clean check at the beginning
    - add long and lat for bipart network
    - add format in vertex data for further use with plot.data_network
    - delete optional column alt as it is not needed for map
- format_data_PPBstats.data_agro : 
    - add long and lat in option
- format_data_PPBstats.data_napping :
    - delete code arg
- plot.data_network, cf #67 :
    - implement map with ggmap
    - add pie on map and network
    - add barplot for unipart network on location
    - display in variable edges of the network for unipart sl on barplot
    - display in variable edges of the network for unipart sl on map with pies 
    - display output from unipart location on a map
- plot.data_agro, cf #68 :
    - add plot_type = "map"
    - add r2 on biplot, cf #84
    - add argument data_version
- ggplot_check_model_GxE : add table with % next to pie plot, cf #38
- format_data_PPBstats.data_agro_version
    - add specific class for data_version_SR and data_version_MR
- common_functions
    - common function for check model regarding frequentist analysis, cf #71
    
### add new functions
- create workflow for napping, cf #72
    - plot.data_organo_napping
    - model_napping
    - check_model.fit_model_napping
    - plot.check_model_napping
    - biplot_data
    - plot.biplot_napping
- create workflow for hedonic, cf #72
    - plot.data_organo_hedonic
    - model_hedonic
    - check_model.fit_model_hedonic
    - plot.check_model_hedonic
    - biplot_data
    - plot.biplot_hedonic
    - mean_comparisons.check_model_hedonic 
    - plot.mean_comparisons_model_hedonic
- biplot_data, cf #93
    - biplot_data.check_model_GxE
    - biplot_data.check_model_hedonic
    - biplot_data.check_model_napping

### update vignette (inst/bookdown)
- regarding change with plot.data_network
- regarding change with plot.data_agro
- regarding workflow in organoleptic analysis
- improve text

### update webiste (inst/web_site)
- improve download
- add email list
- improve text

### update RData
- standardize RData, cf #85
    

# **PPBstats 0.23**
### update fonctions
- format_data_PPBstats.R : 
    - when date, add a column with julian day, cf #65
    - add type data_organo_napping and data_organo_hedonic, cf #72
    - format data for data_network, cf #65
        - add code regarding network_split arg
        - better differentiate year, relation_year_start and relation_year_end : cf Rd
    - data_agro_version done
- describe_data.data_agro.R -> plot.data_agro.R : 
    - add raster arg for plot_type, cf #68
    - replace by plot.data_agro.R
    - use plot methods for describe_data cf #79
- debugs several functions regarding tests
- plot.mean_comparisons_model_1.R
    - improve display of score : cf #61
- plot.parameter_groups.R:
    - model 2 ACP cluster : put in blod or color a given ind, cf #76
- design_experiment.R
    - no location in title for IBD and in the data frame, cf #39
- plot.[...].R
    - change arg ggplot.type to plot_type cf #69
- plot.data_agro.R, cf #68
    - debug radar + simplify ggradar function to get simple appearance
    - create plot with dynamic in time coming from $data_julian from format_data_PPBstats
    - add raster arg + update data_GxE
- reshape_data_split_x_axis_in_col.R in common_function
    - debug when NA in row
- mean_comparisons.check_model_GxE.R & mean_comparisons.check_model_spatial.R
    - update code regarding changes in agricolae package
- rename functions related to model (cf #54)
    - model_1 -> model_bh_intra_location
    - model_2 -> model_bh_GxE
    - GxE -> model_GxE
    - spatial -> model_spatial
    - model_variance_intra -> model_bh_variance_intra

    
### add new functions
- napping.R, cf #72
- hedonic.R, cf #72
- PPBstats_interface.R, cf #64
- /inst/shiny_interface/agro/server.R, cf #64
- /inst/shiny_interface/agro/ui.R, cf #64
- format_data_PPBstats.format_network.R, cf #65
- format_data_PPBstats.format_agro.R, cf #65
- format_data_PPBstats.format_agro_version.R, cf #65
- format_data_PPBstats.format_organo_napping.R, cf #65
- format_data_PPBstats.format_organo_hedonic.R, cf #65
- plot.data_network.R, cf #67

### vignette -> inst/bookdown
- update text (cf #76, #65, #67)
- translate Rnw files to Rmd files, format to bookdown, put all files in inst/bookdown (cf #36)
- explain Skewness and Kurtosis test as well as other output from check model for frequentist analysis (cf #36)
- text and decision tree improved by Pierre, Isabelle and Camille
  
### rename and reformat RData
following change of model name:

- data_GxE -> data_model_GxE
- data_model_1 -> data_model_bh_intra_location
- data_model_2 -> data_model_bh_GxE
- data_variance_intra -> data_model_bh_variance_intra


### add new RData
- data_network_unipart_sl.RData
- data_network_unipart_location.RData
- data_network_bipart.RData
- data_version_SR 
- data_version_MR

### web site
- add files in inst/web_site, cf #83

  
# **PPBstats 0.22**
### update functions
- model_1.R : 
     - correct bug model_1 (cf #60)
     - better plot for score regarding model 1 (cf #61)
- workflow regarding spatial analysis : improve and debug
- update workflow regarding variance_intra model (cf #28, #29, #30, #31, #32) but still work to do : cf #62
- plot.parameter_groups.R : split plot ACP cluster (cf #47) + update Rd in plot.PPBstats.R
- describe_data.R : create class

### add new functions
- format_data_PPBstats.R
- describe_data.data_agro.R : huge refactoring, cf #68
- describe_data.data_network.R : empty from now
- multivariate.R

### vignette
- update text (typo + format_data_PPBstats + cf #36, #47, #68)
- add new section on variance_intra + update workflow figure, functions table, analysis families
- update multivariate analysis section
- update contributions
  

# **PPBstats 0.21**
### update functions
- ggplot_mean_comparisons_model_1.R
- ggplot_mean_comparisons_predict_the_past_model_2.R : cf #34
- model_1.R
- fix typo (cf #53)
- rename file .R in order to have same name of files and functions

### update RData
- Add the date in format of data #48

- add new functions that implement spatial analysis (cf #20)
    - spatial.R 
    - check_model.fit_model_spatial.R
    - plot.check_model_spatial.R
    - mean_comparisons.check_model_spatial.R
    - plot.mean_comparisons_model_spatial.R
  
### vignette
- update text, fix typo, update fig (cf #20, #34, #49)
- add section on spatial analysis
- add Rmd regarding contributions
  

# **PPBstats 0.20** 
- substitute get_ggplot() by plot() methods (#21)
- update vignette and cached results


# **PPBstats 0.19** 
### update functions
- add add_stars_version() in ggplot_mean_comparisons_model_1.R (cf #35)
- ok + update vignette (cf #51)

  
# **PPBstats 0.18** 
### update vignette
- cf #36 : add decision tree + update
- reorganise introduction of agro section

# **PPBstats 0.17** 
### add new functions
- model_variance_intra.R
- check_model_model_variance_intra.R
- ggplot_check_model_model_variance_intra.R
- mean_comparisons_model_variance_intra.R 

### update function
- check_model_model_1.R : add location and year for data_env_whose_param_did_not_converge
- ggplot_parameter_groups.R & parameter_groups.R : change kmeans methods for HCPC
- ggplot_mean_comparisons_model_1.R : little debug

### update vignette
- changes in text and exemple regarding functions udaptes


# **PPBstats 0.16** 
### update functions
- predict_the_past_model_2.R : add estimated and predicted MCMC + parameter statuts
- mean_comparisons_predict_the_past_model_2.R : add parameter statuts in mean comparisons outputs

### update vignette
- remind to install JAGS
- add R version
- new sections : network, agronomic, organoleptic and molecular
- add IBD analysis section
  

# **PPBstats 0.15** 
### update functions
- biplot_GxE.R and ggplot_biplot_GxE.R : add interaction matrix
- GxE.R and GxE_build_interaction_matrix.R : model writing

### update vignette
- regarding changes in R code
- spelling 
- contributions and aknowledgement


# **PPBstats 0.14** 
Huge refactoring of the code in several functions for each steps of the analysis

### rename function
- analyse.outputs.R becomes check_model.R
- get.parameter.groups.R becomes parameter.groups.R
- get.mean.comparisons.R becomes mean.comparisons.R
- MC.R becomes model_1.R
- FWH.R becomes model_2.R
- cross.validation.R becomes cross_validation_model_2.R
- predict.the.past.R becomes predict_the_past_model_2.R
- get.ggplot.R becomes get_ggplot.R

### add new functions
- biplot_GxE.R
  
- check_model_model_1.R
- check_model_model_2.R
- check_model_GxE.R
  
- mean_comparisons_GxE.R
- mean_comparisons_model_1.R
- mean_comparisons_model_2.R
- mean_comparisons_predict_the_past_model_2.R
  
- parameter_groups_GxE.R
- parameter_groups_model_2.R
  
- ggplot_biplot_GxE.R
- ggplot_check_model_model_1.R
- ggplot_check_model_model_2.R
- ggplot_check_model_GxE.R
- ggplot_mean_comparisons_GxE.R
- ggplot_parameter_groups.R
- ggplot_cross_validation_model_2.R
- ggplot_predict_the_past_model_2.R
- ggplot_mean_comparisons_predict_the_past_model_2.R

### delete functions
- ggplot_LSDgroup

### vignette
- update regarding changes in R code


# **PPBstats 0.13** 
### add new functions
- describe_data.R
- extra_functions.R
- design_experiment.R
- ggplot_which_won_where.R
- ggplot_mean_vs_stability.R
- ggplot_discrimitiveness_vs_representativeness.R

### rename function
- AMMI.R becomes GxE.R
  
### update functions
- MC.R
- get.ggplot.R: manage nb_parameters_per_plot for ggplot.type == "biplot-alpha-beta"
- get.significant.group.R
- get.PPBstats.data.R

### vignette
- refactor the code
- add new sections


# **PPBstats 0.12** 
- delete all the pdf in the file that are generated by Rnw 
in order to earn space on github

- change the nomenclature of the version


# **PPBstats 0.11.1** 
- update some little bugs regarding tests

### update functions
- get.ggplot.R
     - debug data_version regarding real data set
    

# **PPBstats 0.11.0** 
- update some little bugs regarding tests

### update the vignette
- add example for data_version
- add example for ggplot.type = "biplot-alpha-beta"
  

# **PPBstats 0.10.2** 
### update functions
- get.ggplot.R
     - add data_2 argument
     - add ggplot.type = "biplot" which is possible for model 2


# **PPBstats 0.10.1** 
- update some little bugs regarding tests

### update functions
- get.ggplot.R : add data_version argument
- get.mean.comparisons.R
- predict.the.past.R : take into account when there are no parameters in the MCMC
  
### add new functions
- AMMI.R
- AMMI_called_functions.R

### add new data set
- data_version.RData
  

# **PPBstats 0.10.0** 
### vignette
- update the vignette following previous developments


# **PPBstats 0.9.2** 
- fix little bugs
- add the presence.absence.matrix for the model in FWH, analyse.outputs and predict.the.past
- add model1.data_env_whose_param_did_not_converge in analyse.outputs and get.ggplot
- return MCMC only for parameters that converge
- possible to choose NULL for get.at.least.X.groups
- "cluster" is displayed in the legend for ggplots from get.parameter.groups
- fix huge bug in MC where the mean of the observation were not correctly done


# **PBstats 0.9.1** 
- little changes in vignette and readme
- little changes in the comments of some functions


# **PPBstats 0.9** 
- first version on Github
