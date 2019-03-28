#' Run Home Away model
#'
#' @description
#' \code{model_home_away} runs home away model
#'
#' @param data The data frame on which the model is run.
#' It should come from \code{\link{format_data_PPBstats.data_agro}}
#'
#' @param data_version It should come from \code{\link{format_data_PPBstats.data_agro_version}} with home and away in version
#'
#' @param variable variable to analyse
#'
#' @return
#' The function returns a list with three elements :
#' \itemize{
#'  \item info : a list with variable
#'  \item ANOVA a list with two elements :
#'   \itemize{
#'    \item model
#'    \item anova_model
#'   }
#' }
#'
#' @details
#' Find details in the book \href{https://priviere.github.io/PPBstats_book/family-4.html#family-4}{here}.
#' @author
#' Pierre Riviere and Gaelle Van Frank
#'
#' @seealso
#' \itemize{
#'  \item \code{\link{check_model}}
#'  \item \code{\link{check_model.fit_model_home_away}}
#' }
#'
#' @export
#'
#' @import plyr
#' @import dplyr
#' @import stats
#'
model_home_away <- function(data, data_version, variable){
  # 1. Error messages ----------
    if(!is(data, "data_agro")){ stop(substitute(data), " must be formated with type = \"data_agro\", see PPBstats::format_data_PPBstats().") }
    if(!is(data_version, "data_agro_version_HA")){ stop(substitute(data_version), " must be formated with type = \"data_agro_version\" and home away in version, see PPBstats::format_data_PPBstats.data_agro_version") }
    check_data_vec_variables(data, variable)

  # 2. Set up data set ----------
    colnames(data)[which(colnames(data) == variable)] = "variable"
    data$id_azerty = paste(data$location, data$year, data$germplasm, sep = "-")
    data_version$id_azerty = paste(data_version$location, data_version$year, data_version$germplasm, sep = "-")

  # get row where id is present in both data set
    t1 <- is.element(data_version$id_azerty, data$id_azerty)
  # get rid of row where group (location of origin) is not present in the data set
    t2 <- is.element(data_version[t1,]$group, data_version[t1,]$location)
    id_ok <- data_version[t1,]$id_azerty[t2]
    id_not_ok <- data_version[t2,]$id_azerty[!t2]

    if( length(id_not_ok) > 0 ) {
        warning("The following rows are not taken into account in data_version: ", paste(id_not_ok, collape = ", "))
    }
    if( length(id_not_ok) == length(t) ) { stop("There is not match between data_version and data. Not analysis can be done.") }
    data_version <- droplevels(dplyr::filter(data_version, id_azerty %in% id_ok))

    data <- plyr::join(data_version, data, by = "id_azerty")
    data <- data[c("location", "germplasm", "year", "block", "version", "variable")]
    data <- droplevels(na.omit(data))
    data <- data[!duplicated(as.list(data))]

  # 3. ANOVA ----------

    if(nlevels(data$year) > 1) { # depends on the years available in the data set
        model <- stats::lm(variable ~ location + germplasm + year + version + location:year + version:germplasm + location:year/block + version:germplasm:year, data = data)
    } else {
        model <- stats::lm(variable ~ location + germplasm + version + version:germplasm + location/block, data = data)
    }

    anova_model <- stats::anova(model)
  
   # 4. Return results ----------
    out <- list(
        "info" = list("variable" = variable),
        "ANOVA" = list(
            "model" = model,
            "anova_model" = anova_model
        )
    )

    class(out) <- c("PPBstats", "fit_model_home_away")
    return(out)
}
