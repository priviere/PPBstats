#' Run Home Away model
#'
#' @description
#' \code{model_home_away} runs home away model
#'
#' @param data The data frame on which the model is run.
#' It should come from \code{\link{format_data_PPBstats.data_agro_HA}}
#'#'
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
#' Pierre Riviere and Gaelle Van Frank and Baptiste Rouger
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
model_home_away <- function(data, variable){
  # 1. Error messages ----------
    if(!is(data, "data_agro_HA")){ stop(substitute(data), " must be formated with type = \"data_agro_HA\", see PPBstats::format_data_PPBstats().") }
    check_data_vec_variables(data, variable)
    colnames(data)[which(colnames(data) == variable)] = "variable"

  # 2. ANOVA ----------

    if(nlevels(data$year) > 1) { # depends on the years available in the data set
        model <- stats::lm(variable ~ location + germplasm + year + version + location:year + version:germplasm + location:year/block + version:germplasm:year, data = data)
    } else {
        model <- stats::lm(variable ~ location + germplasm + version + version:germplasm + location/block, data = data)
    }

    anova_model <- stats::anova(model)
  
   # 3. Return results ----------
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
