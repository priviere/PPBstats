% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean_comparisons.check_model_local_foreign.R
\name{mean_comparisons.check_model_local_foreign}
\alias{mean_comparisons.check_model_local_foreign}
\title{Get mean comparisons from \code{\link{check_model.fit_model_local_foreign}} object}
\usage{
\method{mean_comparisons}{check_model_local_foreign}(x, alpha = 0.05,
  p.adj = "none", ...)
}
\arguments{
\item{x}{outputs from \code{\link{check_model.fit_model_local_foreign}}}

\item{alpha}{level of type one error. 0.05 (5\%) by default}

\item{p.adj}{NULL for no adjustement of the type one error.
p.adj can be "tukey".}

\item{...}{further arguments passed to or from other methods}
}
\value{
A list of four elements :
  \itemize{
   \item info : a list with variable and local_foreign analysis
   \item data_ggplot_LSDbarplot_version:location
   \item data_ggplot_LSDbarplot_germplasm
   \item data_ggplot_LSDbarplot_location
   \item data_ggplot_LSDbarplot_year
  }
}
\description{
\code{mean_comparisons} performs mean comparisons from object coming from \code{\link{check_model.fit_model_local_foreign}}
}
\details{
S3 method.
Mean comparisons based on LSmeans with the R package emmeans.
See in the book for mo arere details \href{https://priviere.github.io/PPBstats_book/intro-agro.html#section-freq}{here}
}
\seealso{
\itemize{
 \item \code{\link{mean_comparisons}}
 \item \code{\link{plot.PPBstats}}
 \item \code{\link{plot.mean_comparisons_model_local_foreign}}
}
}
\author{
Pierre Riviere and Baptiste Rouger
}
