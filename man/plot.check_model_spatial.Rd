% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.check_model_spatial.R
\name{plot.check_model_spatial}
\alias{plot.check_model_spatial}
\title{Get ggplot to visualize output from \code{\link{check_model.fit_model_spatial}}}
\usage{
\method{plot}{check_model_spatial}(x, nb_parameters_per_plot = 8, ...)
}
\arguments{
\item{x}{Output from \code{\link{check_model.fit_model_spatial}}}

\item{nb_parameters_per_plot}{number of parameter per plot to display}

\item{...}{further arguments passed to or from other methods}
}
\value{
\itemize{
 \item residuals
 \itemize{
  \item histogram : histogram with the distribution of the residuals
  \item qqplot
  \item points
  }
 \item variability_repartition : pie with repartition of SumSq for each factor
 }
}
\description{
\code{plot.check_model_spatial} returns ggplot to visualize outputs from \code{\link{check_model.fit_model_spatial}}
}
\details{
S3 method.
See example in the book: https://priviere.github.io/PPBstats_book/family-1.html#spatial-analysis
}
\seealso{
\code{\link{check_model.fit_model_spatial}}
}
\author{
Pierre Riviere
}
