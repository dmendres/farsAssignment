% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize the content of multiple FARS data files}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A list of 4-digit year numbers (YYYY) specifying the FARS files to be read.}
}
\value{
This function returns a \code{data.frame} of 12 observations, one each month,
of the number FARS reports in the specified years.
}
\description{
This function produces a summary \code{data.frame} containing the number of reports
by month for the FARS files corresponding to each year in the \code{years} list.
}
\details{
Missing years will be skipped with a warning.

The following functions are imported: \code{dplr::bind_rows, dplr::group_by, dply::summarize}, and \code{tidyr::spread}
}
\examples{

fars_summarize_years(list(2013, 2014, 2015))
# will produce a warning
#fars_summarize_years(list(2013,2016))
# will fail
#not run, will fail: fars_summarize_years(1999)

}
