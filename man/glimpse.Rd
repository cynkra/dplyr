% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reexport-tibble.r
\name{glimpse}
\alias{glimpse}
\title{Get a glimpse of your data}
\value{
x original x is (invisibly) returned, allowing \code{glimpse()} to be
used within a data pipeline.
}
\description{
\code{glimpse()} is like a transposed version of \code{print()}:
columns run down the page, and data runs across.
This makes it possible to see every column in a data frame.
It's a little like \code{\link[=str]{str()}} applied to a data frame
but it tries to show you as much data as possible.
(And it always shows the underlying data, even when applied
to a remote data source.)

\code{glimpse()} is provided by the pillar package, and re-exported
by dplyr. See \code{\link[pillar:glimpse]{pillar::glimpse()}} for more details.
}
\examples{
glimpse(mtcars)

# Note that original x is (invisibly) returned, allowing `glimpse()` to be
# used within a pipeline.
mtcars \%>\%
  glimpse() \%>\%
  select(1:3)

glimpse(starwars)
}
