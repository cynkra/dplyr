% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slice.R
\name{slice}
\alias{slice}
\alias{slice_head}
\alias{slice_tail}
\alias{slice_min}
\alias{slice_max}
\alias{slice_sample}
\title{Subset rows using their positions}
\usage{
slice(.data, ..., .preserve = FALSE)

slice_head(.data, ..., n, prop)

slice_tail(.data, ..., n, prop)

slice_min(.data, order_by, ..., n, prop, with_ties = TRUE)

slice_max(.data, order_by, ..., n, prop, with_ties = TRUE)

slice_sample(.data, ..., n, prop, weight_by = NULL, replace = FALSE)
}
\arguments{
\item{.data}{A data frame, data frame extension (e.g. a tibble), or a
lazy data frame (e.g. from dbplyr or dtplyr). See \emph{Methods}, below, for
more details.}

\item{...}{For \code{slice()}: <\code{\link[=dplyr_data_masking]{data-masking}}> Integer row
values.

Provide either positive values to keep, or negative values to drop.
The values provided must be either all positive or all negative.
Indices beyond the number of rows in the input are silently ignored.

For \code{slice_helpers()}, these arguments are passed on to methods.}

\item{.preserve}{Relevant when the \code{.data} input is grouped.
If \code{.preserve = FALSE} (the default), the grouping structure
is recalculated based on the resulting data, otherwise the grouping is kept as is.}

\item{n, prop}{Provide either \code{n}, the number of rows, or \code{prop}, the
proportion of rows to select. If neither are supplied, \code{n = 1} will be
used.

If a negative value of \code{n} or \code{prop} is provided, the specified number or
proportion of rows will be removed.

If \code{n} is greater than the number of rows in the group (or \code{prop > 1}),
the result will be silently truncated to the group size. If the
\code{prop}ortion of a group size does not yield an integer number of rows, the
absolute value of \code{prop*nrow(.data)} is rounded down.}

\item{order_by}{Variable or function of variables to order by.}

\item{with_ties}{Should ties be kept together? The default, \code{TRUE},
may return more rows than you request. Use \code{FALSE} to ignore ties,
and return the first \code{n} rows.}

\item{weight_by}{Sampling weights. This must evaluate to a vector of
non-negative numbers the same length as the input. Weights are
automatically standardised to sum to 1.}

\item{replace}{Should sampling be performed with (\code{TRUE}) or without
(\code{FALSE}, the default) replacement.}
}
\value{
An object of the same type as \code{.data}. The output has the following
properties:
\itemize{
\item Each row may appear 0, 1, or many times in the output.
\item Columns are not modified.
\item Groups are not modified.
\item Data frame attributes are preserved.
}
}
\description{
\code{slice()} lets you index rows by their (integer) locations. It allows you
to select, remove, and duplicate rows. It is accompanied by a number of
helpers for common use cases:
\itemize{
\item \code{slice_head()} and \code{slice_tail()} select the first or last rows.
\item \code{slice_sample()} randomly selects rows.
\item \code{slice_min()} and \code{slice_max()} select rows with highest or lowest values
of a variable.
}

If \code{.data} is a \link{grouped_df}, the operation will be performed on each group,
so that (e.g.) \code{slice_head(df, n = 5)} will select the first five rows in
each group.
}
\details{
Slice does not work with relational databases because they have no
intrinsic notion of row order. If you want to perform the equivalent
operation, use \code{\link[=filter]{filter()}} and \code{\link[=row_number]{row_number()}}.
}
\section{Methods}{

These function are \strong{generic}s, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

Methods available in currently loaded packages:
\itemize{
\item \code{slice()}: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice")}.
\item \code{slice_head()}: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice_head")}.
\item \code{slice_tail()}: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice_tail")}.
\item \code{slice_min()}: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice_min")}.
\item \code{slice_max()}: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice_max")}.
\item \code{slice_sample()}: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("slice_sample")}.
}
}

\examples{
mtcars \%>\% slice(1L)
# Similar to tail(mtcars, 1):
mtcars \%>\% slice(n())
mtcars \%>\% slice(5:n())
# Rows can be dropped with negative indices:
slice(mtcars, -(1:4))

# First and last rows based on existing order
mtcars \%>\% slice_head(n = 5)
mtcars \%>\% slice_tail(n = 5)

# Rows with minimum and maximum values of a variable
mtcars \%>\% slice_min(mpg, n = 5)
mtcars \%>\% slice_max(mpg, n = 5)

# slice_min() and slice_max() may return more rows than requested
# in the presence of ties. Use with_ties = FALSE to suppress
mtcars \%>\% slice_min(cyl, n = 1)
mtcars \%>\% slice_min(cyl, n = 1, with_ties = FALSE)

# slice_sample() allows you to random select with or without replacement
mtcars \%>\% slice_sample(n = 5)
mtcars \%>\% slice_sample(n = 5, replace = TRUE)

# you can optionally weight by a variable - this code weights by the
# physical weight of the cars, so heavy cars are more likely to get
# selected
mtcars \%>\% slice_sample(weight_by = wt, n = 5)

# Group wise operation ----------------------------------------
df <- tibble(
  group = rep(c("a", "b", "c"), c(1, 2, 4)),
  x = runif(7)
)

# All slice helpers operate per group, silently truncating to the group
# size, so the following code works without error
df \%>\% group_by(group) \%>\% slice_head(n = 2)

# When specifying the proportion of rows to include non-integer sizes
# are rounded down, so group a gets 0 rows
df \%>\% group_by(group) \%>\% slice_head(prop = 0.5)

# Filter equivalents --------------------------------------------
# slice() expressions can often be written to use `filter()` and
# `row_number()`, which can also be translated to SQL. For many databases,
# you'll need to supply an explicit variable to use to compute the row number.
filter(mtcars, row_number() == 1L)
filter(mtcars, row_number() == n())
filter(mtcars, between(row_number(), 5, n()))
}
\seealso{
Other single table verbs: 
\code{\link{arrange}()},
\code{\link{filter}()},
\code{\link{mutate}()},
\code{\link{rename}()},
\code{\link{select}()},
\code{\link{summarise}()}
}
\concept{single table verbs}
