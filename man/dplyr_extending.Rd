% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{dplyr_extending}
\alias{dplyr_extending}
\alias{dplyr_row_slice}
\alias{dplyr_col_modify}
\alias{dplyr_reconstruct}
\title{Extending dplyr with new data frame subclasses}
\usage{
dplyr_row_slice(data, i, ...)

dplyr_col_modify(data, cols)

dplyr_reconstruct(data, template)
}
\arguments{
\item{data}{A tibble. We use tibbles because they avoid some inconsistent
subset-assignment use cases}

\item{i}{A numeric or logical vector that indexes the rows of \code{.data}.}

\item{cols}{A named list used modify columns. A \code{NULL} value should remove
an existing column.}

\item{template}{Template to use for restoring attributes}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

These three functions, along with \verb{names<-} and 1d numeric \code{[}
(i.e. \code{x[loc]}) methods, provide a minimal interface for extending dplyr
to work with new data frame subclasses. This means that for simple cases
you should only need to provide a couple of methods, rather than a method
for every dplyr verb.

These functions are a stop-gap measure until we figure out how to solve
the problem more generally, but it's likely that any code you write to
implement them will find a home in what comes next.
}
\section{Basic advice}{
This section gives you basic advice if you want to extend dplyr to work with
your custom data frame subclass, and you want the dplyr methods to behave
in basically the same way.
\itemize{
\item If you have data frame attributes that don't depend on the rows or columns
(and should unconditionally be preserved), you don't need to do anything.
\item If you have \strong{scalar} attributes that depend on \strong{rows}, implement a
\code{dplyr_reconstruct()} method. Your method should recompute the attribute
depending on rows now present.
\item If you have \strong{scalar} attributes that depend on \strong{columns}, implement a
\code{dplyr_reconstruct()} method and a 1d \code{[} method. For example, if your
class requires that certain columns be present, your method should return
a data.frame or tibble when those columns are removed.
\item If your attributes are \strong{vectorised} over \strong{rows}, implement a
\code{dplyr_row_slice()} method. This gives you access to \code{i} so you can
modify the row attribute accordingly. You'll also need to think carefully
about how to recompute the attribute in \code{dplyr_reconstruct()}, and
you will need to carefully verify the behaviour of each verb, and provide
additional methods as needed.
\item If your attributes that are \strong{vectorised} over \strong{columns}, implement
\code{dplyr_col_modify()}, 1d \code{[}, and \verb{names<-} methods. All of these methods
know which columns are being modified, so you can update the column
attribute according. You'll also need to think carefully about how to
recompute the attribute in \code{dplyr_reconstruct()}, and you will need to
carefully verify the behaviour of each verb, and provide additional
methods as needed.
}
}

\section{Current usage}{
\itemize{
\item \code{arrange()}, \code{filter()}, \code{slice()}, \code{semi_join()}, and \code{anti_join()}
work by generating a vector of row indices, and then subsetting
with \code{dplyr_row_slice()}.
\item \code{mutate()} generates a list of new column value (using \code{NULL} to indicate
when columns should be deleted), then passes that to \code{dplyr_col_modify()}.
\code{transmute()} does the same then uses 1d \code{[} to select the columns.
\item \code{summarise()} works similarly to \code{mutate()} but the data modified by
\code{dplyr_col_modify()} comes from \code{group_data()}.
\item \code{select()} uses 1d \code{[} to select columns, then \verb{names<-} to rename them.
\code{rename()} just uses \verb{names<-}. \code{relocate()} just uses 1d \code{[}.
\item \code{inner_join()}, \code{left_join()}, \code{right_join()}, and \code{full_join()}
coerces \code{x} to a tibble, modify the rows, then uses \code{dplyr_reconstruct()}
to convert back to the same type as \code{x}.
\item \code{nest_join()} uses \code{dplyr_col_modify()} to cast the key variables to
common type and add the nested-df that \code{y} becomes.
\item \code{distinct()} does a \code{mutate()} if any expressions are present, then
uses 1d \code{[} to select variables to keep, then \code{dplyr_row_slice()} to
select distinct rows.
}

Note that \code{group_by()} and \code{ungroup()} don't use any these generics and
you'll need to provide methods directly.
}

\keyword{internal}
