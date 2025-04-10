% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rows.R
\name{rows}
\alias{rows}
\alias{rows_insert}
\alias{rows_update}
\alias{rows_patch}
\alias{rows_upsert}
\alias{rows_delete}
\title{Manipulate individual rows}
\usage{
rows_insert(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE)

rows_update(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE)

rows_patch(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE)

rows_upsert(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE)

rows_delete(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE)
}
\arguments{
\item{x, y}{A pair of data frames or data frame extensions (e.g. a tibble).
\code{y} must have the same columns of \code{x} or a subset.}

\item{by}{An unnamed character vector giving the key columns. The key
values must uniquely identify each row (i.e. each combination of key
values occurs at most once), and the key columns must exist in both \code{x}
and \code{y}.

By default, we use the first column in \code{y}, since the first column is
a reasonable place to put an identifier variable.}

\item{...}{Other parameters passed onto methods.}

\item{copy}{If \code{x} and \code{y} are not from the same data source,
and \code{copy} is \code{TRUE}, then \code{y} will be copied into the
same src as \code{x}.  This allows you to join tables across srcs, but
it is a potentially expensive operation so you must opt into it.}

\item{in_place}{Should \code{x} be modified in place? This argument is only
relevant for mutable backends (e.g. databases, data.tables).

When \code{TRUE}, a modified version of \code{x} is returned invisibly;
when \code{FALSE}, a new object representing the resulting changes is returned.}
}
\value{
An object of the same type as \code{x}. The order of the rows and columns of \code{x}
is preserved as much as possible. The output has the following properties:
\itemize{
\item \code{rows_update()} preserves rows as is; \code{rows_insert()} and \code{rows_upsert()}
return all existing rows and potentially new rows; \code{rows_delete()} returns
a subset of the rows.
\item Columns are not added, removed, or relocated, though the data may be updated.
\item Groups are taken from \code{x}.
\item Data frame attributes are taken from \code{x}.
}

If \code{in_place = TRUE}, the result will be returned invisibly.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

These functions provide a framework for modifying rows in a table using
a second table of data. The two tables are matched \code{by} a set of key
variables whose values must uniquely identify each row. The functions are
inspired by SQL's \code{INSERT}, \code{UPDATE}, and \code{DELETE}, and can optionally
modify \code{in_place} for selected backends.
\itemize{
\item \code{rows_insert()} adds new rows (like \code{INSERT}); key values in \code{y} must
not occur in \code{x}.
\item \code{rows_update()} modifies existing rows (like \code{UPDATE}); key values in
\code{y} must occur in \code{x}.
\item \code{rows_patch()} works like \code{rows_update()} but only overwrites \code{NA} values.
\item \code{rows_upsert()} inserts or updates depending on whether or not the
key value in \code{y} already exists in \code{x}.
\item \code{rows_delete()} deletes rows (like \code{DELETE}); key values in \code{y} must
exist in \code{x}.
}
}
\examples{
data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)
data

# Insert
rows_insert(data, tibble(a = 4, b = "z"))
try(rows_insert(data, tibble(a = 3, b = "z")))

# Update
rows_update(data, tibble(a = 2:3, b = "z"))
rows_update(data, tibble(b = "z", a = 2:3), by = "a")

# Variants: patch and upsert
rows_patch(data, tibble(a = 2:3, b = "z"))
rows_upsert(data, tibble(a = 2:4, b = "z"))

# Delete and truncate
rows_delete(data, tibble(a = 2:3))
rows_delete(data, tibble(a = 2:3, b = "b"))
try(rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b")))
}
