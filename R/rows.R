#' Manipulate individual rows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' These functions provide a framework for modifying rows in a table using
#' a second table of data. The two tables are matched `by` a set of key
#' variables whose values must uniquely identify each row. The functions are
#' inspired by SQL's `INSERT`, `UPDATE`, and `DELETE`, and can optionally
#' modify `in_place` for selected backends.
#'
#' * `rows_insert()` adds new rows (like `INSERT`); key values in `y` must
#'    not occur in `x`.
#' * `rows_update()` modifies existing rows (like `UPDATE`); key values in
#'   `y` must occur in `x`.
#' * `rows_patch()` works like `rows_update()` but only overwrites `NA` values.
#' * `rows_upsert()` inserts or updates depending on whether or not the
#'   key value in `y` already exists in `x`.
#' * `rows_delete()` deletes rows (like `DELETE`); key values in `y` must
#'   exist in `x`.
#'
#' @inheritParams left_join
#' @param x,y A pair of data frames or data frame extensions (e.g. a tibble).
#'   `y` must have the same columns of `x` or a subset.
#' @param by An unnamed character vector giving the key columns. The key
#'   values must uniquely identify each row (i.e. each combination of key
#'   values occurs at most once), and the key columns must exist in both `x`
#'   and `y`.
#'
#'   By default, we use the first column in `y`, since the first column is
#'   a reasonable place to put an identifier variable.
#' @param in_place Should `x` be modified in place? This argument is only
#'   relevant for mutable backends (e.g. databases, data.tables).
#'
#'   When `TRUE`, a modified version of `x` is returned invisibly;
#'   when `FALSE`, a new object representing the resulting changes is returned.
#' @returns
#' An object of the same type as `x`. The order of the rows and columns of `x`
#' is preserved as much as possible. The output has the following properties:
#'
#' * `rows_update()` preserves rows as is; `rows_insert()` and `rows_upsert()`
#'   return all existing rows and potentially new rows; `rows_delete()` returns
#'   a subset of the rows.
#' * Columns are not added, removed, or relocated, though the data may be updated.
#' * Groups are taken from `x`.
#' * Data frame attributes are taken from `x`.
#'
#' If `in_place = TRUE`, the result will be returned invisibly.
#' @name rows
#' @examples
#' data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)
#' data
#'
#' # Insert
#' rows_insert(data, tibble(a = 4, b = "z"))
#' try(rows_insert(data, tibble(a = 3, b = "z")))
#'
#' # Update
#' rows_update(data, tibble(a = 2:3, b = "z"))
#' rows_update(data, tibble(b = "z", a = 2:3), by = "a")
#'
#' # Variants: patch and upsert
#' rows_patch(data, tibble(a = 2:3, b = "z"))
#' rows_upsert(data, tibble(a = 2:4, b = "z"))
#'
#' # Delete and truncate
#' rows_delete(data, tibble(a = 2:3))
#' rows_delete(data, tibble(a = 2:3, b = "b"))
#' try(rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b")))
NULL


#' @rdname rows
#' @export
rows_insert <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  lifecycle::signal_stage("experimental", "rows_insert()")
  UseMethod("rows_insert")
}

#' @export
rows_insert.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  check_dots_empty()
  key <- rows_check_key(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  rows_df_in_place(in_place)

  rows_check_key_df(x, key, df_name = "x")
  rows_check_key_df(y, key, df_name = "y")

  idx <- vctrs::vec_match(y[key], x[key])
  bad <- which(!is.na(idx))
  if (has_length(bad)) {
    abort("Attempting to insert duplicate rows.")
  }

  rows_bind(x, y)
}

#' @rdname rows
#' @export
rows_update <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  lifecycle::signal_stage("experimental", "rows_update()")
  UseMethod("rows_update", x)
}

#' @export
rows_update.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  check_dots_empty()
  key <- rows_check_key(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  rows_df_in_place(in_place)

  rows_check_key_df(x, key, df_name = "x")
  rows_check_key_df(y, key, df_name = "y")
  idx <- vctrs::vec_match(y[key], x[key])

  bad <- which(is.na(idx))
  if (has_length(bad)) {
    abort("Attempting to update missing rows.")
  }

  x[idx, names(y)] <- y
  x
}

#' @rdname rows
#' @export
rows_patch <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  lifecycle::signal_stage("experimental", "rows_patch()")
  UseMethod("rows_patch", x)
}

#' @export
rows_patch.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  check_dots_empty()
  key <- rows_check_key(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  rows_df_in_place(in_place)

  rows_check_key_df(x, key, df_name = "x")
  rows_check_key_df(y, key, df_name = "y")
  idx <- vctrs::vec_match(y[key], x[key])

  bad <- which(is.na(idx))
  if (has_length(bad)) {
    abort("Can't patch missing row.")
  }

  new_data <- map2(x[idx, names(y)], y, coalesce)

  x[idx, names(y)] <- new_data
  x
}

#' @rdname rows
#' @export
rows_upsert <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  lifecycle::signal_stage("experimental", "rows_upsert()")
  UseMethod("rows_upsert", x)
}

#' @export
rows_upsert.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  check_dots_empty()
  key <- rows_check_key(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  rows_df_in_place(in_place)

  rows_check_key_df(x, key, df_name = "x")
  rows_check_key_df(y, key, df_name = "y")
  idx <- vctrs::vec_match(y[key], x[key])
  new <- is.na(idx)
  idx_existing <- idx[!new]
  idx_new <- idx[new]

  x[idx_existing, names(y)] <- vec_slice(y, !new)
  rows_bind(x, vec_slice(y, new))
}

#' @rdname rows
#' @export
rows_delete <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  lifecycle::signal_stage("experimental", "rows_delete()")
  UseMethod("rows_delete", x)
}

#' @export
rows_delete.data.frame <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
  check_dots_empty()
  key <- rows_check_key(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  rows_df_in_place(in_place)

  rows_check_key_df(x, key, df_name = "x")
  rows_check_key_df(y, key, df_name = "y")

  extra_cols <- setdiff(names(y), key)
  if (has_length(extra_cols)) {
    bullets <- glue("Ignoring extra columns: ", commas(tick_if_needed(extra_cols)))
    inform(bullets, class = c("dplyr_message_delete_extra_cols", "dplyr_message"))
  }

  idx <- vctrs::vec_match(y[key], x[key])

  bad <- which(is.na(idx))
  if (has_length(bad)) {
    abort("Can't delete missing row.")
  }

  dplyr_row_slice(x, -idx)
}

# helpers -----------------------------------------------------------------

rows_check_key <- function(by, x, y, error_call = caller_env()) {
  if (!length(y)) {
    abort("`y` must have at least 1 column so a key variable can be set.", call = error_call)
  }

  if (is.null(by)) {
    by <- names(y)[[1]]
    msg <- glue("Matching, by = \"{by}\"")
    inform(msg, class = c("dplyr_message_matching_by", "dplyr_message"))
  }

  if (!is.character(by) || length(by) == 0) {
    abort("`by` must be a character vector.", call = error_call)
  }
  # is_named(by) checks all(names2(by) != ""), we need any(...)
  if (any(names2(by) != "")) {
    abort("`by` must be unnamed.", call = error_call)
  }

  bad <- setdiff(colnames(y), colnames(x))
  if (has_length(bad)) {
    abort("All columns in `y` must exist in `x`.", call = error_call)
  }

  by
}

rows_check_key_df <- function(df, by, df_name, error_call = caller_env()) {
  y_miss <- setdiff(by, colnames(df))
  if (length(y_miss) > 0) {
    msg <- glue("All `by` columns must exist in `{df_name}`.")
    abort(msg, call = error_call)
  }
  if (vctrs::vec_duplicate_any(df[by])) {
    msg <- glue("`{df_name}` key values must be unique.")
    abort(msg, call = error_call)
  }
}

rows_df_in_place <- function(in_place, error_call = caller_env()) {
  if (is_true(in_place)) {
    msg <- "Data frames only support `in_place = FALSE`."
    abort(msg, call = error_call)
  }
}

rows_bind <- function(x, y) {
  dplyr_reconstruct(vctrs::vec_rbind(x, y), x)
}
