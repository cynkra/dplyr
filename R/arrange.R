#' Arrange rows by column values
#'
#' @description
#' `arrange()` orders the rows of a data frame by the values of selected
#' columns.
#'
#' Unlike other dplyr verbs, `arrange()` largely ignores grouping; you
#' need to explicitly mention grouping variables (or use  `.by_group = TRUE`)
#' in order to group by them, and functions of variables are evaluated
#' once per data frame, not once per group.
#'
#' @details
#' ## Locales
#' The sort order for character vectors will depend on the collating sequence
#' of the locale in use: see [locales()].
#'
#' ## Missing values
#' Unlike base sorting with `sort()`, `NA` are:
#' * always sorted to the end for local data, even when wrapped with `desc()`.
#' * treated differently for remote data, depending on the backend.
#'
#' @return
#' An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * All rows appear in the output, but (usually) in a different place.
#' * Columns are not modified.
#' * Groups are not modified.
#' * Data frame attributes are preserved.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("arrange")}.
#' @export
#' @param .data A data frame, data frame extension (e.g. a tibble), or a
#'   lazy data frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for
#'   more details.
#' @param ... <[`data-masking`][dplyr_data_masking]> Variables, or functions of
#'   variables. Use [desc()] to sort a variable in descending order.
#' @param .by_group If `TRUE`, will sort first by grouping variable. Applies to
#'   grouped data frames only.
#' @family single table verbs
#' @examples
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, desc(disp))
#'
#' # grouped arrange ignores groups
#' by_cyl <- mtcars %>% group_by(cyl)
#' by_cyl %>% arrange(desc(wt))
#' # Unless you specifically ask:
#' by_cyl %>% arrange(desc(wt), .by_group = TRUE)
#'
#' # use embracing when wrapping in a function;
#' # see ?dplyr_data_masking for more details
#' tidy_eval_arrange <- function(.data, var) {
#'   .data %>%
#'     arrange({{ var }})
#' }
#' tidy_eval_arrange(mtcars, mpg)
#'
#' # use across() access select()-style semantics
#' iris %>% arrange(across(starts_with("Sepal")))
#' iris %>% arrange(across(starts_with("Sepal"), desc))
arrange <- function(.data, ..., .by_group = FALSE) {
  UseMethod("arrange")
}

#' @export
arrange.data.frame <- function(.data, ..., .by_group = FALSE) {
  dots <- enquos(...)

  if (.by_group) {
    dots <- c(quos(!!!groups(.data)), dots)
  }

  loc <- arrange_rows(.data, dots)
  dplyr_row_slice(.data, loc)
}

# Helpers -----------------------------------------------------------------

arrange_rows <- function(.data, dots, error_call = caller_env()) {
  error_call <- dplyr_error_call(error_call)

  if (length(dots) == 0L) {
    out <- seq_len(nrow(.data))
    return(out)
  }

  directions <- map_chr(dots, function(quosure) {
    if(quo_is_call(quosure, "desc")) "desc" else "asc"
  })

  quosures <- map(dots, function(quosure) {
    if (quo_is_call(quosure, "desc", ns = c("", "dplyr"))) {
      expr <- quo_get_expr(quosure)
      if (!has_length(expr, 2L)) {
        abort("`desc()` must be called with exactly one argument.", call = error_call)
      }

      quosure <- new_quosure(node_cadr(expr), quo_get_env(quosure))
    }
    quosure
  })
  # give the quosures arbitrary names so that
  # data has the right number of columns below after transmute()
  names(quosures) <- paste0("^^--arrange_quosure_", seq_along(quosures))

  # TODO: not quite that because when the quosure is some expression
  #       it should be evaluated by groups.
  #       for now this abuses transmute so that we get just one
  #       column per quosure
  #
  #       revisit when we have something like mutate_one() to
  #       evaluate one quosure in the data mask
  data <- withCallingHandlers({
    transmute(new_data_frame(.data), !!!quosures)
  }, error = function(cnd) {

    if (inherits(cnd, "dplyr:::mutate_error")) {
      # reverse the name mangling
      bullets <- gsub("^^--arrange_quosure_", "..", cnd$bullets, fixed = TRUE)
      # only name bullets that aren't already named
      names <- names2(bullets)
      names[names == ""] <- "x"
      bullets <- set_names(bullets, names)

      # skip the parent as this has reworked the bullets
      # and this would be confusing to have them
      parent <- cnd$parent
    } else {
      parent <- cnd
      bullets <- c()
    }

    bullets <- c(
      "Problem with the implicit `transmute()` step. ",
      bullets
    )
    abort(bullets, call = error_call, parent = parent)

  })

  # we can't just use vec_compare_proxy(data) because we need to apply
  # direction for each column, so we get a list of proxies instead
  # and then mimic vctrs:::order_proxy
  #
  # should really be map2(quosures, directions, ...)
  proxies <- map2(data, directions, function(column, direction) {
    proxy <- vec_proxy_order(column)
    desc <- identical(direction, "desc")
    if (is.data.frame(proxy)) {
      proxy <- order(vec_order(proxy,
        direction = direction,
        na_value = if(desc) "smallest" else "largest"
      ))
    } else if(desc) {
      proxy <- desc(proxy)
    }
    proxy
  })

  exec("order", !!!unname(proxies), decreasing = FALSE, na.last = TRUE)
}
