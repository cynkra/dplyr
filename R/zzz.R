.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("COLLECTOR_PATH") != "") {
    dir.create(Sys.getenv("COLLECTOR_PATH"), recursive = TRUE, showWarnings = FALSE)
    collector::set_collector(
      funs = c(
        "add_count", "anti_join", "arrange", "auto_copy", "collect",
        "compute", "count", "cross_join", "distinct", "do", "explain",
        "full_join", "group_by", "group_indices", "group_keys", "group_map",
        "group_modify", "group_nest", "group_size", "group_split", "group_trim",
        "group_vars", "groups", "inner_join", "intersect.data.frame", "left_join",
        "mutate", "n_groups", "nest_by", "nest_join", "pull",
        "reframe", "relocate", "rename", "rename_with", "right_join",
        "rows_append", "rows_delete", "rows_insert", "rows_patch", "rows_update",
        "rows_upsert", "rowwise", "select", "semi_join", "setdiff.data.frame", "setequal.data.frame",
        "slice", "slice_head", "slice_sample", "slice_tail", "summarise",
        "symdiff", "transmute", "ungroup", "union.data.frame", "union_all"
      ),
      path = Sys.getenv("COLLECTOR_PATH")
    )
  }

  ns_dplyr <- ns_env(pkgname)

  op <- options()
  op.dplyr <- list(
    dplyr.show_progress = TRUE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if (any(toset)) options(op.dplyr[toset])

  .Call(dplyr_init_library, ns_dplyr, ns_env("vctrs"), ns_env("rlang"))

  # TODO: For `arrange()`, `group_by()`, `with_order()`, and `nth()` until vctrs
  # changes `vec_order()` to the new ordering algorithm, at which point we
  # should switch from `vec_order_radix()` to `vec_order()` so vctrs can remove
  # it.
  env_bind(
    .env = ns_dplyr,
    vec_order_radix = import_vctrs("vec_order_radix")
  )

  run_on_load()

  invisible()
}

.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("plyr", "attach"), function(...) {
    packageStartupMessage(rule())
    packageStartupMessage(
      "You have loaded plyr after dplyr - this is likely ",
      "to cause problems.\nIf you need functions from both plyr and dplyr, ",
      "please load plyr first, then dplyr:\nlibrary(plyr); library(dplyr)"
    )
    packageStartupMessage(rule())
  })
}

.onDetach <- function(libpath) {
  setHook(packageEvent("plyr", "attach"), NULL, "replace")
}

import_vctrs <- function(name, optional = FALSE) {
  import_from(name, "vctrs", optional = optional)
}
import_from <- function(name, package, optional = FALSE) {
  ns <- getNamespace(package)

  if (!exists(name, mode = "function", envir = ns, inherits = FALSE)) {
    if (optional) {
      return(NULL)
    }
    abort(sprintf("No such '%s' function: `%s()`.", package, name))
  }

  get(name, mode = "function", envir = ns, inherits = FALSE)
}
